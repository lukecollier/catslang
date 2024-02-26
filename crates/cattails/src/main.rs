use std::fs::{self, read_to_string, File};
use std::path::PathBuf;
use std::time::{self, Instant};

use anyhow::{Context, Result};

use log::info;
use lsp_types::request::Completion;
use lsp_types::{
    request::GotoDefinition, GotoDefinitionResponse, InitializeParams, ServerCapabilities,
};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionResponse, Location, OneOf,
    Position, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions, Url,
};

use clap::Parser as ClapParser;
use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use sqlparser::parser::Parser;
use sqlparser::tokenizer::Tokenizer;
use structured_logger::{json::new_writer, Builder};
use tree_sitter::{Parser as TreeSitterParser, Point, Tree};

#[derive(ClapParser, Debug)]
#[clap()]
pub struct LspConfig {
    /// The file to pipe logs out to
    #[clap(short, long)]
    pub file: Option<String>,

    /// The log level to use, defaults to INFO
    /// Valid values are: TRACE, DEBUG, INFO, WARN, ERROR
    #[clap(short, long, default_value = "INFO")]
    pub level: String,
}

// XDG: ~/.config
// Apple: ~/Library/Preferences
// Windows: ~\AppData\Roaming
fn config_dir() -> Result<std::path::PathBuf> {
    dirs::config_dir().ok_or(anyhow::anyhow!("Could not find home directory"))
}

fn get_data_dir(file_name: &str) -> Result<std::path::PathBuf> {
    let mut data_dir = dirs::data_dir().ok_or(anyhow::anyhow!("Could not find home directory"))?;
    data_dir.push("Plunger");
    fs::create_dir_all(&data_dir)?;
    data_dir.push(file_name);
    Ok(data_dir)
}

struct SqlDocument {
    tree: Tree,
}

fn temp_parse_document(path: &PathBuf) -> Result<SqlDocument> {
    let timer = Instant::now();
    let mut parser = TreeSitterParser::new();
    parser
        .set_language(tree_sitter_sql::language())
        .expect("Error loading SQL grammar");
    let sql_one = fs::read_to_string(path)?;
    let result = parser
        .parse(&sql_one, None)
        .map(|tree| SqlDocument { tree })
        .context("error while parsing");
    eprintln!("took {:?}", timer.elapsed());
    result
}

fn main() -> Result<()> {
    let config = LspConfig::parse();

    // for now we can make this a func and worry about storing progressively parsed trees later
    // should be simple but ultimately would want a concurrent cache, basically need to check if
    // we've seen the file before non blocking and do an incremental parse with tree_sitter if we
    // have
    let mut parser = TreeSitterParser::new();
    parser
        .set_language(tree_sitter_sql::language())
        .expect("Error loading SQL grammar");
    let timer = Instant::now();
    let sql_one = "select why_unexpect from table_one;";
    let sql_two = "select why_enexpect from table_two;";
    let tree_one = parser.parse(sql_one, None).unwrap();
    let tree_two = parser.parse(sql_two, Some(&tree_one)).unwrap();
    dbg!(&timer.elapsed());
    dbg!(&tree_one.root_node().to_sexp());
    dbg!(&tree_two.root_node().to_sexp());
    // let root_node = tree.root_node();
    // let full_parse = time::Instant::now();
    // println!("{:?}", full_parse.sub(start_parse));
    // let desc = root_node
    //     .descendant_for_point_range(Point::new(0, 0), Point::new(0, 12))
    //     .unwrap();
    // let mut walker = desc.walk();

    // // clauses make up a statement.
    // // To give a base of our autocomplete we provide clauses that make up a statement
    // let mut iterator = desc.children(&mut walker);
    // let mut walker_two = desc.walk();
    // let mut iterator_two = desc.children(&mut walker_two);
    // dbg!(&desc);
    // dbg!(&iterator.next());
    // dbg!(&iterator_two.last());
    // dbg!(&iterator.next());
    // dbg!(&iterator.next());
    // dbg!(&iterator.next());
    // dbg!(&desc.kind());
    // dbg!(&desc.kind_id());

    let log_file = File::options()
        .create(true)
        .append(true)
        .open(get_data_dir("lsp.log")?)
        .unwrap();

    // todo: We can notify the lsp log file to the client _i think_
    eprintln!(
        "log file found at {}",
        &get_data_dir("lsp.log")?.to_str().unwrap()
    );

    Builder::with_level(&config.level)
        .with_target_writer("*", new_writer(log_file))
        .init();
    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    // todo: For live testing we can connect to this server.
    // That allows us to see the error logs directly
    let (connection, io_threads) = Connection::listen("127.0.0.1:8080")?;
    // let (connection, io_threads) = Connection::stdio();
    info!("connection established");
    // this for testing
    // let (connection, io_threads) = Connection::memory()?;
    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::INCREMENTAL),
                ..Default::default()
            },
        )),
        definition_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions::default()),
        ..Default::default()
    })?;
    let initialization_params = match connection.initialize(server_capabilities) {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

//todo(blocking): nothing should take a long time or block here, we should offload to a worker
//thread communicating through channels. crossbeam or a mpsc should do the job.
//explain: The reason is if we block this loop we hurt the UX
fn main_loop(connection: Connection, params: serde_json::Value) -> Result<()> {
    let _params: InitializeParams = serde_json::from_value(params)?;
    info!("starting example main loop");
    for msg in &connection.receiver {
        info!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                info!("got request: {req:?}");
                // todo: rust_analyzer uses https://github.com/rust-lang/rust-analyzer/blob/543d7e98dbcc0668528dbf3f5b32d752882baa33/crates/rust-analyzer/src/dispatch.rs#L33-L66 to match the requests. However for simplicity I believe a classic match for now will keep contributions easier, as demonstrated in htmx lsp https://github.com/ThePrimeagen/htmx-lsp/blob/0ad57f0f45b0663c1ab271431a0cfe78d18b5f78/lsp/src/handle.rs#L161-L171
                match req.method.as_str() {
                    "textDocument/completion" => {
                        match cast::<Completion>(req) {
                            Ok((id, params)) => {
                                info!("got completion request #{id}: {params:?}");
                                let path = params
                                    .text_document_position
                                    .text_document
                                    .uri
                                    .to_file_path()
                                    .unwrap();
                                let parsed = temp_parse_document(&path);
                                let result = Some(CompletionResponse::Array(vec![
                                    CompletionItem {
                                        label: "from".to_string(),
                                        detail: Some("from".to_string()),
                                        kind: Some(CompletionItemKind::KEYWORD),
                                        ..CompletionItem::default()
                                    },
                                    CompletionItem {
                                        label: "select".to_string(),
                                        detail: Some("select".to_string()),
                                        kind: Some(CompletionItemKind::KEYWORD),
                                        ..CompletionItem::default()
                                    },
                                ]));
                                let result = serde_json::to_value(&result)?;
                                let resp = Response {
                                    id,
                                    result: Some(result),
                                    error: None,
                                };
                                connection.sender.send(Message::Response(resp))?;
                                continue;
                            }
                            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                            Err(ExtractError::MethodMismatch(req)) => req,
                        };
                    }
                    // api.dev.liveramp.com/data-catalog-ui-service/assets
                    // we could fetch the above files convert them to sql and store them in our
                    // data directory. It's a novel approach to working with sql
                    // select column_name, data_type, character_maximum_length, column_default, is_nullable
                    // from INFORMATION_SCHEMA.COLUMNS where table_name = '<name of table>';
                    // can use the above as a generic default for getting sql info
                    "textDocument/definition" => {
                        match cast::<GotoDefinition>(req) {
                            Ok((id, params)) => {
                                info!("got gotoDefinition request #{id}: {params:?}");
                                let result = Some(GotoDefinitionResponse::Scalar(Location::new(
                                    Url::parse(
                                        "file:///Users/lcolli/Library/Application Support/Plunger/lsp.log",
                                    )?,
                                    lsp_types::Range::new(Position::new(25, 0), Position::new(25, 0)),
                                )));
                                let result = serde_json::to_value(&result).unwrap();
                                let resp = Response {
                                    id,
                                    result: Some(result),
                                    error: None,
                                };
                                connection.sender.send(Message::Response(resp))?;
                                continue;
                            }
                            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                            Err(ExtractError::MethodMismatch(req)) => req,
                        };
                    }
                    _ => {
                        info!("no dice");
                    }
                }
                // ...
            }
            Message::Response(resp) => {
                info!("got response: {resp:?}");
            }
            Message::Notification(not) => {
                info!("got notification: {not:?}");
            }
        }
    }
    Ok(())
}

fn cast<R>(req: Request) -> std::result::Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    match req.extract(R::METHOD) {
        other_req => other_req,
    }
}
