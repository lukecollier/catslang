use crossterm::{
    cursor::{MoveLeft, MoveRight, MoveToColumn, MoveToNextLine},
    event::*,
    execute, queue,
    style::{Print, Stylize},
    terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType},
};
use meow::{
    ast::Program,
    eval::{Environment, Eval, System},
    lexer, parser,
};
use std::io::{self, Stdout};

const PROMPT: &str = ">> ";

// todo: We can use strum to give us the discriminate names for auto complete, but does lack auto
// creating a brace :think:
// fn get_names() {
//     Token::VARIANTS
// }
fn parse(src: &String) -> Result<Program, String> {
    let lexer = lexer::Lexer::new(&src);
    let mut parser = parser::Parser::new(lexer);
    match parser.parse_program() {
        Ok(success) => Ok(success),
        Err(err) => Result::Err(err.to_string()),
    }
}

// note: This ends up being our presentation layer, it has convinience methods for writing to the
// terminal.
// thanks to the seperation of concerns we can provide channels at construction for eventing.
// The events would allow us to handle async which we can support using something like io_uring,
// neato!
struct CrosstermTerminal<'a> {
    pub stdout: &'a mut Stdout,
}

impl<'a> CrosstermTerminal<'a> {
    fn from_stdout(stdout: &'a mut Stdout) -> Self {
        Self { stdout }
    }

    fn move_left(&mut self, by: u16) -> std::io::Result<()> {
        execute!(self.stdout, MoveLeft(by))?;
        Ok(())
    }

    fn move_right(&mut self, by: u16) -> std::io::Result<()> {
        execute!(self.stdout, MoveRight(by))?;
        Ok(())
    }

    fn execute(&mut self, input: String, repl: &mut Repl) -> std::io::Result<()> {
        queue!(
            self.stdout,
            MoveToNextLine(1),
            Clear(ClearType::CurrentLine),
            Print(format!(">> {input}\r\n")),
        )?;
        let result = match repl.run(self) {
            Ok(success) => success.to_string().dark_grey(),
            Err(err) => err.to_string().dark_red().bold(),
        };
        queue!(
            self.stdout,
            MoveToNextLine(1),
            Clear(ClearType::CurrentLine),
            Print(result),
            Print("\r\n"),
        )?;
        Ok(())
    }

    fn prompt(&mut self, input: &str, column: u16) -> std::io::Result<()> {
        execute!(
            self.stdout,
            MoveToColumn(0),
            Clear(ClearType::UntilNewLine),
            Print(PROMPT.dark_yellow().bold()),
            Print(input),
            MoveToColumn(column + PROMPT.len() as u16),
        )?;
        Ok(())
    }
}

impl<'a> System for CrosstermTerminal<'a> {
    fn println(&mut self, line: &str) -> std::io::Result<()> {
        execute!(self.stdout, Print(line), Print("\r\n"))?;
        Ok(())
    }
}

struct Repl {
    history: Vec<String>,
    input: String,
    stashed: String,
    history_cursor: usize,
    column: usize,
    env: Environment,
}

// this let's state be maintained safely
impl Repl {
    fn new() -> Self {
        let history = Vec::with_capacity(1024);
        let input = String::new();
        let stashed = String::new();
        let history_cursor = 0;
        let column = 0;
        let env = Environment::new();
        Self {
            history,
            input,
            stashed,
            history_cursor,
            column,
            env,
        }
    }

    pub fn reset_input(&mut self) {
        self.history.push(self.input.clone());
        self.input.clear();
        self.history_cursor = self.history.len();
        self.column = 0;
    }

    pub fn run<S: System>(&mut self, runtime: &mut S) -> Result<String, String> {
        match parse(&self.input) {
            Ok(parsed) => match parsed.eval(&mut self.env, runtime) {
                Ok(eval) => {
                    self.reset_input();
                    Ok(format!("{eval}"))
                }
                Err(error) => {
                    self.reset_input();
                    Err(format!("{error}"))
                }
            },
            Err(error) => {
                self.reset_input();
                Err(format!("{error}"))
            }
        }
    }

    pub fn update_input(&mut self, new: String) {
        self.input = new;
        self.column = self.input.len();
    }

    pub fn delete_last(&mut self) {
        if self.column == 0 {
            return;
        }
        self.column -= 1;
        if self.column == self.input.len() {
            self.input.pop();
        } else {
            self.input.remove(self.column);
        }
    }

    pub fn left(&mut self) -> u16 {
        if self.column > 0 {
            self.column -= 1;
            1
        } else {
            0
        }
    }

    pub fn right(&mut self) -> u16 {
        if self.column < self.input.len() {
            self.column += 1;
            1
        } else {
            0
        }
    }

    // goes backwards in hisotory
    pub fn history_back(&mut self) {
        if self.history_cursor == self.history.len() {
            self.stashed = self.input.clone();
        }
        self.history_cursor = self.history_cursor.checked_sub(1).unwrap_or(0);
        match self.history.get(self.history_cursor) {
            Some(line) => {
                self.update_input(line.clone());
            }
            None => (),
        }
    }

    // goes backwards in hisotory
    pub fn history_forward(&mut self) {
        self.history_cursor = self
            .history_cursor
            .checked_add(1)
            .unwrap_or(self.history.len())
            .clamp(0, self.history.len());
        if self.history_cursor == self.history.len() {
            self.update_input(self.stashed.clone());
        }
        match self.history.get(self.history_cursor) {
            Some(line) => {
                self.update_input(line.clone());
            }
            None => (),
        }
    }

    // gives back the current input
    pub fn add_ch(&mut self, ch: char) {
        let mut buffer = String::new();
        buffer.push_str(&self.input[0..self.column]);
        buffer.push(ch);
        buffer.push_str(&self.input[self.column..self.input.len()]);
        self.column += 1;
        self.input = buffer.to_string();
    }

    pub fn add_str(&mut self, s: &str) {
        for ch in s.chars() {
            self.add_ch(ch);
        }
    }

    // gives back the current input
    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn column(&self) -> u16 {
        self.column.try_into().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use meow::eval::InMemorySystem;

    use super::*;
    fn test_repl() -> Repl {
        let history = Vec::with_capacity(1024);
        let input = String::new();
        let stashed = String::new();
        let history_cursor = 0;
        let column = 0;
        let env = Environment::new();
        Repl {
            history,
            input,
            stashed,
            history_cursor,
            column,
            env,
        }
    }

    #[test]
    fn test_initial_state() {
        let repl = test_repl();
        let init = repl.input();
        assert_eq!(init, "");
    }

    #[test]
    fn can_go_back_and_forward_in_history() {
        let mut repl = test_repl();
        let mut in_memory = InMemorySystem::new();
        repl.add_str("let a = 1;");
        repl.run(&mut in_memory).unwrap();
        repl.add_str("let b = 2;");
        repl.run(&mut in_memory).unwrap();
        repl.add_str("let c = 3;");
        repl.run(&mut in_memory).unwrap();
        repl.history_back();
        repl.history_back();
        repl.history_back();
        repl.history_forward();
        assert_eq!(repl.input(), "let b = 2;");
    }

    #[test]
    fn can_get_two_history_back() {
        let mut repl = test_repl();
        let mut in_memory = InMemorySystem::new();
        repl.add_str("let a = 1;");
        repl.run(&mut in_memory).unwrap();
        repl.add_str("let b = 2;");
        repl.run(&mut in_memory).unwrap();
        repl.add_str("let c = 3;");
        repl.run(&mut in_memory).unwrap();
        repl.history_back();
        repl.history_back();
        assert_eq!(repl.input(), "let b = 2;");
    }

    #[test]
    fn can_get_one_history_back() {
        let mut repl = test_repl();
        let mut in_memory = InMemorySystem::new();
        repl.add_str("let f = 1;");
        repl.run(&mut in_memory).unwrap();
        repl.history_back();
        assert_eq!(repl.input(), "let f = 1;");
    }

    #[test]
    fn when_at_start_of_line_and_delete_ok() {
        let mut repl = test_repl();
        repl.add_ch('a');
        repl.add_ch('b');
        repl.add_ch('c');
        repl.add_ch('d');
        repl.left();
        repl.left();
        repl.left();
        repl.left();
        repl.delete_last();
        assert_eq!(repl.input(), "abcd");
    }

    #[test]
    fn can_insert_characters_inbetween_line() {
        let mut repl = test_repl();
        repl.add_ch('a');
        repl.add_ch('b');
        repl.add_ch('c');
        repl.add_ch('d');
        repl.left();
        repl.left();
        repl.add_ch('e');
        repl.add_ch('f');
        repl.left();
        repl.left();
        repl.add_ch('g');
        assert_eq!(repl.input(), "abgefcd");
    }

    #[test]
    fn can_delete_line_after_moving() {
        let mut repl = test_repl();
        repl.add_ch('a');
        repl.add_ch('b');
        repl.add_ch('c');
        repl.add_ch('d');
        repl.left();
        repl.right();
        repl.left();
        repl.left();
        repl.left();
        repl.right();
        repl.delete_last();
        assert_eq!(repl.input(), "acd");
    }

    #[test]
    fn can_delete_line_after_moving_right() {
        let mut repl = test_repl();
        repl.add_ch('a');
        repl.add_ch('b');
        repl.add_ch('c');
        repl.add_ch('d');
        repl.right();
        repl.right();
        repl.right();
        repl.right();
        repl.right();
        repl.left();
        repl.delete_last();
        assert_eq!(repl.input(), "abd");
    }

    #[test]
    fn can_delete_line() {
        let mut repl = test_repl();
        repl.add_ch('a');
        repl.add_ch('b');
        repl.add_ch('c');
        repl.add_ch('d');
        repl.delete_last();
        repl.delete_last();
        repl.delete_last();
        repl.delete_last();
        assert_eq!(repl.input(), "");
    }

    #[test]
    fn can_delete_characters() {
        let mut repl = test_repl();
        repl.add_ch('a');
        repl.add_ch('b');
        repl.add_ch('c');
        repl.add_ch('d');
        repl.delete_last();
        assert_eq!(repl.input(), "abc");
    }

    #[test]
    fn can_delete_characters_half_way_through() {
        let mut repl = test_repl();
        repl.add_ch('a');
        repl.add_ch('b');
        repl.add_ch('c');
        repl.add_ch('d');
        repl.left();
        repl.left();
        repl.delete_last();
        assert_eq!(repl.input(), "acd");
    }

    #[test]
    fn can_add_characters() {
        let mut repl = test_repl();
        repl.add_ch('a');
        repl.add_ch('b');
        repl.add_ch('c');
        repl.add_ch('d');
        assert_eq!(repl.input(), "abcd");
    }
}

// todo: We probably need a single channel
fn main() -> io::Result<()> {
    // let stdin = io::stdin(); // We get `Stdin` here.
    let mut stdout = io::stdout(); // We get `Stdin` here.
    enable_raw_mode()?;
    execute!(
        stdout,
        Print("Welcome to "),
        Print("Meow!\n\r".bold().dark_magenta()),
        Print("<ctrl + c> to exit.\n\r"),
    )?;
    let mut runtime = CrosstermTerminal::from_stdout(&mut stdout);
    let mut repl = Repl::new();
    loop {
        runtime.prompt(repl.input(), repl.column())?;
        match read()? {
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                modifiers: KeyModifiers::CONTROL,
                kind: KeyEventKind::Press,
                state: _,
            }) => {
                break;
            }
            Event::Key(KeyEvent {
                code: KeyCode::Right,
                modifiers: KeyModifiers::NONE,
                kind: KeyEventKind::Press,
                state: _,
            }) => {
                runtime.move_right(repl.right())?;
                ()
            }
            Event::Key(KeyEvent {
                code: KeyCode::Left,
                modifiers: KeyModifiers::NONE,
                kind: KeyEventKind::Press,
                state: _,
            }) => {
                runtime.move_left(repl.left())?;
                ()
            }
            Event::Key(KeyEvent {
                code: KeyCode::Backspace,
                modifiers: KeyModifiers::NONE,
                kind: KeyEventKind::Press,
                state: _,
            }) => {
                repl.delete_last();
                ()
            }
            Event::Key(KeyEvent {
                code: KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                kind: KeyEventKind::Press,
                state: _,
            }) => {
                repl.history_back();
            }
            Event::Key(KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                kind: KeyEventKind::Press,
                state: _,
            }) => {
                repl.history_forward();
            }
            Event::Key(KeyEvent {
                code: KeyCode::Enter,
                modifiers: KeyModifiers::NONE,
                kind: KeyEventKind::Press,
                state: _,
            }) => runtime.execute(repl.input().to_string(), &mut repl)?,
            Event::Key(KeyEvent {
                code: KeyCode::Char(c),
                modifiers: KeyModifiers::NONE,
                kind: KeyEventKind::Press,
                state: _,
            }) => {
                repl.add_ch(c);
            }
            _ => (),
        }
    }
    disable_raw_mode()?;
    Ok(())
}
