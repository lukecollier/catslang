Following this book https://interpreterbook.com/, firstly in go and then in rust!

Only the rust code will end up here.

Thoughts:

In rust theres a lot more that's able to leverage language constructs, for example in [ast.rs](src/ast.rs)
```rust
#[derive(Debug)]
pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}
impl<'a> Display for Program<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer: String = String::with_capacity(1024);
        for s in self.statements.iter() {
            buffer.write_str(&s.to_string())?;
        }
        write!(f, "{}", buffer)
    }
}
```

compared to in go

```go
type LetStatement struct {
	Token token.Token // the token.LET token
	Name  *Identifier
	Value Expression
}

func (ls *LetStatement) statementNode()       {}
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }
func (ls *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.String())
	out.WriteString(" = ")
	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}
	out.WriteString(";")
	return out.String()
}
```
Most of rusts complexity comes from knowing which macros and which structures to use for example we don't need to use a function called String as instead we implement a Display on the Statment achieving the same. 

Interesting to note that in go we also use dummy methods like in
```go
type Statement interface {
	Node
	statementNode()
}
```
This is due to go being nominally typed, meaning this is used later on by Thorsten Ball to differentiate if a node is a statement or an expression

In rust we run into problems due to it being structurally typed. Essentially generics can't allow any subtype to exist as they're given their own unique types at compile time.

So ```Vec<String>``` effectively becomes type ```VecString``` while ```Vec<OtherType>``` is a ```VecOtherType```. This functionally means that to get the equivelent code in rust we have to use enums.
```rust
#[derive(Debug)]
pub enum Node<'a> {
    Statement(Statement<'a>),
    Expression(Expression<'a>),
}

#[derive(Debug)]
pub enum Statement<'a> {
    LetStatement(LetStatement<'a>),
}


#[derive(Debug)]
pub struct Identifier<'a> {
    pub value: &'a str,
}

#[derive(Debug)]
pub struct LetStatement<'a> {
    pub name: Identifier<'a>,
    pub value: Expression<'a>,
}
```

The interesting difference from go is we have rust wanting the decided of "am I a statement" to be the trait type while in go the types can tag themselves.

I find go's approach refreshingly simple while rust's feel's much more solid and wouldn't leave much room for abuse.

while go allows this at the definiton of their struct. Neat.

It appaers though this allows rust to be very clever and flow a little nicer, you can use all your normal rust favourites without thinking about the complex actions the compiler is taking.

For example
```rust
impl<S: Statement> Display for Program<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer: String = String::with_capacity(1024);
        for s in self.statements.iter() {
            buffer.write_str(&s.to_string())?;
        }
        write!(f, "{}", buffer)
    }
}
```

When writing the above I first dutifully put `Display` as a +, before realising that Statement implement Node which Implements Display and rust handles this quite nicely. Allowing a very similar result to the go code below.

```go
func (p *Program) String() string {
	var out bytes.Buffer
	for _, s := range p.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}
```

The original author of the book leveraged go's simplicity a lot, for example theres a lot of helper functions. I could have replicated them over into rust but I decided to try and write them using rust constructs, for example in go error handling is done in a tuple fashion e.g
```go
if !p.expectPeek(token.IDENT) {
    return nil
}
```

while in rust we can use if let statements 
i.e 
```rust 
if let Token::Semicolon = &self.peek_token {
} else {
    return Err(anyhow!(
        "expected next token to be {} but got {} instead",
        Token::Assign,
        &self.peek_token
    ));
};
```

what I like about go is that the functions are re-useable and form consistent errors. It's at the cost of strong types, in go this is all achived by things being very flexible. To port the code exactly we could use 
```rs 
fn peek_token_is(&mut self, token: &Token) -> bool {
    return std::mem::discriminant(&self.peek_token) == std::mem::discriminant(token);
}
```
but the above would result in a funny situation when using `Token::Int` or `Token::Ident` we would have to create something just for the check, which results in usage like this 
```rs 
peek_token_is(Token::Ident("".to_string());
```
yuck.

A better attempt is to split each function for expect_peek_assign which we could implement like the below.
```rs
fn expect_peek_assign(&mut self) -> Result<()> {
    match &self.peek_token {
        Token::Assign => return Ok(()),
        _ => Err(anyhow!(self.peek_error(&Token::Assign))),
    }
}

fn peek_error(&mut self, token: &Token) -> String {
    format!(
        "expected next token to be {:?}, got {:?} instead",
        token, self.peek_token
    )
}
```
We still get the weirdities with passing a dummy token to the peek_error but all in all we get something approximating the go approach. It goes to hightlight how the flexibility can reduce vast amounts of code. For this use case the negatives are basically non existent, we're just writing more boilerplate in rust. This statement is muted as we're not 1-1 mapping the go code to rust, but a decent flow I found was abstracting the common control flows to their own helper expect functions while using the pattern matching for idents and things that were variable using the match statements.

```rs
fn parse_let_statement(&mut self) -> Result<LetStatement> {
    let name = match &self.peek_token() {
        Token::Ident(ident) => Identifier {
            value: ident.to_string(),
        },
        token => return Err(anyhow!(self.peek_error(&Token::Ident("".to_string())))),
    };
    self.next_token();
    self.expect_peek_assign()?;
    self.next_token();
    let value = self.parse_expression(Precedence::Lowest)?;
    self.expect_peek_semicolon()?;
    Ok(LetStatement { value, name })
}
```
This let's the error message be consistent(ish) while also giving us a way to get the information out of our tokens.

Another curious difference is since we've implemented our lexer as a iterator we can get the peekable elements, the original rust port had this:
```rs
fn next_token(&mut self) {
    // todo: Fix this clone
    self.cur_token = self.peek_token.clone();
    self.peek_token = self.l.next_token();
}
```
Which isn't very good, we're cloing and that's _relatively_ expensive! So by calling changing our struct we can make our lexer peekable and use that instead, this offloads all the details to the lexer and gives us a simple interface, the major change is we define a method to get the iterators current peek, this avoids clone! Neat.
```rs
#[derive(Debug)]
struct Parser<'a> {
    l: Peekable<Lexer<'a>>,
    cur_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(l: Lexer<'a>) -> Parser<'a> {
        let mut peekable_l = l.peekable();
        // also we no longer need to utilise the next_token to init
        // we pre get the first token (or EOF)
        let cur_token = peekable_l.next().unwrap_or(Token::EOF);
        let p = Self {
            l: peekable_l,
            cur_token,
            errors: vec![],
        };
        p
    }
    //...
}

//...
fn next_token(&mut self) {
    self.cur_token = self.l.next().unwrap_or(Token::EOF);
}

fn peek_token(&mut self) -> &Token {
    self.l.peek().unwrap_or(&Token::EOF)
}
```
The above optimisations were kind of overhead though! Go handles this case fairly opaquely and I belive it'll be copying data under the hood as it's a primative type. This means our rust implementation should be more efficient as it's just a reference.

But the issue with this optimisation is it's steeped in skill issues, basically how much do you know about the memory model of rust and it's structures? Go just _does the thing_ well enough that you probably wouldn't care that it's copying.


## Implementing Evaluator

Evaluation seem's to be where rust comes into it's own, the gaurentee's from the compiler and trait system play really well here.

Compared with the go solution the compiler ensures we tackle every path and clearly shows us all the posibilities. The trait system is very useful as we can define a trait that exposes a single function, allowing us to easily 

A great example of the benefits is Enum's, in the go variant they use const to instantiate and object in rust we can use an enum. This gives us the singleton property but as a language structure and helps prevent abuse.
```rs
fn eval<'a>(self: &'a Self) -> Object<'a> {
    match self {
        //...
        Expression::Boolean(crate::ast::Boolean { value: true }) => Object::Bool(Boolean::True),
        Expression::Boolean(crate::ast::Boolean { value: false }) => {
            Object::Bool(Boolean::False)
        }
        Expression::IntegerLiteral(literal) => Object::Int(Integer {
            value: &literal.value,
        }),
        //...
    }
}

```
