use std::iter::Peekable;

use crate::{
    ast::{
        BlockStatement, Boolean, CallExpression, Expression, FunctionLiteral, Identifier,
        IfExpression, InfixExpression, InfixOperator, IntegerLiteral, LetStatement,
        PrefixExpression, Program, ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::Token,
};
use anyhow::*;

#[derive(PartialEq, PartialOrd, Ord, Eq, Debug, Clone)]
enum Precedence {
    Lowest,
    Equals,
    LessGraeter,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

fn precedence(token: &Token) -> Precedence {
    use Precedence::*;
    match token {
        Token::Eq => Equals,
        Token::NotEq => Equals,
        Token::Lt => LessGraeter,
        Token::Gt => LessGraeter,
        Token::Plus => Sum,
        Token::Minus => Sum,
        Token::Slash => Product,
        Token::Asterisk => Product,
        Token::LParen => Call,
        Token::LBracket => Index,
        _ => Lowest,
    }
}
#[derive(Debug)]
pub struct Parser<'a> {
    l: Peekable<Lexer<'a>>,
    cur_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(l: Lexer<'a>) -> Parser<'a> {
        let mut peekable_l = l.peekable();
        let cur_token = peekable_l
            .next()
            .unwrap_or(Ok(Token::EOF))
            .unwrap_or(Token::EOF);
        let p = Self {
            l: peekable_l,
            cur_token,
            errors: vec![],
        };
        p
    }

    pub fn from_str(str: &'a str) -> Parser<'a> {
        let mut peekable_l = Lexer::new(str).peekable();
        let cur_token = peekable_l
            .next()
            .unwrap_or(Ok(Token::EOF))
            .unwrap_or(Token::EOF);
        let p = Self {
            l: peekable_l,
            cur_token,
            errors: vec![],
        };
        p
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program {
            // Todo: Estimate capacity required better
            statements: Vec::with_capacity(512),
        };
        while self.cur_token != Token::EOF {
            match self.parse_statement() {
                anyhow::Result::Ok(stmt) => program.statements.push(stmt),
                Err(err) => self.errors.push(err.to_string()),
            }
            self.next_token()
        }
        if self.errors.is_empty() {
            Ok(program)
        } else {
            // can actually return the list here
            Err(anyhow!(self.errors.join(", ")))
        }
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.cur_token {
            Token::Let => {
                let s = self.parse_let_statement()?;
                Ok(Statement::LetStatement(s))
            }
            Token::Return => {
                let s = self.parse_return_statement()?;
                Ok(Statement::ReturnStatement(s))
            }
            Token::Function => {
                let s = self.parse_function_statement()?;
                Ok(s)
            }
            _ => {
                // TODO: These statements can be without a semicolon :(
                let s = self.parse_expression_statement()?;
                Ok(Statement::ExpressionStatement(s))
            }
        }
    }

    fn parse_named_function(&mut self) -> Result<Statement> {
        let ident = self.parse_identifier()?;
        self.next_token();
        let function = self.parse_function_literal()?;
        Ok(Statement::LetStatement(LetStatement {
            name: ident,
            value: Expression::FunctionLiteral(function),
        }))
    }

    fn parse_function_statement(&mut self) -> Result<Statement> {
        match self.peek_token() {
            Token::Ident(_) => self.parse_named_function(),
            Token::LParen => {
                let parsed = self.parse_expression_statement()?;
                Ok(Statement::ExpressionStatement(parsed))
            }
            _ => Err(anyhow!("")),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Expression> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token() == &Token::Semicolon {
            self.next_token();
        };
        Ok(expression)
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement> {
        self.next_token();
        let expression = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek_semicolon()?;
        Ok(ReturnStatement {
            return_value: expression,
        })
    }

    fn parse_identifier(&mut self) -> Result<Identifier> {
        match &self.peek_token() {
            Token::Ident(ident) => Ok(Identifier {
                value: ident.to_string(),
            }),
            _ => Err(anyhow!(self.peek_error(&Token::Ident("".to_string())))),
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        let name = self.parse_identifier()?;
        self.next_token();
        self.expect_peek_assign()?;
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek_semicolon()?;
        Ok(LetStatement { value, name })
    }

    fn parse_hash_literal(&mut self) -> Result<Expression> {
        if self.peek_token() == &Token::RBrace {
            self.next_token();
            return Ok(Expression::hash_map(vec![]));
        }
        let mut expressions = vec![];
        while self.peek_token() != &Token::RBrace {
            self.next_token();
            let left = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek_colon()?;
            self.next_token();
            let right = self.parse_expression(Precedence::Lowest)?;
            expressions.push((left, right));
            if self.peek_token() == &Token::Comma {
                self.next_token();
            }
        }
        self.expect_peek_rbrace()?;
        Ok(Expression::hash_map(expressions))
    }

    fn parse_array_literal(&mut self) -> Result<Expression> {
        if self.peek_token() == &Token::RBracket {
            self.next_token();
            return Ok(Expression::array(vec![]));
        }
        let mut expressions = vec![];
        while self.peek_token() != &Token::RBracket {
            self.next_token();
            expressions.push(self.parse_expression(Precedence::Lowest)?);
            if self.peek_token() == &Token::Comma {
                self.next_token();
            }
        }
        self.expect_peek_rbracket()?;
        Ok(Expression::array(expressions))
    }

    fn parse_prefix(&mut self) -> Result<Expression> {
        match &self.cur_token {
            Token::Ident(name) => Ok(Expression::Identifier(Identifier {
                value: name.to_string(),
            })),
            Token::Int(int_str) => {
                let value = int_str.parse::<i64>()?;
                Ok(Expression::IntegerLiteral(IntegerLiteral { value }))
            }
            Token::Str(string) => Ok(Expression::string(string.to_string())),
            Token::LBracket => self.parse_array_literal(),
            Token::LBrace => self.parse_hash_literal(),
            Token::True => Ok(Expression::Boolean(Boolean { value: true })),
            Token::False => Ok(Expression::Boolean(Boolean { value: false })),
            Token::Minus => {
                self.next_token();
                let r = self.parse_expression(Precedence::Prefix)?;
                Ok(Expression::PrefixExpression(PrefixExpression {
                    operator: crate::ast::PrefixOperator::Minus,
                    right: r.into(),
                }))
            }
            Token::Bang => {
                self.next_token();
                let r = self.parse_expression(Precedence::Prefix)?;
                Ok(Expression::PrefixExpression(PrefixExpression {
                    operator: crate::ast::PrefixOperator::Bang,
                    right: r.into(),
                }))
            }
            Token::Function => {
                let function_literal = self.parse_function_literal()?;
                Ok(Expression::FunctionLiteral(function_literal))
            }
            Token::LParen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            token => Err(anyhow!("no prefix for token {}", token)),
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        self.expect_peek_lparen()?;
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek_rparen()?;
        let consequence = self.parse_block_statement()?;
        if self.peek_token() == &Token::Else {
            self.next_token();
            let alternative = self.parse_block_statement()?;
            Ok(Expression::IfExpression(IfExpression {
                condition: condition.into(),
                consequence,
                alternative,
            }))
        } else {
            Ok(Expression::IfExpression(IfExpression {
                condition: condition.into(),
                consequence,
                alternative: BlockStatement::empty(),
            }))
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next_token();
        let expression = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek_rparen()?;
        Ok(expression)
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>> {
        self.expect_peek_lparen()?;
        if self.peek_token() == &Token::RParen {
            self.next_token();
            return Ok(vec![]);
        }
        let mut identifiers = Vec::with_capacity(12);
        let first_ident = self.parse_identifier()?;
        self.next_token();
        identifiers.push(first_ident);
        while self.peek_token() == &Token::Comma {
            self.next_token();
            let ident = self.parse_identifier()?;
            identifiers.push(ident);
            self.next_token();
        }
        self.expect_peek_rparen()?;
        Ok(identifiers)
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        self.expect_peek_lbrace()?;
        let mut statements: Vec<Statement> = vec![];
        while self.peek_token() != &Token::RBrace && self.peek_token() != &Token::EOF {
            self.next_token();
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }
        self.expect_peek_rbrace()?;
        Ok(BlockStatement { statements })
    }

    fn parse_function_literal(&mut self) -> Result<FunctionLiteral> {
        let paramaters = self.parse_function_parameters()?;
        let body = self.parse_block_statement()?;
        Ok(FunctionLiteral { paramaters, body })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let precedence = precedence(&self.cur_token);
        let op: InfixOperator = self.cur_token.clone().try_into()?;
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(Expression::InfixExpression(InfixExpression {
            left: left.into(),
            operator: op,
            right: right.into(),
        }))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        let mut args = Vec::with_capacity(8);
        if self.peek_token() == &Token::RParen {
            self.next_token();
            return Ok(vec![]);
        }
        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);
        while self.peek_token() == &Token::Comma {
            self.next_token();
            self.next_token();
            let expr = self.parse_expression(Precedence::Lowest);
            args.push(expr?);
        }
        Ok(args)
    }

    // TODO: I don't like that the right paren validation comes from outside the house
    fn parse_call_expression(&mut self, callee: Expression) -> Result<Expression> {
        let args = self.parse_call_arguments()?;
        self.expect_peek_rparen()?;
        Ok(Expression::CallExpression(CallExpression {
            function: callee.try_into()?,
            arguments: args,
        }))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression> {
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek_rbracket()?;
        Ok(Expression::index_expression(left, index))
    }

    // note: This is a nifty trick, at the call site there was a problem where we had to .clone()
    // the left if this function took just an argument and returned a Option<Result<Expression>>
    // so by returning a function it removed the need for clone. I'm certain it's not the only one
    // but still very cool!
    fn parse_infix_fn(&mut self) -> Option<fn(&mut Self, Expression) -> Result<Expression>> {
        match &self.peek_token() {
            Token::Plus => Some(Self::parse_infix_expression),
            Token::Minus => Some(Self::parse_infix_expression),
            Token::Slash => Some(Self::parse_infix_expression),
            Token::Asterisk => Some(Self::parse_infix_expression),
            Token::Eq => Some(Self::parse_infix_expression),
            Token::NotEq => Some(Self::parse_infix_expression),
            Token::Lt => Some(Self::parse_infix_expression),
            Token::Gt => Some(Self::parse_infix_expression),
            Token::LParen => Some(Self::parse_call_expression),
            Token::LBracket => Some(Self::parse_index_expression),
            _ => None,
        }
    }

    fn parse_expression(&mut self, precendence: Precedence) -> Result<Expression> {
        let mut left_expr = self.parse_prefix()?;
        while self.peek_token() != &Token::Semicolon && precendence < self.peek_precedence() {
            if let Some(infix_result) = self.parse_infix_fn() {
                self.next_token();
                left_expr = infix_result(self, left_expr)?;
            } else {
                return Ok(left_expr);
            }
        }
        Ok(left_expr)
    }

    fn peek_precedence(&mut self) -> Precedence {
        precedence(&self.peek_token())
    }

    fn next_token(&mut self) {
        self.cur_token = self
            .l
            .next()
            .unwrap_or(Ok(Token::EOF))
            .unwrap_or(Token::EOF);
    }

    fn peek_token(&mut self) -> &Token {
        self.l
            .peek()
            .map(|result| result.as_ref().unwrap_or(&Token::EOF))
            .unwrap_or(&Token::EOF)
    }

    fn expect_peek_rbracket(&mut self) -> Result<()> {
        match &self.peek_token() {
            Token::RBracket => {
                self.next_token();
                Ok(())
            }
            _ => Err(anyhow!(self.peek_error(&Token::RBrace))),
        }
    }

    fn expect_peek_rbrace(&mut self) -> Result<()> {
        match &self.peek_token() {
            Token::RBrace => {
                self.next_token();
                Ok(())
            }
            _ => Err(anyhow!(self.peek_error(&Token::RBrace))),
        }
    }

    fn expect_peek_lbrace(&mut self) -> Result<()> {
        match &self.peek_token() {
            Token::LBrace => {
                self.next_token();
                Ok(())
            }
            _ => Err(anyhow!(self.peek_error(&Token::LBrace))),
        }
    }

    fn expect_peek_rparen(&mut self) -> Result<()> {
        match self.peek_token() {
            Token::RParen => {
                self.next_token();
                Ok(())
            }
            _ => Err(anyhow!(self.peek_error(&Token::RParen))),
        }
    }

    fn expect_peek_lparen(&mut self) -> Result<()> {
        match &self.peek_token() {
            Token::LParen => {
                self.next_token();
                Ok(())
            }
            _ => Err(anyhow!(self.peek_error(&Token::LParen))),
        }
    }

    fn expect_peek_colon(&mut self) -> Result<()> {
        match &self.peek_token() {
            Token::Colon => {
                self.next_token();
                Ok(())
            }
            _ => Err(anyhow!(self.peek_error(&Token::Colon))),
        }
    }

    fn expect_peek_semicolon(&mut self) -> Result<()> {
        match &self.peek_token() {
            Token::Semicolon => {
                self.next_token();
                Ok(())
            }
            _ => Err(anyhow!(self.peek_error(&Token::Semicolon))),
        }
    }

    fn expect_peek_assign(&mut self) -> Result<()> {
        match &self.peek_token() {
            Token::Assign => {
                self.next_token();
                Ok(())
            }
            _ => Err(anyhow!(self.peek_error(&Token::Assign))),
        }
    }

    fn peek_error(&mut self, token: &Token) -> anyhow::Error {
        anyhow!(format!(
            "expected next token to be {:?}, got {:?} instead",
            token,
            self.peek_token()
        ))
    }
}

#[cfg(test)]
mod tests {

    use crate::ast::{Callee, PrefixOperator};
    use pretty_assertions::assert_eq;

    use super::*;

    fn let_statement<'a>(identifier: &'a str, value: Expression) -> Program {
        Program {
            statements: vec![Statement::LetStatement(LetStatement {
                name: Identifier {
                    value: identifier.to_string(),
                },
                value,
            })],
        }
    }

    fn return_statement<'a>(return_value: Expression) -> Program {
        Program {
            statements: vec![Statement::ReturnStatement(ReturnStatement { return_value })],
        }
    }

    fn prefix_expression(operator: PrefixOperator, value: i64) -> Program {
        Program {
            statements: vec![Statement::expression(Expression::prefix_expression(
                operator,
                Expression::IntegerLiteral(IntegerLiteral { value }).into(),
            ))],
        }
    }

    fn infix_expression<'a>(left: i64, operator: InfixOperator, right: i64) -> Program {
        Program {
            statements: vec![Statement::ExpressionStatement(Expression::InfixExpression(
                InfixExpression {
                    left: Expression::IntegerLiteral(IntegerLiteral { value: left }).into(),
                    operator: operator,
                    right: Expression::IntegerLiteral(IntegerLiteral { value: right }).into(),
                },
            ))],
        }
    }

    #[test]
    fn index_expression_parsing() {
        let lexer = crate::lexer::Lexer::new("myArray[1 + 1]");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::expression(Expression::index_expression(
                Expression::identifier("myArray"),
                Expression::infix_expression(
                    Expression::integer_literal(1),
                    InfixOperator::Plus,
                    Expression::integer_literal(1),
                ),
            ))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn identifier_expression() {
        let lexer = crate::lexer::Lexer::new("foobar;");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::expression(Expression::identifier("foobar"))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn function_statement_parsing() {
        // todo: Is this equivelent to letting to an anonymous function?
        // I mean it is but weird.
        let lexer = crate::lexer::Lexer::new("fn add(x, y) { x + y; }");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::let_statement(
                Identifier::from_str("add"),
                Expression::FunctionLiteral(FunctionLiteral {
                    paramaters: vec![
                        Identifier {
                            value: "x".to_string(),
                        },
                        Identifier {
                            value: "y".to_string(),
                        },
                    ],
                    body: crate::ast::BlockStatement {
                        statements: vec![Statement::ExpressionStatement(
                            Expression::InfixExpression(InfixExpression {
                                left: Expression::Identifier(Identifier {
                                    value: "x".to_string(),
                                })
                                .into(),
                                operator: InfixOperator::Plus,
                                right: Expression::Identifier(Identifier {
                                    value: "y".to_string(),
                                })
                                .into(),
                            }),
                        )],
                    },
                }),
            )],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn annonymous_function_literal_parsing() {
        let lexer = crate::lexer::Lexer::new("fn(x, y) { x + y; };");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::FunctionLiteral(
                FunctionLiteral {
                    paramaters: vec![
                        Identifier {
                            value: "x".to_string(),
                        },
                        Identifier {
                            value: "y".to_string(),
                        },
                    ],
                    body: crate::ast::BlockStatement {
                        statements: vec![Statement::ExpressionStatement(
                            Expression::InfixExpression(InfixExpression {
                                left: Expression::Identifier(Identifier {
                                    value: "x".to_string(),
                                })
                                .into(),
                                operator: InfixOperator::Plus,
                                right: Expression::Identifier(Identifier {
                                    value: "y".to_string(),
                                })
                                .into(),
                            }),
                        )],
                    },
                },
            ))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn call_expression_parsing() {
        let lexer = crate::lexer::Lexer::new("add(1, 2 * 3, 4 + 5);");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::expression(Expression::call(
                Callee::identifier_str("add"),
                vec![
                    Expression::integer_literal(1),
                    Expression::infix_expression(
                        Expression::integer_literal(2).into(),
                        InfixOperator::Multiply,
                        Expression::integer_literal(3).into(),
                    ),
                    Expression::infix_expression(
                        Expression::integer_literal(4).into(),
                        InfixOperator::Plus,
                        Expression::integer_literal(5).into(),
                    ),
                ],
            ))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn else_expression_parse() {
        let lexer = crate::lexer::Lexer::new("if (x < y) { x } else { y }");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::IfExpression(
                IfExpression {
                    condition: Expression::InfixExpression(InfixExpression {
                        left: Expression::Identifier(Identifier::from_str("x")).into(),
                        operator: InfixOperator::Lt,
                        right: Expression::Identifier(Identifier::from_str("y")).into(),
                    })
                    .into(),
                    consequence: BlockStatement {
                        statements: vec![Statement::ExpressionStatement(Expression::Identifier(
                            Identifier::from_str("x"),
                        ))],
                    },
                    alternative: BlockStatement {
                        statements: vec![Statement::ExpressionStatement(Expression::Identifier(
                            Identifier::from_str("y"),
                        ))],
                    },
                },
            ))],
        };
        assert_eq!(parsed, expected);
    }

    // todo: Make this illegal
    #[test]
    fn if_statement_expression_parsing() {
        let lexer = crate::lexer::Lexer::new("if (x < y) { x }");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::IfExpression(
                IfExpression {
                    condition: Expression::InfixExpression(InfixExpression {
                        left: Expression::Identifier(Identifier::from_str("x")).into(),
                        operator: InfixOperator::Lt,
                        right: Expression::Identifier(Identifier::from_str("y")).into(),
                    })
                    .into(),
                    consequence: BlockStatement {
                        statements: vec![Statement::ExpressionStatement(Expression::Identifier(
                            Identifier::from_str("x"),
                        ))],
                    },
                    alternative: BlockStatement::empty(),
                },
            ))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn grouped_expression_parsing() {
        let (parsed, expected): (Vec<String>, Vec<String>) = [
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ]
        .into_iter()
        .map(|(statement, expected)| {
            let lexer = crate::lexer::Lexer::new(statement);
            let mut parser = Parser::new(lexer);
            (
                parser.parse_program().unwrap().to_string(),
                expected.to_string(),
            )
        })
        .unzip();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn string_literal_parses() {
        let lexer = crate::lexer::Lexer::new("\"hello\";");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::string_str(
                "hello",
            ))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn boolean_expression_false() {
        let lexer = crate::lexer::Lexer::new("false;");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Boolean(
                Boolean { value: false },
            ))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn boolean_expression_true() {
        let lexer = crate::lexer::Lexer::new("true;");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::Boolean(
                Boolean { value: true },
            ))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn integer_literal_expression() {
        let lexer = crate::lexer::Lexer::new("5;");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::ExpressionStatement(Expression::IntegerLiteral(
                IntegerLiteral { value: 5 },
            ))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn array_literal_parsing() {
        let lexer = crate::lexer::Lexer::new("[1, 2 * 2, 3 + 3];");
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::expression(Expression::array(vec![
                Expression::IntegerLiteral(IntegerLiteral { value: 1 }),
                Expression::InfixExpression(InfixExpression {
                    left: Expression::IntegerLiteral(IntegerLiteral { value: 2 }).into(),
                    operator: InfixOperator::Multiply,
                    right: Expression::IntegerLiteral(IntegerLiteral { value: 2 }).into(),
                }),
                Expression::InfixExpression(InfixExpression {
                    left: Expression::IntegerLiteral(IntegerLiteral { value: 3 }).into(),
                    operator: InfixOperator::Plus,
                    right: Expression::IntegerLiteral(IntegerLiteral { value: 3 }).into(),
                }),
            ]))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn hash_map_literal_int_keys_expression_parsing() {
        let lexer = crate::lexer::Lexer::new(r#"{1: "one", 2: "two", 3: "three"}"#);
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::expression(Expression::hash_map(vec![
                (
                    Expression::integer_literal(1),
                    Expression::string_str("one"),
                ),
                (
                    Expression::integer_literal(2),
                    Expression::string_str("two"),
                ),
                (
                    Expression::integer_literal(3),
                    Expression::string_str("three"),
                ),
            ]))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn hash_map_literal_boolean_keys_expression_parsing() {
        let lexer = crate::lexer::Lexer::new(r#"{true: 1, false: 0}"#);
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::expression(Expression::hash_map(vec![
                (Expression::boolean(true), Expression::integer_literal(1)),
                (Expression::boolean(false), Expression::integer_literal(0)),
            ]))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn hash_map_literal_expression_parsing() {
        let lexer = crate::lexer::Lexer::new(r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#);
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::expression(Expression::hash_map(vec![
                (
                    Expression::string_str("one"),
                    Expression::infix_expression(
                        Expression::integer_literal(0),
                        InfixOperator::Plus,
                        Expression::integer_literal(1),
                    ),
                ),
                (
                    Expression::string_str("two"),
                    Expression::infix_expression(
                        Expression::integer_literal(10),
                        InfixOperator::Minus,
                        Expression::integer_literal(8),
                    ),
                ),
                (
                    Expression::string_str("three"),
                    Expression::infix_expression(
                        Expression::integer_literal(15),
                        InfixOperator::Divide,
                        Expression::integer_literal(5),
                    ),
                ),
            ]))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn hash_map_literal_parsing() {
        let lexer = crate::lexer::Lexer::new(r#"{"one": 1, "two": 2, "three": 3};"#);
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse_program().unwrap();
        let expected = Program {
            statements: vec![Statement::expression(Expression::hash_map(vec![
                (
                    Expression::string_str("one"),
                    Expression::integer_literal(1),
                ),
                (
                    Expression::string_str("two"),
                    Expression::integer_literal(2),
                ),
                (
                    Expression::string_str("three"),
                    Expression::integer_literal(3),
                ),
            ]))],
        };
        assert_eq!(parsed, expected);
    }

    #[test]
    fn prefix_expressions_parsing() {
        let parsed: [Program; 2] = ["!5;", "-15"].map(|statement| {
            let lexer = crate::lexer::Lexer::new(statement);
            let mut parser = Parser::new(lexer);
            parser.parse_program().unwrap()
        });
        let expected = [
            prefix_expression(PrefixOperator::Bang, 5),
            prefix_expression(PrefixOperator::Minus, 15),
        ];
        assert_eq!(parsed, expected);
    }

    #[test]
    fn infix_expressions_parsing() {
        let parsed: [Program; 8] = [
            "5 + 5;", "5 - 5", "5 * 5", "5 / 5", "5 > 5", "5 < 5", "5 == 5", "5 != 5",
        ]
        .map(|statement| {
            let lexer = crate::lexer::Lexer::new(statement);
            let mut parser = Parser::new(lexer);
            parser.parse_program().unwrap()
        });
        let expected = [
            infix_expression(5, InfixOperator::Plus, 5),
            infix_expression(5, InfixOperator::Minus, 5),
            infix_expression(5, InfixOperator::Multiply, 5),
            infix_expression(5, InfixOperator::Divide, 5),
            infix_expression(5, InfixOperator::Gt, 5),
            infix_expression(5, InfixOperator::Lt, 5),
            infix_expression(5, InfixOperator::Eq, 5),
            infix_expression(5, InfixOperator::NotEq, 5),
        ];
        assert_eq!(parsed, expected);
    }

    #[test]
    fn return_statements() {
        let parsed: [Program; 3] =
            ["return 5;", "return true;", "return foobar;"].map(|statement| {
                let lexer = crate::lexer::Lexer::new(statement);
                let mut parser = Parser::new(lexer);
                parser.parse_program().unwrap()
            });
        let expected = [
            return_statement(Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
            return_statement(Expression::Boolean(Boolean { value: true })),
            return_statement(Expression::Identifier(Identifier {
                value: "foobar".to_string(),
            })),
        ];
        assert_eq!(parsed, expected);
    }

    #[test]
    fn let_statements() {
        let parsed: [Program; 3] =
            ["let x = 5;", "let y = true;", "let foobar = y;"].map(|statement| {
                let lexer = crate::lexer::Lexer::new(statement);
                let mut parser = Parser::new(lexer);
                parser.parse_program().unwrap()
            });
        let expected = [
            let_statement("x", Expression::IntegerLiteral(IntegerLiteral { value: 5 })),
            let_statement("y", Expression::Boolean(Boolean { value: true })),
            let_statement(
                "foobar",
                Expression::Identifier(Identifier {
                    value: "y".to_string(),
                }),
            ),
        ];
        assert_eq!(parsed, expected);
    }
}
