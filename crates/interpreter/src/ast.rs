use std::{
    fmt::{Display, Write},
    rc::Rc,
};

use anyhow::anyhow;

use crate::token::Token;

#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer: String = String::with_capacity(1024);
        for s in self.statements.iter() {
            buffer.write_str(&s.to_string())?;
        }
        write!(f, "{}", buffer)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(Expression),
}
impl Statement {
    pub fn let_statement(name: Identifier, expression: Expression) -> Statement {
        Statement::LetStatement(LetStatement {
            name,
            value: expression,
        })
    }
    pub fn expression(expression: Expression) -> Statement {
        Statement::ExpressionStatement(expression)
    }
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(inner) => write!(f, "{}", inner),
            Statement::ReturnStatement(inner) => write!(f, "{}", inner),
            Statement::ExpressionStatement(inner) => write!(f, "{}", inner),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Callee {
    FunctionLiteral(FunctionLiteral),
    Identifier(Identifier),
}
impl Callee {
    pub fn identifier_str(name: &str) -> Callee {
        Callee::Identifier(Identifier::from_str(name))
    }
    pub fn identifier(identifier: Identifier) -> Callee {
        Callee::Identifier(identifier)
    }
    pub fn function_literal(function_literal: FunctionLiteral) -> Callee {
        Callee::FunctionLiteral(function_literal)
    }
}
impl Display for Callee {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Callee::FunctionLiteral(function_literal) => {
                write!(f, "{}", function_literal)
            }
            Callee::Identifier(identifier) => write!(f, "{}", identifier),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CallExpression {
    pub function: Callee,
    pub arguments: Vec<Expression>,
}
impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            &self.function,
            &self
                .arguments
                .iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Rc<Expression>,
    pub consequence: BlockStatement,
    pub alternative: BlockStatement,
}
impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "if {} {} else {}",
            &self.condition, &self.consequence, &self.alternative,
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Identifier(Identifier),
    Boolean(Boolean),
    IntegerLiteral(IntegerLiteral),
    ArrayLiteral(ArrayLiteral),
    HashLiteral(HashLiteral),
    IndexExpression(IndexExpression),
    StringLiteral(StringLiteral),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    IfExpression(IfExpression),
}
impl Expression {
    // todo: Is this not a postfix?
    pub fn index_expression(left: Expression, right: Expression) -> Expression {
        Expression::IndexExpression(IndexExpression {
            left: left.into(),
            right: right.into(),
        })
    }

    pub fn hash_map(pairs: Vec<(Expression, Expression)>) -> Expression {
        Expression::HashLiteral(HashLiteral { pairs })
    }

    pub fn array(elements: Vec<Expression>) -> Expression {
        Expression::ArrayLiteral(ArrayLiteral { elements })
    }

    pub fn identifier(name: &str) -> Expression {
        Expression::Identifier(Identifier {
            value: name.to_string(),
        })
    }

    pub fn string(value: String) -> Expression {
        Expression::StringLiteral(StringLiteral { value })
    }

    pub fn string_str(value: &str) -> Expression {
        Expression::StringLiteral(StringLiteral {
            value: value.to_string(),
        })
    }

    pub fn boolean(value: bool) -> Expression {
        Expression::Boolean(Boolean { value })
    }

    pub fn call(callee: Callee, arguments: Vec<Expression>) -> Expression {
        Expression::CallExpression(CallExpression {
            function: callee,
            arguments,
        })
    }

    pub fn prefix_expression(op: PrefixOperator, right: Expression) -> Expression {
        Expression::PrefixExpression(PrefixExpression {
            operator: op,
            right: right.into(),
        })
    }

    pub fn infix_expression(left: Expression, op: InfixOperator, right: Expression) -> Expression {
        Expression::InfixExpression(InfixExpression {
            left: left.into(),
            operator: op,
            right: right.into(),
        })
    }

    pub fn integer_literal(value: i64) -> Expression {
        Expression::IntegerLiteral(IntegerLiteral { value })
    }
}
impl TryInto<Callee> for Expression {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<Callee, Self::Error> {
        match self {
            Expression::Identifier(identifier) => Ok(Callee::Identifier(identifier)),
            Expression::FunctionLiteral(function_literal) => {
                Ok(Callee::FunctionLiteral(function_literal))
            }
            expr => Err(anyhow!("Could not convert {} into a function call", expr)),
        }
    }
}
impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(literal) => write!(f, "{}", literal.to_string()),
            Expression::Boolean(literal) => write!(f, "{}", literal.to_string()),
            Expression::IntegerLiteral(literal) => write!(f, "{}", literal.to_string()),
            Expression::StringLiteral(literal) => write!(f, "{}", literal.to_string()),
            Expression::ArrayLiteral(literal) => write!(f, "{}", literal.to_string()),
            Expression::HashLiteral(literal) => write!(f, "{}", literal.to_string()),
            Expression::IndexExpression(literal) => write!(f, "{}", literal.to_string()),
            Expression::PrefixExpression(expr) => write!(f, "{}", expr.to_string()),
            Expression::InfixExpression(expr) => write!(f, "{}", expr.to_string()),
            Expression::FunctionLiteral(expr) => write!(f, "{}", expr),
            Expression::CallExpression(expr) => write!(f, "{}", expr),
            Expression::IfExpression(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}
impl BlockStatement {
    pub fn new(statements: Vec<Statement>) -> BlockStatement {
        BlockStatement { statements }
    }
    pub fn single(statement: Statement) -> BlockStatement {
        BlockStatement {
            statements: vec![statement],
        }
    }
    pub fn empty() -> BlockStatement {
        BlockStatement {
            statements: Vec::new(),
        }
    }
}
impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer: String = String::with_capacity(1024);
        for s in self.statements.iter() {
            buffer.write_str(&s.to_string())?;
        }
        write!(f, "{{ {} }}", buffer)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PrefixOperator {
    Minus,
    Bang,
}
impl Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOperator::Minus => write!(f, "-"),
            PrefixOperator::Bang => write!(f, "!"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IndexExpression {
    pub left: Rc<Expression>,
    pub right: Rc<Expression>,
}
impl Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.right)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: PrefixOperator,
    pub right: Rc<Expression>,
}
impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum InfixOperator {
    Minus,
    Plus,
    Multiply,
    Divide,
    Gt,
    Lt,
    Eq,
    NotEq,
}
impl TryFrom<Token> for InfixOperator {
    type Error = anyhow::Error;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Plus => Ok(InfixOperator::Plus),
            Token::Minus => Ok(InfixOperator::Minus),
            Token::Asterisk => Ok(InfixOperator::Multiply),
            Token::Slash => Ok(InfixOperator::Divide),
            Token::Lt => Ok(InfixOperator::Lt),
            Token::Gt => Ok(InfixOperator::Gt),
            Token::Eq => Ok(InfixOperator::Eq),
            Token::NotEq => Ok(InfixOperator::NotEq),
            token => Err(anyhow!(
                "{} ({:?}) is not a valid infix operator",
                token,
                &token
            )),
        }
    }
}
impl Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Maths
            InfixOperator::Minus => write!(f, "-"),
            InfixOperator::Plus => write!(f, "+"),
            InfixOperator::Multiply => write!(f, "*"),
            InfixOperator::Divide => write!(f, "/"),
            // Boolean
            InfixOperator::Gt => write!(f, ">"),
            InfixOperator::Lt => write!(f, "<"),
            InfixOperator::Eq => write!(f, "=="),
            InfixOperator::NotEq => write!(f, "!="),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct InfixExpression {
    pub left: Rc<Expression>,
    pub operator: InfixOperator,
    pub right: Rc<Expression>,
}
impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
}
impl Identifier {
    pub fn from_str(str: &str) -> Self {
        Identifier {
            value: str.to_string(),
        }
    }
}
impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ReturnStatement {
    pub return_value: Expression,
}
impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.return_value)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Boolean {
    pub value: bool,
}
impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct HashLiteral {
    pub pairs: Vec<(Expression, Expression)>,
}
impl Display for HashLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.pairs
                .iter()
                .map(|(key, value)| format!("{}: {}", key, value))
                .collect::<Vec<_>>()
                .join(", "),
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}
impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.elements
                .iter()
                .map(|ident| ident.to_string())
                .collect::<Vec<_>>()
                .join(", "),
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionLiteral {
    pub paramaters: Vec<Identifier>,
    pub body: BlockStatement,
}
impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}) {}",
            self.paramaters
                .iter()
                .map(|ident| ident.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            self.body
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StringLiteral {
    pub value: String,
}
impl Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IntegerLiteral {
    pub value: i64,
}
impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn let_statement_can_be_displayed() {
        let statement = LetStatement {
            name: Identifier {
                value: "myVar".to_string(),
            },
            value: Expression::Identifier(Identifier {
                value: "anotherVar".to_string(),
            }),
        };
        assert_eq!(statement.to_string(), "let myVar = anotherVar;")
    }
}
