use anyhow::*;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash as StdHash;

use crate::ast;

use crate::eval::{Environment, System};

// Todo: Wouldn't it be neat to not use object wrapper
#[derive(Eq, PartialEq, Clone)]
pub enum Object {
    Int(Integer),
    Bool(Boolean),
    String(String),
    Array(Array),
    Hash(Hash),
    Fn(Function),
    BuiltinFn(BuiltinFunction),
    Return(Box<Object>),
    Null,
}

impl Object {
    pub fn string(value: std::string::String) -> Object {
        Object::String(String { value })
    }

    pub fn str(value: &str) -> Object {
        Object::String(String {
            value: value.to_string(),
        })
    }

    pub fn array(elements: Vec<Object>) -> Object {
        Object::Array(Array { elements })
    }

    pub fn hash_map(pairs: HashMap<HashObject, Object>) -> Object {
        let hash_map = pairs.into_iter().collect::<HashMap<_, _>>();
        Object::Hash(Hash { pairs: hash_map })
    }

    pub fn hash(pairs: Vec<(HashObject, Object)>) -> Object {
        let hash_map = pairs.into_iter().collect::<HashMap<_, _>>();
        Object::Hash(Hash { pairs: hash_map })
    }

    pub fn int(value: i64) -> Object {
        Object::Int(Integer { value })
    }

    pub fn func_with_env(
        parameters: Vec<ast::Identifier>,
        body: ast::BlockStatement,
        env: Environment,
    ) -> Object {
        Object::Fn(Function {
            parameters,
            body,
            env,
        })
    }

    pub fn builtin(
        name: std::string::String,
        func: fn(Vec<Object>, &mut dyn System) -> anyhow::Result<Object>,
    ) -> Object {
        Object::BuiltinFn(BuiltinFunction { name, func })
    }

    pub fn func(parameters: Vec<ast::Identifier>, body: ast::BlockStatement) -> Object {
        let env = Environment::new();
        Object::Fn(Function {
            parameters,
            body,
            env,
        })
    }

    pub fn func_str(args: Vec<&str>, body: ast::BlockStatement) -> Object {
        let env = Environment::new();
        let args_identifiers: Vec<_> = args
            .iter()
            .map(|name| ast::Identifier::from_str(name))
            .collect();
        Object::Fn(Function {
            parameters: args_identifiers,
            body,
            env,
        })
    }

    pub fn bool(value: bool) -> Object {
        if value {
            Object::Bool(Boolean::True)
        } else {
            Object::Bool(Boolean::False)
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(value) => write!(f, "{}::<INTEGER>", value),
            Object::Bool(bool) => write!(f, "{}::<BOOLEAN>", bool),
            Object::String(value) => write!(f, "{}::<STRING>", value),
            Object::Null => write!(f, "NULL"),
            Object::Fn(func) => write!(f, "{func}::<FUNCTION>"),
            Object::BuiltinFn(func) => write!(f, "{func}::<BUILTIN>"),
            Object::Array(array) => write!(f, "{array}::<ARRAY>"),
            Object::Hash(hash) => write!(f, "{hash}::<HASH>"),
            Object::Return(_) => unreachable!(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(value) => write!(f, "{}", value),
            Object::Bool(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Return(value) => write!(f, "{}", value),
            Object::Fn(func) => write!(f, "{}", func),
            Object::BuiltinFn(func) => write!(f, "{}", func),
            Object::Array(array) => write!(f, "{}", array),
            Object::Hash(hash) => write!(f, "{}", hash),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, StdHash)]
pub enum HashObject {
    Int(Integer),
    Bool(Boolean),
    String(String),
}

impl Into<Object> for HashObject {
    fn into(self) -> Object {
        match self {
            HashObject::Int(value) => Object::Int(value),
            HashObject::Bool(value) => Object::Bool(value),
            HashObject::String(value) => Object::String(value),
        }
    }
}

impl TryFrom<Object> for HashObject {
    type Error = anyhow::Error;

    fn try_from(value: Object) -> std::prelude::v1::Result<Self, Self::Error> {
        match value {
            Object::Int(value) => Ok(HashObject::Int(value)),
            Object::Bool(value) => Ok(HashObject::Bool(value)),
            Object::String(value) => Ok(HashObject::String(value)),
            obj => Err(anyhow!("Cannot use object {} as a key", obj)),
        }
    }
}

impl HashObject {
    pub fn string(value: std::string::String) -> Self {
        HashObject::String(String { value })
    }

    pub fn str(value: &str) -> Self {
        HashObject::String(String {
            value: value.to_string(),
        })
    }

    pub fn int(value: i64) -> Self {
        HashObject::Int(Integer { value })
    }

    pub fn bool(value: bool) -> Self {
        HashObject::Bool(value.into())
    }
}

impl Display for HashObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HashObject::Int(value) => write!(f, "{}", value),
            HashObject::Bool(value) => write!(f, "{}", value),
            HashObject::String(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Hash {
    pub pairs: HashMap<HashObject, Object>,
}
impl Display for Hash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Hash { pairs } => write!(
                f,
                "{{{}}}",
                pairs
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Array {
    pub elements: Vec<Object>,
}
impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Array { elements } => write!(
                f,
                "[{}]",
                elements
                    .iter()
                    .map(|el| el.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, StdHash)]
pub struct String {
    pub value: std::string::String,
}
impl Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            String { value } => write!(f, "\"{}\"", value),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, StdHash)]
pub struct Integer {
    pub value: i64,
}
impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Integer { value } => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BuiltinFunction {
    pub name: std::string::String,
    pub func: fn(Vec<Object>, &mut dyn System) -> anyhow::Result<Object>,
}
impl Display for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}()", self.name)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<ast::Identifier>,
    pub body: ast::BlockStatement,
    pub env: Environment,
}
impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}) {{ {} }}",
            self.parameters
                .iter()
                .map(|param| param.to_string())
                .collect::<Vec<_>>()
                .join(","),
            self.body
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone, StdHash)]
pub enum Boolean {
    True,
    False,
}
impl Into<bool> for Boolean {
    fn into(self) -> bool {
        match self {
            Boolean::True => true,
            Boolean::False => false,
        }
    }
}
impl From<bool> for Boolean {
    fn from(value: bool) -> Self {
        match value {
            true => Boolean::True,
            false => Boolean::False,
        }
    }
}
impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
        }
    }
}
