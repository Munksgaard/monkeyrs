use crate::ast::{Identifier, Statement};
use crate::env::Environment;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
    Null,
    Function(Vec<Identifier>, Environment, Vec<Statement>),
    String(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Null => write!(f, "null"),
            Object::Function(_, _, _) => write!(f, "<function>"),
            Object::String(s) => write!(f, "{:?}", s),
        }
    }
}
