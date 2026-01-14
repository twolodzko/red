pub mod buildins;
mod csv;
mod eval;
mod logfmt;
pub mod parser;
pub mod program;
pub mod reader;
mod types;

pub use crate::types::Program;
use crate::types::{Action, Type};

#[cfg(test)]
mod tests;

fn unescape(s: &str) -> String {
    unescape::unescape(s).unwrap_or(s.to_string())
}

fn join<I>(args: I, sep: &str) -> String
where
    I: IntoIterator,
    I::Item: ToString,
{
    args.into_iter()
        .map(|elem| elem.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    // Parsing errors
    EndOfInput,
    Missing(char),
    WrongChar(char),
    WrongToken(parser::tokenizer::Token),
    ConstantExpr(Type),
    // Inherited errors
    Io(std::io::Error),
    ParseFloat(std::num::ParseFloatError),
    ParseInt(std::num::ParseIntError),
    Regex(regex::Error),
    SerdeJson(serde_json::Error),
    // Evaluation errors
    Custom(String),
    IndexError,
    NotArray,
    NotMap,
    NotNumber,
    NotRegex,
    NotString,
    #[allow(private_interfaces)]
    UnknownVar(crate::types::Variable),
    WrongArgumentsNumber,
    WrongType,
    // Control flow
    #[allow(private_interfaces)]
    Action(Action),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;
        match self {
            // Parsing errors
            EndOfInput => write!(f, "unexpected end of input"),
            Missing(c) => write!(f, "missing character: '{}'", c),
            WrongChar(c) => write!(f, "unexpected character: '{}'", c),
            WrongToken(t) => write!(f, "unexpected: '{}'", t),
            ConstantExpr(v) => write!(f, "constant expression '{}' is a meaningless assertion", v),
            // Inherited errors
            Io(e) => e.fmt(f),
            ParseFloat(e) => e.fmt(f),
            ParseInt(e) => e.fmt(f),
            Regex(e) => e.fmt(f),
            SerdeJson(e) => e.fmt(f),
            // Evaluation errors
            Custom(msg) => write!(f, "{}", msg),
            IndexError => write!(f, "index out of range"),
            NotArray => write!(f, "not an array"),
            NotMap => write!(f, "not a map"),
            NotNumber => write!(f, "not a number"),
            NotRegex => write!(f, "not a regular expression"),
            NotString => write!(f, "not a string"),
            UnknownVar(v) => write!(f, "unknown variable: {}", v),
            WrongArgumentsNumber => write!(f, "incorrect number of arguments"),
            WrongType => write!(f, "incorrect argument type"),
            // Control flow
            Action(a) => write!(f, "invalid usage of '{}'", a),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Error::Io(value)
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(value: std::num::ParseIntError) -> Self {
        Error::ParseInt(value)
    }
}

impl From<regex::Error> for Error {
    fn from(value: regex::Error) -> Self {
        Error::Regex(value)
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(value: std::num::ParseFloatError) -> Self {
        Error::ParseFloat(value)
    }
}

impl From<serde_json::Error> for Error {
    fn from(value: serde_json::Error) -> Self {
        Error::SerdeJson(value)
    }
}
