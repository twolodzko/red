mod array;
mod ast;
mod data;
mod map;
mod number;
mod operator;
mod regex;
mod template;

pub(crate) use array::Array;
pub use ast::Program;
pub(crate) use ast::{Action, AtomicBool, Block, Expr, Function, Match, Print, Variable};
pub(crate) use data::{Count, Type};
pub(crate) use map::Map;
pub(crate) use number::Number;
pub(crate) use operator::Operator;
pub(crate) use regex::Regex;
pub(crate) use template::Template;

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
