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

pub(crate) fn wrapped_usize(value: &Type, bound: usize) -> crate::Result<usize> {
    if bound == 0 {
        return Ok(0);
    }
    let num = Number::try_from(value)?;
    if num.is_negative() {
        let num = usize::try_from(num.abs())?;
        Ok(bound.wrapping_sub(num))
    } else {
        Ok(usize::try_from(num)?)
    }
}
