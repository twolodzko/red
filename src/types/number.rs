use crate::{Error, Result};
use core::f64;
use ordered_float::{OrderedFloat, Pow};
use serde::Serialize;
use std::{
    ops::{Add, Div, Mul, Neg, Rem, Sub},
    str::FromStr,
};

type Int = i64;
type Float = OrderedFloat<f64>;

#[allow(clippy::derived_hash_with_manual_eq)]
#[derive(Debug, Eq, Clone, Copy, Hash)]
pub(crate) enum Number {
    Int(Int),
    Float(Float),
}

impl Number {
    pub(crate) const NAN: Number = Number::Float(OrderedFloat(f64::NAN));
    pub(crate) const ZERO: Number = Number::Int(0);

    pub(crate) fn floor(self) -> Number {
        match self {
            n @ Number::Int(_) => n,
            Number::Float(n) => Number::Int(n.floor() as Int),
        }
    }

    pub(crate) fn as_float(self) -> Float {
        match self {
            Number::Int(n) => (n as f64).into(),
            Number::Float(n) => n,
        }
    }

    pub(crate) fn to_bool(self) -> bool {
        match self {
            Number::Int(n) => n != 0,
            Number::Float(n) => n != 0.0,
        }
    }

    pub(crate) fn abs(self) -> Number {
        match self {
            Number::Int(n) => Number::Int(n.abs()),
            Number::Float(n) => Number::Float(n.abs().into()),
        }
    }

    pub(crate) fn exp(self) -> Number {
        Number::Float(self.as_float().exp().into())
    }

    pub(crate) fn log(self) -> Number {
        Number::Float(self.as_float().ln().into())
    }

    pub(crate) fn log10(self) -> Number {
        Number::Float(self.as_float().log10().into())
    }

    pub(crate) fn sqrt(self) -> Number {
        Number::Float(self.as_float().sqrt().into())
    }

    pub(crate) fn pow(self, rhs: Number) -> Number {
        Number::Float(self.as_float().pow(rhs.as_float()))
    }

    pub(crate) fn is_nan(&self) -> bool {
        match self {
            Number::Int(_) => false,
            Number::Float(n) => n.is_nan(),
        }
    }

    pub(crate) fn is_negative(&self) -> bool {
        match self {
            Number::Int(n) => *n < 0,
            Number::Float(n) => **n < 0.0,
        }
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use Number::*;
        match (self, other) {
            (Float(a), b) => a.cmp(&b.as_float()),
            (a, Float(b)) => a.as_float().cmp(b),
            (Int(a), Int(b)) => a.cmp(b),
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        use Number::*;
        match (self, other) {
            (Float(a), b) => *a == b.as_float(),
            (a, Float(b)) => a.as_float() == *b,
            (Int(a), Int(b)) => a == b,
        }
    }
}

macro_rules! op {
    ($lhs:ident $op:tt $rhs:expr) => {{
        use Number::*;
        match ($lhs, $rhs) {
            (Float(a), b) => Float(a $op b.as_float()),
            (a, Float(b)) => Float(a.as_float() $op b),
            (Int(a), Int(b)) => Int(a $op b),
        }
    }};
}

impl Add for Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        op!(self + rhs)
    }
}

impl Sub for Number {
    type Output = Number;

    fn sub(self, rhs: Self) -> Self::Output {
        op!(self - rhs)
    }
}

impl Mul for Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        op!(self * rhs)
    }
}

impl Div for Number {
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        Number::Float(self.as_float() / rhs.as_float())
    }
}

impl Rem for Number {
    type Output = Number;

    fn rem(self, rhs: Self) -> Self::Output {
        op!(self % rhs)
    }
}

impl Neg for Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match self {
            Number::Int(n) => Number::Int(-n),
            Number::Float(n) => Number::Float(-n),
        }
    }
}

impl FromStr for Number {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let val = if let Ok(val) = s.parse::<Int>() {
            Number::Int(val)
        } else {
            Number::Float(s.parse::<Float>()?)
        };
        Ok(val)
    }
}

impl From<i64> for Number {
    fn from(value: i64) -> Self {
        Number::Int(value)
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Number::Float(value.into())
    }
}

impl From<usize> for Number {
    fn from(value: usize) -> Self {
        Number::Int(value as Int)
    }
}

impl TryFrom<Number> for usize {
    type Error = Error;

    fn try_from(value: Number) -> Result<Self> {
        if value.is_negative() {
            return Err(Error::Custom(format!("{} is negative", value)));
        }
        let val = match value {
            Number::Int(n) => n as usize,
            Number::Float(n) => n.floor() as usize,
        };
        Ok(val)
    }
}

impl From<Number> for f64 {
    fn from(value: Number) -> Self {
        use Number::*;
        match value {
            Int(n) => {
                if n < f64::MIN as i64 {
                    return f64::NAN;
                }
                if n > f64::MAX as i64 {
                    return f64::NAN;
                }
                n as f64
            }
            Float(f) => f.into(),
        }
    }
}

impl From<Number> for i64 {
    fn from(value: Number) -> Self {
        match value {
            Number::Int(n) => n,
            Number::Float(n) => n.floor() as i64,
        }
    }
}

impl From<serde_json::Number> for Number {
    fn from(value: serde_json::Number) -> Self {
        if let Some(val) = value.as_i64() {
            Number::Int(val)
        } else {
            let val = value.as_f64().expect("failed to parse JSON number").into();
            Number::Float(val)
        }
    }
}

impl Serialize for Number {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use Number::*;
        match self {
            Int(v) => v.serialize(serializer),
            Float(v) => f64::from(*v).serialize(serializer),
        }
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Int(n) => n.fmt(f),
            Number::Float(n) => n.fmt(f),
            //     format!("{n:.6}")
            //         .trim_end_matches('0')
            //         .trim_end_matches('.')
            //         .fmt(f),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        eval::{Context, eval},
        parser::Parser,
        tests::WriteMock,
        types::Map,
    };
    use test_case::test_case;

    #[test_case("1 < 2")]
    #[test_case("1.0 < 2")]
    #[test_case("1 < 2.0")]
    #[test_case("1.0 < 2.0")]
    #[test_case("22 >= 11")]
    #[test_case("22.2 >= 11")]
    #[test_case("22 >= 11.1")]
    #[test_case("22.1 >= 11.1")]
    #[test_case("42 == 42")]
    #[test_case("42.0 == 42")]
    #[test_case("42 == 42.0")]
    #[test_case("42.0 == 42.0")]
    fn ord_trait(input: &str) {
        let mut parser = Parser::from(input.to_string());
        let expr = parser.expr().unwrap();
        let mut ctx = Context::new(WriteMock {});
        let data = &mut Map::new();
        let result = eval(&expr, data, &mut ctx).unwrap();
        assert!(result.is_true())
    }
}
