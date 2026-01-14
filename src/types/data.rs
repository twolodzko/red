use super::{Array, Map, Number, Regex};
use crate::{Error, Result};
use serde::Serialize;
use std::{borrow::Cow, collections::HashMap, hash::Hash, str::FromStr};

#[allow(private_interfaces)]
#[allow(clippy::derived_hash_with_manual_eq)]
#[derive(Clone, Hash)]
pub enum Type {
    Any,
    Null,
    String(String),
    Regex(Regex),
    Bool(bool),
    Number(Number),
    Array(Array),
    Map(Map),
    Count(Count),
}

impl Type {
    pub(crate) const TRUE: Type = Type::Bool(true);
    pub(crate) const FALSE: Type = Type::Bool(false);

    pub(crate) fn is_true(&self) -> bool {
        *self != Type::FALSE
    }

    pub(crate) fn join(&self, other: &Type) -> Result<Type> {
        match (self, other) {
            // Null and any Are don't join
            (other, Type::Any | Type::Null) | (Type::Any | Type::Null, other) => Ok(other.clone()),
            // standard
            (Type::Array(lhs), Type::Array(rhs)) => Ok(Type::Array(lhs.join(rhs))),
            (Type::Map(lhs), Type::Map(rhs)) => Ok(Type::Map(lhs.join(rhs))),
            (Type::Regex(lhs), Type::Regex(rhs)) => {
                let s = format!("{}{}", lhs, rhs);
                let r = Regex::from_str(&s)?;
                Ok(Type::Regex(r))
            }
            // otherwise cast to strings
            (lhs, rhs) => Ok(Type::String(format!(
                "{}{}",
                lhs.as_string()?,
                rhs.as_string()?
            ))),
        }
    }

    pub(crate) fn as_string<'a>(&'a self) -> Result<Cow<'a, str>> {
        let s = match self {
            Type::String(s) => return Ok(Cow::Borrowed(s)),
            Type::Regex(r) => r.to_string(),
            Type::Number(n) => n.to_string(),
            Type::Bool(b) => b.to_string(),
            Type::Null => String::new(),
            _ => return Err(Error::WrongType),
        };
        Ok(Cow::Owned(s))
    }

    pub(crate) fn as_regex<'a>(&'a self) -> Result<Cow<'a, Regex>> {
        match self {
            Type::Regex(regex) => Ok(Cow::Borrowed(regex)),
            Type::String(s) => Ok(Cow::Owned(Regex::from_str(s)?)),
            _ => Err(Error::NotRegex),
        }
    }
}

impl std::ops::Not for Type {
    type Output = Type;

    fn not(self) -> Self::Output {
        match self {
            Type::Bool(false) => Type::TRUE,
            _ => Type::FALSE,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Any => write!(f, "_"),
            Null => write!(f, "null"),
            Bool(v) => write!(f, "{}", v),
            Number(v) => write!(f, "{}", v),
            String(s) => write!(f, "{}", s),
            Regex(r) => write!(f, "{}", r),
            Array(v) => write!(f, "{}", v),
            Map(v) => write!(f, "{}", v),
            Count(a) => a.materialize().fmt(f),
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::String(s) => write!(f, "\"{}\"", s),
            Type::Regex(s) => write!(f, "/{}/", s),
            Type::Array(a) => write!(f, "{:?}", a),
            Type::Map(m) => write!(f, "{:?}", m),
            other => write!(f, "{}", other),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        let (lhs, rhs) = unify_to_compare(self, other);
        match (lhs.as_ref(), rhs.as_ref()) {
            (Any, _) | (_, Any) => true,
            (Null, Null) => true,
            (Number(lhs), Number(rhs)) => lhs == rhs,
            (String(lhs), String(rhs)) => lhs == rhs,
            (Bool(lhs), Bool(rhs)) => lhs == rhs,
            (Regex(lhs), Regex(rhs)) => lhs == rhs,
            (Array(lhs), Array(rhs)) => lhs == rhs,
            (Map(lhs), Map(rhs)) => lhs == rhs,
            (_, _) => false,
        }
    }
}

impl Eq for Type {}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Type::*;
        use std::cmp::Ordering::*;
        let (lhs, rhs) = unify_to_compare(self, other);
        match (lhs.as_ref(), rhs.as_ref()) {
            (Any, _) | (_, Any) => Some(Equal),
            (Null, Null) => Some(Equal),
            (Number(lhs), Number(rhs)) => lhs.partial_cmp(rhs),
            (String(lhs), String(rhs)) => lhs.partial_cmp(rhs),
            (Regex(lhs), Regex(rhs)) => lhs.partial_cmp(rhs),
            (Array(lhs), Array(rhs)) => lhs.partial_cmp(rhs),
            (_, _) => None,
        }
    }
}

impl From<bool> for Type {
    fn from(value: bool) -> Self {
        Type::Bool(value)
    }
}

impl From<String> for Type {
    fn from(value: String) -> Self {
        match value.as_str() {
            "null" => Type::Null,
            "true" => Type::TRUE,
            "false" => Type::FALSE,
            s if s.starts_with(|c: char| c == '-' || c == '+' || c.is_numeric()) => {
                Number::from_str(s)
                    .map(Type::Number)
                    .unwrap_or(Type::String(value))
            }
            _ => Type::String(value),
        }
    }
}

impl From<Regex> for Type {
    fn from(value: Regex) -> Self {
        Type::Regex(value)
    }
}

impl From<usize> for Type {
    fn from(value: usize) -> Self {
        Type::Number(Number::Int(value as i64))
    }
}

impl TryFrom<&Type> for usize {
    type Error = Error;

    fn try_from(value: &Type) -> Result<Self> {
        let num = Number::try_from(value)?;
        usize::try_from(num)
    }
}

impl TryFrom<Type> for f64 {
    type Error = Error;

    fn try_from(value: Type) -> Result<Self> {
        let num = Number::try_from(&value)?;
        Ok(f64::from(num))
    }
}

impl TryFrom<&Type> for Number {
    type Error = Error;

    fn try_from(value: &Type) -> std::result::Result<Self, Self::Error> {
        match value {
            Type::Number(n) => Ok(*n),
            Type::String(s) => Ok(Number::from_str(s)?),
            Type::Regex(r) => Ok(Number::from_str(&r.to_string())?),
            _ => Err(Error::Custom(format!("{} is not a number", value))),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::String(String::default())
    }
}

impl Serialize for Type {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use Type::*;
        match self {
            Any => serializer.serialize_str("_"),
            Null => serializer.serialize_none(),
            String(s) => serializer.serialize_str(s),
            Regex(r) => serializer.serialize_str(&r.to_string()),
            Bool(b) => serializer.serialize_bool(*b),
            Number(n) => n.serialize(serializer),
            Array(v) => v.serialize(serializer),
            Map(m) => m.serialize(serializer),
            Count(a) => a.materialize().serialize(serializer),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Count(pub(crate) HashMap<String, usize>);

impl Count {
    pub(crate) fn new() -> Self {
        Count(HashMap::new())
    }

    fn materialize(&self) -> Type {
        let mut acc = Map::new();
        for (k, v) in self.0.iter() {
            acc.insert(k.to_string(), Type::Number(Number::from(*v)));
        }
        Type::Map(acc)
    }
}

impl std::hash::Hash for Count {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.materialize().hash(state);
    }
}

fn unify_to_compare<'a>(lhs: &'a Type, rhs: &'a Type) -> (Cow<'a, Type>, Cow<'a, Type>) {
    match (lhs, rhs) {
        (Type::Any, _) | (_, Type::Any) => (Cow::Borrowed(lhs), Cow::Borrowed(rhs)),
        (Type::Number(_), val) => {
            let rhs = Number::try_from(val)
                .map_or(Cow::Borrowed(rhs), |val| Cow::Owned(Type::Number(val)));
            (Cow::Borrowed(lhs), rhs)
        }
        (val, Type::Number(_)) => {
            let lhs = Number::try_from(val)
                .map_or(Cow::Borrowed(lhs), |val| Cow::Owned(Type::Number(val)));
            (lhs, Cow::Borrowed(rhs))
        }
        (Type::String(_), rhs) => (
            Cow::Borrowed(lhs),
            Cow::Owned(Type::String(rhs.to_string())),
        ),
        (lhs, Type::String(_)) => (
            Cow::Owned(Type::String(lhs.to_string())),
            Cow::Borrowed(rhs),
        ),
        _ => (Cow::Borrowed(lhs), Cow::Borrowed(rhs)),
    }
}
