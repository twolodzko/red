use super::{Type, join};
use crate::{Error, Result};
use core::slice::Iter;
use serde::Serialize;

#[derive(Clone, PartialEq, Eq, PartialOrd, Hash, Default)]
pub(crate) struct Array(Vec<Type>);

impl Array {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn push(&mut self, value: Type) {
        self.0.push(value);
    }

    pub(crate) fn iter<'a>(&'a self) -> Iter<'a, Type> {
        self.0.iter()
    }

    pub(crate) fn get_rec(&self, keys: &[Type]) -> Result<Option<Type>> {
        let index = usize::try_from(&keys[0])?;
        let rest = &keys[1..];
        let val = match self.0.get(index) {
            Some(val) => val,
            None => return Ok(None),
        };

        if rest.is_empty() {
            return Ok(Some(val.clone()));
        }

        if rest.len() == 1
            && let Type::String(s) = val
        {
            let index = usize::try_from(&rest[0])?;
            return match s.chars().nth(index) {
                Some(c) => Ok(Some(Type::String(c.to_string()))),
                None => Ok(None),
            };
        }

        match val {
            Type::Map(map) => map.get_rec(rest),
            Type::Array(arr) => arr.get_rec(rest),
            _ => Err(Error::NotArray),
        }
    }

    pub(crate) fn insert_rec(&mut self, keys: &[Type], value: Type) -> Result<()> {
        let index = usize::try_from(&keys[0])?;
        let rest = &keys[1..];
        let old = match self.0.get_mut(index) {
            Some(old) => old,
            None => {
                if index == self.0.len() {
                    self.push(value);
                    return Ok(());
                } else {
                    return Err(Error::IndexError);
                }
            }
        };
        if rest.is_empty() {
            *old = value;
        } else {
            match old {
                Type::Map(map) => map.insert_rec(rest, value)?,
                Type::Array(arr) => arr.insert_rec(rest, value)?,
                _ => return Err(Error::NotArray),
            }
        }
        Ok(())
    }

    pub(crate) fn get_mut_rec<'a>(&'a mut self, keys: &[Type]) -> Result<&'a mut Type> {
        let index = usize::try_from(&keys[0])?;
        let rest = &keys[1..];
        let val = match self.0.get_mut(index) {
            Some(val) => val,
            None => return Err(Error::NotArray),
        };
        if rest.is_empty() {
            return Ok(val);
        }
        match val {
            Type::Map(map) => map.get_mut_rec(rest),
            Type::Array(arr) => arr.get_mut_rec(rest),
            _ => Err(Error::NotArray),
        }
    }

    pub(crate) fn contains(&self, value: &Type) -> bool {
        self.0.contains(value)
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn join(&self, other: &Array) -> Self {
        let mut acc = Array::new();
        self.iter().for_each(|v| acc.push(v.clone()));
        other.iter().for_each(|v| acc.push(v.clone()));
        acc
    }

    pub(crate) fn slice(&self, start: usize, stop: usize) -> Option<Array> {
        Some(self.0.get(start..stop)?.to_vec().into())
    }

    pub(crate) fn reverse(&self) -> Array {
        Array::from(self.0.iter().cloned().rev().collect::<Vec<Type>>())
    }

    pub(crate) fn sort(&self) -> Array {
        let mut arr = self.0.clone();
        arr.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        Array(arr)
    }

    pub(crate) fn flatten(&self) -> Array {
        self.iter()
            .flat_map(flatten_array)
            .collect::<Vec<Type>>()
            .into()
    }

    fn to_json(&self) -> Result<String> {
        Ok(serde_json::to_string(self)?)
    }
}

fn flatten_array(val: &Type) -> Vec<Type> {
    match val {
        Type::Array(arr) => arr.iter().flat_map(flatten_array).collect(),
        other => vec![other.clone()],
    }
}

impl std::fmt::Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_json().unwrap())
    }
}

impl std::fmt::Debug for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            join(self.iter().map(|v| format!("{:?}", v)), ", ")
        )
    }
}

impl Serialize for Array {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl From<Vec<Type>> for Array {
    fn from(value: Vec<Type>) -> Self {
        Array(value)
    }
}
