use super::{Type, join};
use crate::{Error, Result};
use indexmap::{IndexMap, map::Iter};
use serde::Serialize;

#[derive(Clone, Serialize, Default, PartialEq)]
pub(crate) struct Map(IndexMap<String, Type>);

impl Map {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn insert(&mut self, key: String, value: Type) {
        self.0.insert(key, value);
    }

    pub(crate) fn get(&self, key: &str) -> Option<&Type> {
        self.0.get(key)
    }

    pub(crate) fn get_rec(&self, keys: &[Type]) -> Result<Option<Type>> {
        let key = &keys[0].as_string()?;
        let rest = &keys[1..];
        let val = match self.0.get(key.as_ref()) {
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
            _ => Err(Error::NotMap),
        }
    }

    pub(crate) fn insert_rec(&mut self, keys: &[Type], value: Type) -> Result<()> {
        let key = &keys[0].as_string()?;
        let rest = &keys[1..];
        let old = match self.0.get_mut(key.as_ref()) {
            Some(old) => old,
            None => {
                if rest.is_empty() {
                    self.0.insert(key.to_string(), value);
                    return Ok(());
                } else {
                    return Err(Error::NotMap);
                }
            }
        };

        if rest.is_empty() {
            *old = value;
        } else {
            match old {
                Type::Map(map) => map.insert_rec(rest, value)?,
                Type::Array(arr) => arr.insert_rec(rest, value)?,
                _ => return Err(Error::NotMap),
            }
        }
        Ok(())
    }

    pub(crate) fn get_mut_rec<'a>(&'a mut self, keys: &[Type]) -> Result<&'a mut Type> {
        let key = &keys[0].as_string()?;
        let rest = &keys[1..];
        let val = match self.0.get_mut(key.as_ref()) {
            Some(val) => val,
            None => return Err(Error::NotMap),
        };
        if rest.is_empty() {
            return Ok(val);
        }
        match val {
            Type::Map(map) => map.get_mut_rec(rest),
            Type::Array(arr) => arr.get_mut_rec(rest),
            _ => Err(Error::NotMap),
        }
    }

    pub(crate) fn get_mut(&mut self, key: &str) -> Option<&mut Type> {
        self.0.get_mut(key)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub(crate) fn to_logfmt(&self) -> Result<String> {
        let val = self
            .0
            .iter()
            .map(|(k, v)| {
                let k = k.replace(|c: char| c.is_whitespace(), "_");
                match v {
                    Type::String(_) | Type::Regex(_) => {
                        let v = v.to_string();
                        if !v.chars().all(|c| !c.is_whitespace()) {
                            return Ok(format!("{}={}", k, quote(&v)));
                        }
                    }
                    Type::Map(map) => {
                        let v = map.to_json()?;
                        return Ok(format!("{}={}", k, quote(&v)));
                    }
                    Type::Array(arr) => {
                        let v = arr.to_string();
                        return Ok(format!("{}={}", k, quote(&v)));
                    }
                    _ => (),
                }
                Ok(format!("{k}={v}"))
            })
            .collect::<Result<Vec<String>>>()?
            .join(" ");
        Ok(val)
    }

    pub(crate) fn from_json(s: &str) -> Result<Self> {
        let json: IndexMap<String, serde_json::Value> = serde_json::from_str(s)?;
        let mut acc = Map::new();
        for (ref k, v) in json {
            acc.insert(k.to_string(), v.into());
        }
        Ok(acc)
    }

    pub(crate) fn to_json(&self) -> Result<String> {
        Ok(serde_json::to_string(self)?)
    }

    pub(crate) fn to_pretty(&self) -> Result<String> {
        Ok(serde_json::to_string_pretty(self)?)
    }

    pub(crate) fn flatten(&self) -> Map {
        let mut acc = Map::new();
        for (k, v) in self.0.iter() {
            if let Type::Map(m) = v
                && !m.is_empty()
            {
                for (k2, v2) in m.flatten().0.iter() {
                    acc.insert(format!("{k}.{k2}"), v2.clone());
                }
            } else {
                acc.insert(k.to_string(), v.clone());
            }
        }
        acc
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn contains_key(&self, key: &str) -> bool {
        self.0.contains_key(key)
    }

    pub(crate) fn iter<'a>(&'a self) -> Iter<'a, String, Type> {
        self.0.iter()
    }

    pub(crate) fn join(&self, other: &Map) -> Self {
        let mut acc = Map::new();
        self.iter()
            .for_each(|(k, v)| acc.insert(k.to_string(), v.clone()));
        other
            .iter()
            .for_each(|(k, v)| acc.insert(k.to_string(), v.clone()));
        acc
    }

    pub(crate) fn reverse(&self) -> Self {
        let mut acc = Map::new();
        for k in self.0.keys().rev() {
            acc.insert(k.to_string(), self.0[k].clone());
        }
        acc
    }

    pub(crate) fn sort(&self) -> Self {
        let mut acc = Map::new();
        let mut keys: Vec<String> = self.0.keys().cloned().collect();
        keys.sort();
        for k in keys {
            let v = self.0[&k].clone();
            acc.insert(k, v);
        }
        acc
    }

    pub(crate) fn keys(&self) -> impl Iterator<Item = &String> {
        self.0.keys()
    }

    pub(crate) fn values(&self) -> impl Iterator<Item = &Type> {
        self.0.values()
    }
}

impl From<serde_json::Map<String, serde_json::Value>> for Map {
    fn from(value: serde_json::Map<String, serde_json::Value>) -> Self {
        let mut acc = IndexMap::new();
        for (k, v) in value {
            acc.insert(k, v.into());
        }
        Map(acc)
    }
}

impl std::hash::Hash for Map {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in self.0.iter() {
            (k, v).hash(state);
        }
    }
}

impl std::fmt::Display for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_json().unwrap())
    }
}

impl std::fmt::Debug for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            join(
                self.0
                    .iter()
                    .map(|(k, v)| { format!("\"{}\": {:?}", k, v) }),
                ", "
            )
        )
    }
}

impl From<serde_json::Value> for Type {
    fn from(value: serde_json::Value) -> Self {
        use serde_json::Value::*;
        match value {
            Null => Type::Null,
            Bool(val) => Type::Bool(val),
            Number(val) => Type::Number(val.into()),
            String(val) => Type::String(val),
            Array(vals) => Type::Array(
                vals.into_iter()
                    .map(Type::from)
                    .collect::<Vec<Type>>()
                    .into(),
            ),
            Object(val) => Type::Map(Map::from(val)),
        }
    }
}

fn quote(line: &str) -> String {
    format!("\"{}\"", line.replace('\\', "\\\\").replace('"', "\\\""))
}

#[cfg(test)]
pub(crate) mod tests {
    use super::Map;

    #[test]
    fn serde() {
        use super::Type;
        let input = r#"{"address":{"city":"London","street":"10 Downing Street"},"age":43,"gender":null,"name":"John Doe","phones":["+44 1234567","+44 2345678"]}"#;
        let json: serde_json::Value = serde_json::from_str(input).unwrap();
        let result = Type::from(json);
        assert_eq!(serde_json::to_string(&result).unwrap(), input)
    }

    #[test]
    fn flatten() {
        let input = r#"{
            "foo": 1,
            "bar": {
                "baz": 2,
                "qux": {},
                "fiz": [1,2,3]
            }
        }"#;
        // strangely the order changes here
        let expected = r#"{"foo":1,"bar.baz":2,"bar.fiz":[1,2,3],"bar.qux":{}}"#;
        let map = Map::from_json(input).unwrap();
        let result = map.flatten().to_json().unwrap();
        assert_eq!(result, expected)
    }
}
