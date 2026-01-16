use crate::{
    Error, Result,
    eval::Context,
    types::{Array, Count, Map, Number, Type},
};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    io::Write,
};

pub(crate) type Procedure = fn(&[Type]) -> Result<Type>;

pub const PROCEDURES: &[(&str, Procedure, &str, &str)] = &[
    // Type checks
    ("isarr", is_array, "value", "check if argument is an array"),
    ("isbool", is_bool, "value", "check if argument is a boolean"),
    ("ismap", is_map, "value", "check if argument is a map"),
    (
        "isnan",
        is_nan,
        "value",
        "argument cannot be parsed to a number or is a NaN",
    ),
    ("isnum", is_num, "value", "check if argument is a number"),
    ("isstr", is_string, "value", "check if argument is a string"),
    // String operations
    (
        "escape",
        escape,
        "string",
        "escape special characters in a string",
    ),
    ("lower", lower, "string", "convert string to lowercase"),
    (
        "re",
        regex_extract,
        "string, regex",
        "extract substrings from a string to an array given a regular expression",
    ),
    (
        "replace",
        replace,
        "string, from, to",
        "replace elements in a string equal to the given value, to the replacement value",
    ),
    (
        "split",
        split,
        "string, [separator]",
        "split string by the separator to an array",
    ),
    (
        "sub",
        sub,
        "string, regex, to, [limit]",
        "substitute elements in a string matching the regular expressions, with the replacement value",
    ),
    ("trim", trim, "string", "trim the whitespaces from a string"),
    ("unescape", unescape, "string", "unescape the string"),
    (
        "unprefix",
        remove_prefix,
        "string, prefix",
        "remove the prefix",
    ),
    (
        "unsuffix",
        remove_suffix,
        "string, suffix",
        "remove the suffix",
    ),
    ("upper", upper, "string", "convert string to uppercase"),
    // Collection's operations
    (
        "flatten",
        flatten,
        "collection",
        "flatten an array or a map",
    ),
    (
        "len",
        len,
        "string|collection",
        "length of a string, array, or a map",
    ),
    (
        "rev",
        rev,
        "string|collection",
        "reverse a string, array, or a map",
    ),
    (
        "sort",
        sort,
        "string|collection",
        "sort a string, array, or a map",
    ),
    (
        "join",
        join,
        "array, [string]",
        "convert array to string, joining with separator",
    ),
    (
        "unique",
        unique,
        "string|collection",
        "unique elements of a string, array, or a map",
    ),
    // Math
    ("abs", abs, "number", "absolute value of a number"),
    ("exp", exp, "number", "exponentiate a number"),
    ("floor", floor, "number", "floor of a number"),
    ("log", log, "number", "natural logarithm of a number"),
    ("log10", log10, "number", "base 10 logarithm of a number"),
    ("max", max, "number", "bigger of two values"),
    ("min", min, "number", "smaller of two values"),
    ("round", round, "number", "round a number"),
    ("sqrt", sqrt, "number", "square root of a number"),
    // Parsers & conversions
    ("bool", to_bool, "value", "convert value to a boolean"),
    ("json", from_json, "string", "parse string as a JSON"),
    (
        "keys",
        keys,
        "map",
        "return keys of a map as an array of their names",
    ),
    ("num", to_num, "value", "convert value to a number"),
    ("str", to_str, "value", "convert value to a string"),
    ("vals", vals, "map", "return values of a map as an array"),
    // Side effects
    (
        "error",
        error,
        "string",
        "throw an error with the given message",
    ),
    // Aggregator initializers
    ("count", count, "", "initialize the count aggregator"),
];

impl<O: Write> Context<O> {
    pub(crate) fn new(out: O) -> Self {
        let mut ctx = Context {
            index: 0,
            line: String::new(),
            done: false,
            buildins: HashMap::new(),
            funs: HashMap::new(),
            globals: Map::new(),
            print_matched: false,
            out,
        };
        for (k, v, _, _) in PROCEDURES {
            ctx.buildins.insert(k.to_string(), *v);
        }
        let args: Vec<Type> = std::env::args()
            .map(|a| Type::String(a.to_string()))
            .collect();
        ctx.globals
            .insert("ARGV".to_string(), Type::Array(Array::from(args)));
        ctx
    }
}

// Type checks

fn is_nan(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    if let Ok(num) = Number::try_from(&args[0]) {
        Ok(num.is_nan().into())
    } else {
        Ok(false.into())
    }
}

fn is_string(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    Ok(matches!(args[0], Type::String(_) | Type::Regex(_)).into())
}

fn is_bool(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    Ok(matches!(args[0], Type::Bool(_)).into())
}

fn is_num(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    Ok(matches!(args[0], Type::Number(_)).into())
}

fn is_array(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    Ok(matches!(args[0], Type::Array(_)).into())
}

fn is_map(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    Ok(matches!(args[0], Type::Map(_) | Type::Count(_)).into())
}

// String manipulation

fn lower(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    Ok(args[0].as_string()?.to_lowercase().into())
}

fn upper(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    Ok(args[0].as_string()?.to_uppercase().into())
}

fn trim(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    Ok(Type::String(args[0].as_string()?.trim().to_string()))
}

fn split(args: &[Type]) -> Result<Type> {
    let sep = match args.len() {
        1 => Cow::Owned(String::new()),
        2 => args[1].as_string()?,
        _ => return Err(Error::WrongArgumentsNumber),
    };
    let string = args[0].as_string()?;
    let arr = if sep.is_empty() {
        string
            .chars()
            .map(|c| Type::String(c.to_string()))
            .collect::<Vec<Type>>()
            .into()
    } else {
        string
            .split(sep.as_ref())
            .map(|s| Type::String(s.to_string()))
            .collect::<Vec<Type>>()
            .into()
    };
    Ok(Type::Array(arr))
}

fn join(args: &[Type]) -> Result<Type> {
    let sep = match args.len() {
        1 => Cow::Owned(String::new()),
        2 => args[1].as_string()?,
        _ => return Err(Error::WrongArgumentsNumber),
    };
    let Type::Array(arr) = &args[0] else {
        return Err(Error::NotArray);
    };
    let s = super::join(arr.iter(), sep.as_ref());
    Ok(Type::String(s))
}

fn sub(args: &[Type]) -> Result<Type> {
    if args.len() < 3 || args.len() > 4 {
        return Err(Error::WrongArgumentsNumber);
    }
    let string = args[0].as_string()?;
    let regex = args[1].as_regex()?;
    let rep = args[2].as_string()?;
    let limit = if let Some(limit) = args.get(3) {
        usize::try_from(limit)?
    } else {
        0
    };
    let acc = regex.replacen(&string, &rep, limit);
    Ok(Type::String(acc))
}

fn replace(args: &[Type]) -> Result<Type> {
    if args.len() != 3 {
        return Err(Error::WrongArgumentsNumber);
    }
    let string = args[0].as_string()?;
    let from = args[1].as_string()?;
    let to = args[1].as_string()?;
    Ok(Type::String(string.replace(from.as_ref(), &to)))
}

fn remove_prefix(args: &[Type]) -> Result<Type> {
    if args.len() != 2 {
        return Err(Error::WrongArgumentsNumber);
    }
    let string = args[0].as_string()?;
    let prefix = args[1].as_string()?;
    Ok(Type::String(
        string
            .strip_prefix(prefix.as_ref())
            .unwrap_or(&string)
            .to_string(),
    ))
}

fn remove_suffix(args: &[Type]) -> Result<Type> {
    if args.len() != 2 {
        return Err(Error::WrongArgumentsNumber);
    }
    let string = args[0].as_string()?;
    let suffix = args[1].as_string()?;
    Ok(Type::String(
        string
            .strip_suffix(suffix.as_ref())
            .unwrap_or(&string)
            .to_string(),
    ))
}

fn regex_extract(args: &[Type]) -> Result<Type> {
    if args.len() != 2 {
        return Err(Error::WrongArgumentsNumber);
    }
    let string = args[0].as_string()?;
    let regex = args[1].as_regex()?;
    let mut arr = Array::new();
    for m in regex.captures(&string).iter() {
        for c in m.iter().flatten() {
            arr.push(Type::String(c.as_str().to_string()));
        }
    }
    Ok(Type::Array(arr))
}

fn escape(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    Ok(Type::String(
        args[0].as_string()?.escape_default().to_string(),
    ))
}

fn unescape(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    Ok(Type::String(super::unescape(&args[0].to_string())))
}

// Collection's operations

fn len(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    let res = match &args[0] {
        Type::String(s) => s.len(),
        Type::Array(a) => a.len(),
        Type::Map(m) => m.len(),
        _ => return Err(Error::WrongType),
    };
    Ok(res.into())
}

fn flatten(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    match &args[0] {
        Type::Array(arr) => Ok(Type::Array(arr.flatten())),
        Type::Map(map) => Ok(Type::Map(map.flatten())),
        _ => Err(Error::WrongType),
    }
}

fn rev(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    let val = match &args[0] {
        Type::Array(arr) => Type::Array(arr.reverse()),
        Type::Map(map) => Type::Map(map.reverse()),
        Type::String(str) => Type::String(str.chars().rev().collect::<String>()),
        Type::Regex(rx) => Type::String(rx.to_string().chars().rev().collect::<String>()),
        _ => return Err(Error::WrongType),
    };
    Ok(val)
}

fn sort(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    let val = match &args[0] {
        Type::Array(arr) => Type::Array(arr.sorted()),
        Type::Map(map) => Type::Map(map.sorted()),
        _ => return Err(Error::WrongType),
    };
    Ok(val)
}

fn unique(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    match &args[0] {
        Type::Array(arr) => {
            #[allow(clippy::mutable_key_type)]
            let mut seen = HashSet::new();
            let acc = arr
                .iter()
                .filter(|&x| seen.insert(x))
                .cloned()
                .collect::<Vec<Type>>()
                .into();
            Ok(Type::Array(acc))
        }
        _ => Err(Error::NotArray),
    }
}

fn keys(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    if let Type::Map(m) = &args[0] {
        let acc: Vec<Type> = m.keys().map(|k| Type::String(k.to_string())).collect();
        return Ok(Type::Array(acc.into()));
    }
    Err(Error::NotMap)
}

fn vals(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    if let Type::Map(m) = &args[0] {
        let acc: Vec<Type> = m.values().cloned().collect();
        return Ok(Type::Array(acc.into()));
    }
    Err(Error::NotMap)
}

// Math

fn min(args: &[Type]) -> Result<Type> {
    if args.len() != 2 {
        return Err(Error::WrongArgumentsNumber);
    }
    let lhs: Number = Number::try_from(&args[0]).unwrap_or(Number::NAN);
    let rhs: Number = Number::try_from(&args[1]).unwrap_or(Number::NAN);
    Ok(Type::Number(std::cmp::min(lhs, rhs)))
}

fn max(args: &[Type]) -> Result<Type> {
    if args.len() != 2 {
        return Err(Error::WrongArgumentsNumber);
    }
    let lhs = Number::try_from(&args[0]).unwrap_or(Number::NAN);
    let rhs = Number::try_from(&args[1]).unwrap_or(Number::NAN);
    Ok(Type::Number(std::cmp::max(lhs, rhs)))
}

fn abs(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    let num = Number::try_from(&args[0]).unwrap_or(Number::NAN);
    Ok(Type::Number(num.abs()))
}

fn sqrt(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    let num = Number::try_from(&args[0]).unwrap_or(Number::NAN);
    Ok(Type::Number(num.sqrt()))
}

fn exp(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    let num = Number::try_from(&args[0]).unwrap_or(Number::NAN);
    Ok(Type::Number(num.exp()))
}

fn log(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    let num = Number::try_from(&args[0]).unwrap_or(Number::NAN);
    Ok(Type::Number(num.log()))
}

fn log10(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    let num = Number::try_from(&args[0]).unwrap_or(Number::NAN);
    Ok(Type::Number(num.log10()))
}

fn floor(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    let Ok(num) = Number::try_from(&args[0]) else {
        return Ok(Type::Number(Number::NAN));
    };
    Ok(Type::Number(num.floor()))
}

fn round(args: &[Type]) -> Result<Type> {
    if args.is_empty() || args.len() > 2 {
        return Err(Error::WrongArgumentsNumber);
    }
    let number = Number::try_from(&args[0]).unwrap_or(Number::NAN);
    let digits = if let Some(v) = args.get(1) {
        usize::try_from(v)?
    } else {
        0
    };
    // https://stackoverflow.com/a/61101531
    let out = format!("{:.1$}", number, digits);
    Ok(Type::String(out))
}

// Parsers & conversions

fn to_str(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    if args[0] == Type::Null {
        return Ok(Type::String(String::new()));
    }
    Ok(Type::String(args[0].to_string()))
}

fn to_num(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    let num = match &args[0] {
        Type::String(s) if s.is_empty() => Number::ZERO,
        Type::Null => Number::ZERO,
        val => Number::try_from(val)?,
    };
    Ok(Type::Number(num))
}

fn to_bool(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    let val = match &args[0] {
        Type::Bool(val) => *val,
        Type::String(str) if str.to_lowercase() == "true" => true,
        Type::String(str) if str.to_lowercase() == "false" => false,
        Type::Number(num) => num.to_bool(),
        other => {
            return Err(Error::Custom(format!(
                "{} cannot be converted to bool",
                other
            )));
        }
    };
    Ok(Type::Bool(val))
}

fn from_json(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    match &args[0] {
        map @ Type::Map(_) => Ok(map.clone()),
        other => {
            let s = other.as_string()?;
            Ok(Type::Map(Map::from_json(&s)?))
        }
    }
}

// Side effects

fn error(args: &[Type]) -> Result<Type> {
    if args.len() != 1 {
        return Err(Error::WrongArgumentsNumber);
    }
    Err(Error::Custom(format!("{}", args[0])))
}

// Aggregator initializers

fn count(args: &[Type]) -> Result<Type> {
    if !args.is_empty() {
        return Err(Error::WrongArgumentsNumber);
    }
    Ok(Type::Count(Count::new()))
}
