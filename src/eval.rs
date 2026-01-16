use crate::{
    Error, Result,
    buildins::Procedure,
    csv, logfmt,
    types::{
        Array, Collection, Expr, Function, Map, Match, Number, Operator, Print, Type, Variable,
        wrapped_usize,
    },
};
use indexmap::IndexMap;
use std::{
    borrow::Cow,
    collections::HashMap,
    env::{self, VarError},
    io::Write,
};

#[derive(Debug, Default)]
pub(crate) struct Context<O: Write> {
    pub(crate) index: i64,
    pub(crate) line: String,
    pub(crate) done: bool,
    pub(crate) buildins: HashMap<String, Procedure>,
    pub(crate) funs: HashMap<String, Function>,
    pub(crate) globals: Map,
    pub(crate) print_matched: bool,
    pub(crate) out: O,
}

pub(crate) fn eval<O: Write>(expr: &Expr, data: &mut Map, ctx: &mut Context<O>) -> Result<Type> {
    use Expr::*;
    use Operator::*;
    let mut expr = Cow::Borrowed(expr);
    loop {
        return match expr.as_ref() {
            Const(val) => Ok(val.clone()),
            NewArray(arr) => {
                let arr = eval_all(arr, data, ctx)?.into();
                Ok(Type::Array(arr))
            }
            NewMap(map) => {
                let mut acc = Map::new();
                for (k, v) in map {
                    acc.insert(k.to_string(), eval(v, data, ctx)?);
                }
                Ok(Type::Map(acc))
            }
            Apply(name, args) if name == "has" => {
                let Some(Expr::Get(var)) = args.first() else {
                    unreachable!()
                };
                Ok(var.exists(data, ctx).into())
            }
            Apply(name, args) if name == "local" => {
                if !args.is_empty() {
                    return Err(Error::WrongArgumentsNumber);
                }
                Ok(Type::Map(data.clone()))
            }
            Apply(name, args) => {
                let args = eval_all(args, data, ctx)?;
                match ctx.buildins.get(name).cloned() {
                    Some(func) => func(&args),
                    None => match ctx.funs.get(name).cloned() {
                        Some(func) => {
                            let (e, d) = func.call(&args, ctx)?;
                            expr = Cow::Owned(e);
                            *data = d;
                            continue;
                        }
                        None => return Err(Error::Custom(format!("unknown function: {}", name))),
                    },
                }
            }
            UnaryOp(op, val) => {
                let res = eval(val, data, ctx)?;
                if let Operator::Not = op {
                    return Ok(!res);
                }
                let mut val = Number::try_from(&res).unwrap_or(Number::NAN);
                if let Operator::Sub = op {
                    val = -val;
                }
                Ok(Type::Number(val))
            }
            Op(In, lhs, rhs) => {
                let lhs = eval(lhs, data, ctx)?;
                let rhs = eval(rhs, data, ctx)?;
                let ok = match rhs {
                    Type::Array(arr) => arr.contains(&lhs),
                    Type::Map(map) => map.contains_key(lhs.as_string()?.as_ref()),
                    Type::String(str) => str.contains(lhs.as_string()?.as_ref()),
                    _ => return Err(Error::WrongType),
                };
                Ok(ok.into())
            }
            Op(Join, lhs, rhs) => {
                let lhs = eval(lhs, data, ctx)?;
                let rhs = eval(rhs, data, ctx)?;
                Ok(lhs.join(&rhs)?)
            }
            Op(op @ (Add | Sub | Mul | Div | Mod | Pow), lhs, rhs) => {
                let lhs = Number::try_from(&eval(lhs, data, ctx)?).unwrap_or(Number::NAN);
                let rhs = Number::try_from(&eval(rhs, data, ctx)?).unwrap_or(Number::NAN);
                let res = match op {
                    Add => lhs + rhs,
                    Sub => lhs - rhs,
                    Mul => lhs * rhs,
                    Div => lhs / rhs,
                    Mod => lhs % rhs,
                    Pow => lhs.pow(rhs),
                    _ => unreachable!(),
                };
                Ok(Type::Number(res))
            }
            Op(op @ (Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual), lhs, rhs) => {
                let lhs = eval(lhs, data, ctx)?;
                let rhs = eval(rhs, data, ctx)?;
                let res = match op {
                    Equal => lhs == rhs,
                    NotEqual => lhs != rhs,
                    Less => lhs < rhs,
                    LessEqual => lhs <= rhs,
                    Greater => lhs > rhs,
                    GreaterEqual => lhs >= rhs,
                    _ => unreachable!(),
                };
                Ok(res.into())
            }
            Op(And, lhs, rhs) => {
                if !eval(lhs, data, ctx)?.is_true() {
                    return Ok(Type::FALSE);
                }
                eval(rhs, data, ctx)
            }
            Op(Or, lhs, rhs) => {
                let lhs = eval(lhs, data, ctx)?;
                if lhs.is_true() {
                    return Ok(lhs);
                }
                eval(rhs, data, ctx)
            }
            Op(op @ (Matches | NotMatches), lhs, rhs) => {
                let value = match eval(lhs, data, ctx)? {
                    Type::String(s) => s,
                    _ => return Err(Error::NotString),
                };
                let regex = eval(rhs, data, ctx)?;
                let regex = regex.as_regex()?;
                let ok: Type = regex.is_match(&value).into();
                if *op == NotMatches { Ok(!ok) } else { Ok(ok) }
            }
            Op(Not, _, _) => unreachable!(),
            Get(key) => match key {
                Variable::Local(k) => match data.get(k) {
                    Some(val) => Ok(val.clone()),
                    None => return Err(Error::UnknownVar(key.clone())),
                },
                Variable::Global(k) => match ctx.globals.get(k) {
                    Some(val) => Ok(val.clone()),
                    None => return Err(Error::UnknownVar(key.clone())),
                },
                Variable::Env(k) => match env_get(k) {
                    Some(val) => Ok(Type::String(val)),
                    None => return Err(Error::UnknownVar(key.clone())),
                },
                Variable::Line => Ok(Type::String(ctx.line.to_string())),
                Variable::LineNumber => Ok(Type::Number(ctx.index.into())),
            },
            GetField(container, keys) => {
                let keys = eval_all(keys, data, ctx)?;
                let val = match eval(container, data, ctx)? {
                    Type::Map(map) => map.get_rec(&keys)?.unwrap_or(Type::Null),
                    Type::Array(arr) => arr.get_rec(&keys)?.unwrap_or(Type::Null),
                    other => {
                        let s = other.as_string()?;
                        let index = wrapped_usize(&keys[0], s.len())?;
                        let val = if let Some(c) = s.chars().nth(index) {
                            c.to_string()
                        } else {
                            String::new()
                        };
                        Type::String(val)
                    }
                };
                Ok(val)
            }
            Set(lhs, keys, rhs) if keys.is_empty() => {
                let val = eval(rhs, data, ctx)?;
                if matches!(val, Type::Any) {
                    // don't assign Any
                    return Ok(val);
                }
                match lhs {
                    Variable::Local(v) => data.insert(v.to_string(), val),
                    Variable::Global(v) => {
                        ctx.globals.insert(v.to_string(), val);
                    }
                    Variable::Env(k) => {
                        let val = val.to_string();
                        if k.is_empty() || k.contains('\0') || k.contains('=') {
                            return Err(Error::Custom(format!(
                                "invalid environment variable name: {}",
                                k
                            )));
                        }
                        if val.contains('\0') {
                            return Err(Error::Custom(format!(
                                "invalid value for environment variable: {}",
                                val
                            )));
                        }
                        unsafe { env::set_var(k, val) };
                    }
                    Variable::Line => {
                        ctx.line = val.to_string();
                    }
                    Variable::LineNumber => {
                        if let Type::Number(n) = val {
                            ctx.index = n.into();
                        } else {
                            return Err(Error::NotNumber);
                        }
                    }
                }
                Ok(Type::Null)
            }
            Set(lhs, keys, rhs) => {
                let val = eval(rhs, data, ctx)?;
                if matches!(val, Type::Any) {
                    // don't assign Any
                    return Ok(Type::Null);
                }
                let keys = eval_all(keys, data, ctx)?;
                let container = match lhs {
                    Variable::Local(v) => match data.get_mut(v) {
                        Some(container) => container,
                        None => return Err(Error::UnknownVar(lhs.clone())),
                    },
                    Variable::Global(v) => match ctx.globals.get_mut(v) {
                        Some(container) => container,
                        None => return Err(Error::UnknownVar(lhs.clone())),
                    },
                    Variable::Env(_) | Variable::Line | Variable::LineNumber => {
                        return Err(Error::NotMap);
                    }
                };
                match container {
                    Type::Map(c) => c.insert_rec(&keys, val)?,
                    Type::Array(c) => c.insert_rec(&keys, val)?,
                    _ => return Err(Error::WrongType),
                }
                Ok(Type::Null)
            }
            AddAssign(lhs, keys, rhs) if keys.is_empty() => {
                let rhs = eval(rhs, data, ctx)?;
                if matches!(rhs, Type::Any) {
                    // don't assign Any
                    return Ok(Type::Null);
                }
                match lhs {
                    Variable::Local(k) => {
                        if let Some(lhs) = data.get_mut(k) {
                            add_assign(lhs, &rhs)?;
                        } else {
                            return Err(Error::UnknownVar(lhs.clone()));
                        }
                    }
                    Variable::Global(k) => {
                        if let Some(lhs) = ctx.globals.get_mut(k) {
                            add_assign(lhs, &rhs)?;
                        } else {
                            return Err(Error::UnknownVar(lhs.clone()));
                        }
                    }
                    Variable::Env(k) => {
                        let val = rhs.to_string();
                        if k.is_empty() || k.contains('\0') || k.contains('=') {
                            return Err(Error::Custom(format!(
                                "invalid environment variable name: {}",
                                k
                            )));
                        }
                        let Some(mut old) = env_get(k) else {
                            return Err(Error::UnknownVar(lhs.clone()));
                        };
                        old.push_str(&val);
                        if old.contains('\0') {
                            return Err(Error::Custom(format!(
                                "invalid value for environment variable: {}",
                                old
                            )));
                        }
                        unsafe { env::set_var(k, old) };
                    }
                    Variable::Line => {
                        let s = rhs.to_string();
                        ctx.line.push_str(&s);
                    }
                    Variable::LineNumber => {
                        if let Type::Number(n) = rhs {
                            ctx.index += i64::from(n);
                        } else {
                            return Err(Error::NotNumber);
                        }
                    }
                }
                Ok(Type::Null)
            }
            AddAssign(lhs, keys, expr) => {
                let rhs = eval(expr, data, ctx)?;
                if matches!(rhs, Type::Any) {
                    // don't assign Any
                    return Ok(Type::Null);
                }
                let keys = eval_all(keys, data, ctx)?;
                let container = match lhs {
                    Variable::Local(v) => match data.get_mut(v) {
                        Some(container) => container,
                        None => return Err(Error::UnknownVar(lhs.clone())),
                    },
                    Variable::Global(v) => match ctx.globals.get_mut(v) {
                        Some(container) => container,
                        None => return Err(Error::UnknownVar(lhs.clone())),
                    },
                    Variable::Env(_) | Variable::Line | Variable::LineNumber => {
                        return Err(Error::WrongType);
                    }
                };
                let old = match container {
                    Type::Map(map) => map.get_mut_rec(&keys)?,
                    Type::Array(arr) => arr.get_mut_rec(&keys)?,
                    _ => return Err(Error::WrongType),
                };
                add_assign(old, &rhs)?;
                Ok(Type::Null)
            }
            Slice(cont, start, stop) => {
                let cont = eval(cont, data, ctx)?;
                let start = &eval(start, data, ctx)?;
                let stop = if let Some(expr) = stop {
                    Some(eval(expr, data, ctx)?)
                } else {
                    None
                };

                if let Type::Array(arr) = cont {
                    let start = wrapped_usize(start, arr.len())?;
                    if start >= arr.len() {
                        return Ok(Type::Array(Array::default()));
                    }
                    let stop = if let Some(ref stop) = stop {
                        wrapped_usize(stop, arr.len())?
                    } else {
                        arr.len() - 1
                    };
                    if start > stop {
                        return Err(Error::IndexError);
                    }
                    let acc = arr.slice(start, stop).unwrap_or_default();
                    return Ok(Type::Array(acc));
                }

                let s = cont.as_string()?;
                let start = wrapped_usize(start, s.len())?;
                let acc = if start >= s.len() {
                    String::new()
                } else if let Some(ref stop) = stop {
                    let stop = wrapped_usize(stop, s.len())?;
                    if start > stop {
                        return Err(Error::IndexError);
                    }
                    s.get(start..=stop).unwrap_or_default().to_string()
                } else {
                    s.get(start..).unwrap_or_default().to_string()
                };
                Ok(Type::String(acc))
            }
            If(cond, yes, no) => {
                // tail-call optimization
                expr = if eval(cond, data, ctx)?.is_true() {
                    Cow::Owned(eval_but_last(yes, data, ctx)?)
                } else {
                    Cow::Owned(eval_but_last(no, data, ctx)?)
                };
                continue;
            }
            Case(question, branches) => {
                expr = case(question, branches, data, ctx)?;
                continue;
            }
            For(key, iterable, body) => {
                let iterable = eval(iterable, data, ctx)?;
                match iterable {
                    Type::Array(ref a) => for_iter(key, body, a.iter().cloned(), data, ctx),
                    Type::Map(ref m) => {
                        let iter = m.keys().map(|k| Type::String(k.to_string()));
                        for_iter(key, body, iter, data, ctx)
                    }
                    ref other => {
                        let s = other.as_string()?;
                        let iter = s.chars().map(|c| Type::String(c.to_string()));
                        for_iter(key, body, iter, data, ctx)
                    }
                }
            }
            Match(parser) => Ok(parser.matches(data, ctx)?.into()),
            Between(start, stop, inside) => {
                if inside.is_true() {
                    if stop.matches(data, ctx)? {
                        inside.set(false);
                    }
                    Ok(Type::TRUE)
                } else {
                    let ok = start.matches(data, ctx)?;
                    if ok {
                        inside.set(true);
                    }
                    Ok(ok.into())
                }
            }
            Print(print) => {
                let s = print.format(data, ctx)?;
                writeln!(ctx.out, "{}", s)?;
                Ok(Type::Null)
            }
            Action(action) => Err(Error::Action(*action)),
            Template(template) => {
                let s = template.interpolate(data, ctx)?;
                Ok(Type::String(s))
            }
        };
    }
}

fn for_iter<O: Write, I>(
    key: &str,
    body: &[Expr],
    iter: I,
    data: &mut Map,
    ctx: &mut Context<O>,
) -> Result<Type>
where
    I: IntoIterator<Item = Type>,
{
    for v in iter {
        data.insert(key.to_string(), v);
        if !eval_body(body, data, ctx)?.is_true() {
            return Ok(Type::FALSE);
        }
    }
    Ok(Type::Null)
}

fn env_get(key: &str) -> Option<String> {
    let val = match env::var(key) {
        Ok(val) => val,
        Err(VarError::NotUnicode(s)) => s.to_string_lossy().to_string(),
        Err(VarError::NotPresent) => return None,
    };
    Some(val)
}

fn case<'a, O: Write>(
    question: &Expr,
    branches: &[(Vec<Expr>, Vec<Expr>)],
    data: &mut Map,
    ctx: &mut Context<O>,
) -> Result<Cow<'a, Expr>> {
    let val = eval(question, data, ctx)?;
    let mut expr = Cow::Owned(Expr::Const(Type::Null));
    for (candidates, body) in branches {
        if any_equal(candidates, &val, ctx, data)? {
            // tail-call optimization
            expr = Cow::Owned(eval_but_last(body, data, ctx)?);
            break;
        }
    }
    Ok(expr)
}

fn any_equal<O: Write>(
    candidates: &[Expr],
    val: &Type,
    ctx: &mut Context<O>,
    data: &mut Map,
) -> Result<bool> {
    for expr in candidates {
        if *val == eval(expr, data, ctx)? {
            return Ok(true);
        }
    }
    Ok(false)
}

fn add_assign(lhs: &mut Type, rhs: &Type) -> Result<()> {
    match lhs {
        Type::Number(num) => {
            let rhs = Number::try_from(rhs)?;
            *lhs = Type::Number(*num + rhs);
        }
        Type::Count(lhs) => {
            let key = rhs.to_string();
            if let Some(val) = lhs.0.get_mut(&key) {
                *val += 1;
            } else {
                lhs.0.insert(key, 1);
            }
        }
        Type::Array(lhs) => lhs.push(rhs.clone()),
        _ => {
            let s = format!("{}{}", lhs, rhs.as_string()?);
            *lhs = Type::String(s);
        }
    }
    Ok(())
}

pub(crate) fn eval_all<O: Write>(
    exprs: &[Expr],
    data: &mut Map,
    ctx: &mut Context<O>,
) -> Result<Vec<Type>> {
    exprs.iter().map(|val| eval(val, data, ctx)).collect()
}

fn eval_body<O: Write>(exprs: &[Expr], data: &mut Map, ctx: &mut Context<O>) -> Result<Type> {
    let iter = &mut exprs.iter();
    let mut last = Type::Null;
    for elem in iter {
        last = eval(elem, data, ctx)?;
        if !last.is_true() {
            return Ok(Type::FALSE);
        }
    }
    Ok(last)
}

pub(crate) fn eval_but_last<O: Write>(
    exprs: &[Expr],
    data: &mut Map,
    ctx: &mut Context<O>,
) -> Result<Expr> {
    let iter = &mut exprs.iter();
    let mut last = iter.next().unwrap_or(&Expr::Const(Type::Null));
    for elem in iter {
        if !eval(last, data, ctx)?.is_true() {
            return Ok(Expr::Const(Type::FALSE));
        }
        last = elem;
    }
    Ok(last.clone())
}

impl Function {
    fn call<O: Write>(&self, args: &[Type], ctx: &mut Context<O>) -> Result<(Expr, Map)> {
        // initialize arguments
        let mut data = Map::new();
        if args.len() != self.args.len() {
            return Err(Error::WrongArgumentsNumber);
        }
        std::iter::zip(self.args.iter(), args)
            .for_each(|(k, v)| data.insert(k.to_string(), v.clone()));
        // run body (tail-call optimized)
        let last = eval_but_last(&self.body, &mut data, ctx)?;
        Ok((last, data))
    }
}

impl Match {
    fn matches<O: Write>(&self, data: &mut Map, ctx: &mut Context<O>) -> Result<bool> {
        use Match::*;
        let mut matched = false;
        let res = match self {
            RegexMatches(regex) => regex.is_match(&ctx.line),
            Json => {
                let json: IndexMap<String, serde_json::Value> = serde_json::from_str(&ctx.line)?;
                for (ref key, val) in json {
                    data.insert(key.to_string(), val.into());
                    matched = true;
                }
                matched
            }
            Logfmt => {
                let reader = logfmt::Reader::from(&ctx.line);
                for (key, val) in reader {
                    let val = Type::String(val);
                    data.insert(key, val);
                    matched = true;
                }
                matched
            }
            Csv(columns) => {
                let fields = csv::Reader::from(&ctx.line);
                for (key, val) in std::iter::zip(columns, fields) {
                    if let Some(key) = key {
                        data.insert(key.to_string(), Type::String(val));
                        matched = true;
                    }
                }
                matched
            }
            Tsv(columns) => {
                // https://github.com/eBay/tsv-utils/blob/master/docs/comparing-tsv-and-csv.md
                let fields = ctx.line.split('\t');
                for (key, val) in std::iter::zip(columns, fields) {
                    if let Some(key) = key {
                        data.insert(key.to_string(), Type::String(val.to_string()));
                        matched = true;
                    }
                }
                matched
            }
            Regex(regex) => {
                let caps = match regex.captures(&ctx.line) {
                    Some(caps) => caps,
                    None => return Ok(false),
                };
                for key in regex.capture_names() {
                    let val = Type::String(caps[key].to_string());
                    data.insert(key.to_string(), val);
                    matched = true;
                }
                matched
            }
            Expr(expr) => eval(expr, data, ctx)?.is_true(),
        };
        Ok(res)
    }
}

impl Print {
    fn format<O: Write>(&self, data: &mut Map, ctx: &mut Context<O>) -> Result<String> {
        use Print::*;
        let val = match self {
            Json => data.to_json()?,
            PrettyJson => data.to_pretty()?,
            Logfmt => data.to_logfmt()?,
            Expr(expr) => eval(expr, data, ctx)?.to_string(),
        };
        Ok(val)
    }
}

impl Variable {
    fn exists<O: Write>(&self, data: &Map, ctx: &Context<O>) -> bool {
        match self {
            Variable::Local(k) => data.contains_key(k),
            Variable::Global(k) => ctx.globals.contains_key(k),
            Variable::Env(k) => !matches!(env::var(k), Err(VarError::NotPresent)),
            _ => true,
        }
    }
}
