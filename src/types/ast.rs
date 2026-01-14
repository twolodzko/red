use super::{Operator, Regex, Template, Type};
use crate::join;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Default, PartialEq)]
pub struct Program {
    pub(crate) funs: HashMap<String, Function>,
    pub(crate) vars: Vec<(String, Expr)>,
    pub(crate) body: Vec<Block>,
    pub(crate) end: Vec<Block>,
    pub print_matched: bool,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub(crate) struct Block(pub(crate) Vec<Expr>);

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Match {
    Json,
    Logfmt,
    Regex(Regex),
    Csv(Vec<Option<String>>),
    Tsv(Vec<Option<String>>),
    RegexMatches(Regex),
    Expr(Box<Expr>),
}

impl std::fmt::Display for Match {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Match::*;
        match self {
            Json => write!(f, "json"),
            Logfmt => write!(f, "logfmt"),
            Regex(r) => write!(f, "/{}/", r),
            Csv(k) => {
                let s = join(
                    k.iter().map(|v| match v {
                        Some(v) => v.to_string(),
                        None => "_".to_string(),
                    }),
                    ", ",
                );
                write!(f, "csv({})", s)
            }
            Tsv(k) => {
                let s = join(
                    k.iter().map(|v| match v {
                        Some(v) => v.to_string(),
                        None => "_".to_string(),
                    }),
                    ", ",
                );
                write!(f, "tsv({})", s)
            }
            RegexMatches(r) => write!(f, "/{}/", r),
            Expr(e) => e.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Print {
    Json,
    PrettyJson,
    Logfmt,
    Expr(Box<Expr>),
}

impl std::fmt::Display for Print {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Print::*;
        match self {
            Json => write!(f, "json"),
            PrettyJson => write!(f, "pretty"),
            Logfmt => write!(f, "logfmt"),
            Expr(e) => e.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Expr {
    Const(Type),
    Apply(String, Vec<Expr>),
    Op(Operator, Box<Expr>, Box<Expr>),
    UnaryOp(Operator, Box<Expr>),
    NewArray(Vec<Expr>),
    NewMap(Vec<(String, Expr)>),
    Get(Variable),
    GetField(Box<Expr>, Vec<Expr>),
    Set(Variable, Vec<Expr>, Box<Expr>),
    AddAssign(Variable, Vec<Expr>, Box<Expr>),
    Slice(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    If(Box<Expr>, Vec<Expr>, Vec<Expr>),
    Case(Box<Expr>, Vec<(Vec<Expr>, Vec<Expr>)>),
    Match(Match),
    Between(Match, Match, AtomicBool),
    Print(Print),
    Action(Action),
    Template(Template),
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub(crate) enum Action {
    Next,
    Exit,
}

impl std::fmt::Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Action::Exit => write!(f, "exit"),
            Action::Next => write!(f, "next"),
        }
    }
}

impl From<Type> for Expr {
    fn from(value: Type) -> Self {
        Expr::Const(value)
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expr::*;
        match self {
            Op(op, lhs, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            UnaryOp(op, val) => write!(f, "{}({})", op, val),
            Const(Type::String(s)) => write!(f, "\"{}\"", s),
            Const(val) => write!(f, "{}", val),
            Apply(name, args) => write!(f, "{}({})", name, join(args, ", ")),
            NewMap(vals) => {
                write!(
                    f,
                    "{{{}}}",
                    join(vals.iter().map(|(k, v)| format!("{}: {}", k, v)), ", ")
                )
            }
            NewArray(vals) => write!(f, "[{}]", join(vals, ", ")),
            Get(name) => write!(f, "{}", name),
            GetField(cont, keys) => {
                let acc = keys
                    .iter()
                    .map(|k| format!("[{}]", k))
                    .collect::<Vec<String>>()
                    .join("");
                write!(f, "{}{}", cont, acc)
            }
            Set(cont, keys, val) => {
                if keys.is_empty() {
                    write!(f, "{} = {}", cont, val)
                } else {
                    let acc = keys
                        .iter()
                        .map(|k| format!("[{}]", k))
                        .collect::<Vec<String>>()
                        .join("");
                    write!(f, "{}{} = {}", cont, acc, val)
                }
            }
            AddAssign(lhs, keys, rhs) => {
                if keys.is_empty() {
                    write!(f, "{} += {}", lhs, rhs)
                } else {
                    let acc = keys
                        .iter()
                        .map(|k| format!("[{}]", k))
                        .collect::<Vec<String>>()
                        .join("");
                    write!(f, "{}{} += {}", lhs, acc, rhs)
                }
            }
            Slice(cont, lhs, None) => write!(f, "{}[{}:]", cont, lhs),
            Slice(cont, lhs, Some(rhs)) => write!(f, "{}[{}:{}]", cont, lhs, rhs),
            If(cond, yes, no) => write!(
                f,
                "if {} {{ {} }} else {{ {} }}",
                cond,
                join(yes, ", "),
                join(no, ", ")
            ),
            Case(expr, branches) => {
                let mut acc = Vec::new();
                for (cond, body) in branches {
                    let cond = join(cond, " | ");
                    let body = join(body, ", ");
                    acc.push(format!("{} {{ {} }}", cond, body));
                }
                writeln!(f, "match {} {{ {} }}", expr, join(acc, ", "))
            }
            Match(p) => p.fmt(f),
            Between(start, stop, _) => write!(f, "between({}, {})", start, stop),
            Print(p) => p.fmt(f),
            Action(a) => a.fmt(f),
            Template(t) => t.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Function {
    pub(crate) args: Vec<String>,
    pub(crate) body: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Variable {
    Line,
    LineNumber,
    Local(String),
    Global(String),
    Env(String),
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Variable::*;
        match self {
            Line => write!(f, "."),
            LineNumber => write!(f, "N"),
            Local(n) => write!(f, "{}", n),
            Global(n) => write!(f, "&{}", n),
            Env(n) => write!(f, "${}", n),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub(crate) struct AtomicBool(Rc<RefCell<bool>>);

impl AtomicBool {
    pub(crate) fn is_true(&self) -> bool {
        *self.0.borrow()
    }

    pub(crate) fn set(&self, value: bool) {
        *self.0.borrow_mut() = value;
    }
}

#[cfg(test)]
mod tests {
    use crate::types::AtomicBool;

    #[test]
    fn atomic_bool() {
        let atomic = AtomicBool::default();
        assert!(!atomic.is_true());
        atomic.set(true);
        assert!(atomic.is_true());
        atomic.set(false);
        assert!(!atomic.is_true());
    }
}
