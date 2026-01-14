use super::join;
use crate::{
    Error, Result,
    eval::{Context, eval},
    parser::Parser,
    types::{Expr, Map, Regex, Variable},
};
use std::{io::Write, str::FromStr};

#[derive(Debug, Default, PartialEq, Clone)]
pub(crate) struct Template(Vec<Field>);

impl std::fmt::Display for Template {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        join(self.0.iter().map(ToString::to_string), "").fmt(f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Field {
    Literal(String),
    Variable(Expr),
    Wildcard,
}

impl Field {
    fn to_regex(&self) -> Result<String> {
        let regex = match self {
            Field::Wildcard => ".*".to_string(),
            Field::Literal(s) => escape(s),
            Field::Variable(Expr::Get(Variable::Local(n))) => format!("(?<{}>.*)", n),
            Field::Variable(_) => {
                return Err(Error::Custom(
                    "only local variables can be used to construct regular expressions".to_string(),
                ));
            }
        };
        Ok(regex)
    }
}

impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Field::*;
        match self {
            Literal(s) => s.fmt(f),
            Variable(e) => write!(f, "<{}>", e),
            Wildcard => write!(f, "<_>"),
        }
    }
}

fn escape(s: &str) -> String {
    // see: https://stackoverflow.com/a/32212181
    let mut acc = String::new();
    for c in s.chars() {
        if matches!(
            c,
            '.' | '^'
                | '$'
                | '*'
                | '+'
                | '-'
                | '?'
                | '('
                | ')'
                | '['
                | ']'
                | '{'
                | '}'
                | '\\'
                | '|'
        ) {
            acc.push('\\');
        }
        acc.push(c);
    }
    acc
}

impl TryFrom<Template> for Regex {
    type Error = Error;

    fn try_from(value: Template) -> Result<Regex> {
        Regex::from_str(&value.to_regex()?)
    }
}

impl FromStr for Template {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        Template::parse(s)
    }
}

impl Template {
    fn parse(input: &str) -> Result<Self> {
        use crate::Error::*;

        let mut chars = input.chars().peekable();
        let mut acc = String::new();
        let mut pattern = Vec::new();

        while let Some(c) = chars.next() {
            match c {
                '\\' => {
                    let Some(c) = chars.next() else {
                        return Err(Error::EndOfInput);
                    };
                    match c {
                        '<' | '>' => acc.push(c),
                        _ => {
                            acc.push('\\');
                            acc.push(c);
                        }
                    }
                }
                '<' => {
                    let Some(c) = chars.peek() else {
                        return Err(EndOfInput);
                    };
                    match c {
                        '_' => {
                            if !acc.is_empty() {
                                acc = crate::unescape(&acc);
                                pattern.push(Field::Literal(acc.to_string()));
                                acc.clear();
                            }

                            pattern.push(Field::Wildcard);
                            chars.next();

                            let Some(c) = chars.next() else {
                                return Err(EndOfInput);
                            };
                            match c {
                                '>' => (),
                                _ => return Err(WrongChar(c)),
                            }
                        }
                        _ => {
                            if !acc.is_empty() {
                                acc = crate::unescape(&acc);
                                pattern.push(Field::Literal(acc.to_string()));
                                acc.clear();
                            }

                            loop {
                                let Some(c) = chars.next() else {
                                    return Err(EndOfInput);
                                };
                                match c {
                                    '>' => {
                                        let mut parser = Parser::from(acc.to_string());
                                        let expr = parser.expr()?;
                                        pattern.push(Field::Variable(expr));
                                        acc.clear();
                                        break;
                                    }
                                    '\\' => {
                                        let Some(c) = chars.next() else {
                                            return Err(EndOfInput);
                                        };
                                        match c {
                                            '<' | '>' => acc.push(c),
                                            _ => {
                                                acc.push('\\');
                                                acc.push(c);
                                            }
                                        }
                                    }
                                    c => acc.push(c),
                                }
                            }
                        }
                    }
                }
                _ => acc.push(c),
            }
        }
        if !acc.is_empty() {
            acc = crate::unescape(&acc);
            pattern.push(Field::Literal(acc));
        }
        Ok(Template(pattern))
    }

    pub(crate) fn interpolate<O: Write>(
        &self,
        data: &mut Map,
        ctx: &mut Context<O>,
    ) -> Result<String> {
        let mut acc = String::new();
        for field in self.0.iter() {
            let s = match field {
                Field::Literal(s) => s,
                Field::Variable(expr) => {
                    let val = eval(expr, data, ctx)?;
                    &val.to_string()
                }
                Field::Wildcard => "",
            };
            acc.push_str(s);
        }
        Ok(acc)
    }

    fn to_regex(&self) -> Result<String> {
        let mut acc = String::new();
        for elem in self.0.iter() {
            acc.push_str(&elem.to_regex()?);
        }
        Ok(acc)
    }
}

#[cfg(test)]
mod tests {
    use super::Template;
    use crate::{
        eval::Context,
        tests::WriteMock,
        types::{Map, Type},
    };
    use regex::Regex;
    use std::str::FromStr;
    use test_case::test_case;

    #[test_case(
        "",
        "";
        "empty"
    )]
    #[test_case(
        r". ^ $ * + - ? ( ) [ ] { } \ |",
        r"\. \^ \$ \* \+ \- \? \( \) \[ \] \{ \} \\ \|";
        "regex special chars"
    )]
    #[test_case(
        "hello, world!",
        "hello, world!";
        "literal"
    )]
    #[test_case(
        "<a><b><c>",
        "(?<a>.*)(?<b>.*)(?<c>.*)";
        "only fields"
    )]
    #[test_case(
        "foo = <X> bar = <Y> <_>",
        "foo = (?<X>.*) bar = (?<Y>.*) .*";
        "mixed"
    )]
    #[test_case(
        r"\<<X>\>__\<\>__\<_\>__\<<_>\>",
        r"<(?<X>.*)>__<>__<_>__<.*>";
        "escaped"
    )]
    fn parse(input: &str, expected: &str) {
        let pattern = Template::parse(input).unwrap();
        assert_eq!(pattern.to_regex().unwrap(), expected)
    }

    #[test_case(
        "foo = <X> bar = <Y> <_>",
        "foo = \"abc\" bar = 123 baz = xxx buz = ... asdasd",
        true;
        "mixed matches"
    )]
    #[test_case(
        "foo = <X> bar = <Y> <_>",
        "foo = 123 wrong = xxx",
        false;
        "mixed no match"
    )]
    fn parse_and_match(template: &str, example: &str, should_match: bool) {
        let template = Template::from_str(template).unwrap();
        let regex = Regex::try_from(template.to_regex().unwrap()).unwrap();
        assert!(regex.is_match(example) == should_match)
    }

    #[test_case(
        "",
        &[],
        "";
        "empty"
    )]
    #[test_case(
        "a <_> b <_> c",
        &[],
        "a  b  c";
        "wildcards and literals"
    )]
    #[test_case(
        "foo = <X> bar = <Y>",
        &[("X", "_1_"), ("ZZZ", "<wrong>"), ("Y", "_2_")],
        "foo = _1_ bar = _2_";
        "variables"
    )]
    fn interpolate(template: &str, values: &[(&str, &str)], expected: &str) {
        let pattern = Template::from_str(template).unwrap();
        let data = &mut Map::new();
        let mut ctx = Context::new(WriteMock {});
        for (k, v) in values {
            data.insert(k.to_string(), Type::String(v.to_string()));
        }
        let result = pattern.interpolate(data, &mut ctx).unwrap();
        assert_eq!(result, expected)
    }
}
