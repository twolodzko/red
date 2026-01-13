use crate::{Error, Result, parser::code_reader::Reader, types::Operator};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// `"[^"]*"`
    String(String),
    /// `/[^/]*/`` or `^[^$]*$`
    Regex(String),
    /// ~"[^"]*"
    Template(String),
    /// integer or float
    Number(String),
    /// true or false
    Bool(String),
    /// `[a-zA-Z][a-zA-Z0-9_]*`
    Ident(String),
    /// ``` `[a-zA-Z][a-zA-Z0-9_]*` ```
    QuotedIdent(String),
    /// `&[a-zA-Z][a-zA-Z0-9_]*`
    GlobalVar(String),
    /// `$[a-zA-Z][a-zA-Z0-9_]*`
    EnvVar(String),
    /// null
    Null,
    /// =
    Assign,
    /// +=
    AddAssign,
    /// operators
    Op(Operator),
    /// ()[]{}
    Bracket(char),
    /// .
    Dot,
    /// ?
    Question,
    /// :
    Colon,
    /// fn
    Fn,
    /// var
    Var,
    /// end
    End,
    /// _
    Wildcard,
    /// , ; \n
    Delimiter(char),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        match self {
            String(s) => write!(f, "\"{}\"", s),
            Regex(s) => write!(f, "/{}/", s),
            Template(s) => write!(f, "~\"{}\"", s),
            Number(s) => s.fmt(f),
            Bool(s) => s.fmt(f),
            Ident(s) => s.fmt(f),
            QuotedIdent(s) => write!(f, "`{}`", s),
            GlobalVar(s) => write!(f, "&{}", s),
            EnvVar(s) => write!(f, "&{}", s),
            Assign => write!(f, "="),
            AddAssign => write!(f, "+="),
            Op(o) => o.fmt(f),
            Null => write!(f, "null"),
            Bracket(c) => write!(f, "{}", c),
            Delimiter(c) => write!(f, "{}", c),
            Dot => write!(f, "."),
            Question => write!(f, "?"),
            Colon => write!(f, ":"),
            Fn => write!(f, "fn"),
            Var => write!(f, "var"),
            End => write!(f, "end"),
            Wildcard => write!(f, "_"),
        }
    }
}

pub(crate) struct Tokenizer<R: Reader> {
    reader: R,
    cache: Option<Token>,
    can_be_op: bool,
}

impl<R: Reader> Tokenizer<R> {
    pub(crate) fn new(reader: R) -> Self {
        Self {
            reader,
            cache: None,
            can_be_op: false,
        }
    }

    pub(crate) fn peek(&mut self) -> Result<Option<Token>> {
        if self.cache.is_none() {
            self.cache = self.read_token()?;
        }
        Ok(self.cache.clone())
    }

    pub(crate) fn next(&mut self) -> Result<Option<Token>> {
        let res = self.peek()?;
        self.cache = self.read_token()?;
        Ok(res)
    }

    pub(crate) fn next_or_err(&mut self) -> Result<Token> {
        match self.next()? {
            Some(t) => Ok(t),
            None => Err(Error::EndOfInput),
        }
    }

    pub(crate) fn skip(&mut self) -> Result<()> {
        self.cache = self.read_token()?;
        Ok(())
    }

    fn read_token(&mut self) -> Result<Option<Token>> {
        use Token::*;
        while let Some(c) = self.reader.peek()? {
            match c {
                c if c.is_alphabetic() => {
                    let s = self.read_keyword()?;
                    let token = match s.as_str() {
                        "and" => Op(Operator::And),
                        "or" => Op(Operator::Or),
                        "not" => Op(Operator::Not),
                        "in" => Op(Operator::In),
                        "fn" => Fn,
                        "var" => Var,
                        "end" => End,
                        "null" => Null,
                        "inf" | "nan" => Number(s),
                        "true" | "false" => Bool(s),
                        _ => Ident(s),
                    };
                    self.can_be_op = false;
                    return Ok(Some(token));
                }
                c if c.is_ascii_digit() => {
                    self.can_be_op = true;
                    let s = self.read_number()?;
                    return Ok(Some(s));
                }
                _ => (),
            }

            // so not to repeat skip's all the time
            self.reader.skip();

            let token = match c {
                '"' | '\'' => {
                    let s = self.read_until(c)?;
                    String(s)
                }
                '`' => {
                    let s = self.read_until(c)?;
                    QuotedIdent(s)
                }
                '~' => {
                    let s = match self.reader.next_or_err()? {
                        c @ ('"' | '\'') => self.read_until(c)?,
                        other => return Err(Error::WrongChar(other)),
                    };
                    Template(s)
                }
                '#' => {
                    // skip comments
                    while let Some(c) = self.reader.peek()? {
                        if c == '\n' {
                            break;
                        }
                        self.reader.skip();
                    }
                    continue;
                }
                '_' => {
                    // they can have names, which are irrelevant
                    self.read_keyword()?;
                    Wildcard
                }
                '+' => match self.reader.peek()? {
                    Some('+') => {
                        self.reader.skip();
                        Op(Operator::Join)
                    }
                    Some('=') => {
                        self.reader.skip();
                        AddAssign
                    }
                    _ => Op(Operator::Add),
                },
                '-' => Op(Operator::Sub),
                '*' => Op(Operator::Mul),
                '/' => {
                    if self.can_be_op {
                        Op(Operator::Div)
                    } else {
                        let s = self.read_until('/')?;
                        Regex(s)
                    }
                }
                '%' => Op(Operator::Mod),
                '^' => {
                    if self.can_be_op {
                        Op(Operator::Pow)
                    } else {
                        let s = self.read_until('$')?;
                        Regex(format!("^{s}$"))
                    }
                }
                '=' => match self.reader.peek_or_err()? {
                    '~' => {
                        self.reader.skip();
                        Op(Operator::Matches)
                    }
                    '=' => {
                        self.reader.skip();
                        Op(Operator::Equal)
                    }
                    _ => Assign,
                },
                '!' => match self.reader.next_or_err()? {
                    '=' => Op(Operator::NotEqual),
                    '~' => Op(Operator::NotMatches),
                    c => return Err(Error::WrongChar(c)),
                },
                '<' => {
                    if self.reader.next_is('=')? {
                        Op(Operator::LessEqual)
                    } else {
                        Op(Operator::Less)
                    }
                }
                '>' => {
                    if self.reader.next_is('=')? {
                        Op(Operator::GreaterEqual)
                    } else {
                        Op(Operator::Greater)
                    }
                }
                '(' | ')' | '[' | ']' => Bracket(c),
                '{' => Bracket(c),
                '}' => Bracket(c),
                ':' => Colon,
                '.' => Dot,
                ',' => Delimiter(','),
                '\n' => Delimiter('\n'),
                ';' => Delimiter(';'),
                '?' => Question,
                '&' => {
                    let s = self.read_keyword()?;
                    if s.is_empty() {
                        Delimiter('&')
                    } else {
                        GlobalVar(s)
                    }
                }
                '$' => {
                    let s = self.read_keyword()?;
                    EnvVar(s)
                }
                '|' => Delimiter('|'),
                c if c.is_whitespace() => continue,
                _ => return Err(Error::WrongChar(c)),
            };
            // `/` and `^` can only be applied to numbers, so to:
            // constants, variables, elements of collections, function returns
            self.can_be_op = matches!(
                token,
                Number(_) | Ident(_) | QuotedIdent(_) | GlobalVar(_) | Bracket(')' | ']')
            );
            return Ok(Some(token));
        }
        Ok(None)
    }

    fn read_until(&mut self, delim: char) -> Result<String> {
        let mut acc = String::new();
        while let Some(c) = self.reader.next()? {
            match c {
                c if c == delim => return Ok(acc),
                '\\' => {
                    if let Some(e) = self.reader.next()? {
                        if e != delim {
                            acc.push(c);
                        }
                        acc.push(e);
                    } else {
                        break;
                    }
                }
                _ => acc.push(c),
            }
        }
        Err(Error::Missing(delim))
    }

    /// Read a possible unsigned number
    fn read_number(&mut self) -> Result<Token> {
        let mut acc = String::new();
        // [0-9]*
        acc.push_str(&self.read_digits()?);
        // .
        if self.reader.next_is('.')? {
            acc.push('.');
        }
        // [0-9]*
        acc.push_str(&self.read_digits()?);
        // [eE]
        if let Some(c @ ('e' | 'E')) = self.reader.peek()? {
            self.reader.skip();
            acc.push(c);
            // + | -
            if let Some(c @ ('+' | '-')) = self.reader.peek()? {
                self.reader.skip();
                acc.push(c);
            }
        }
        // [0-9]*
        acc.push_str(&self.read_digits()?);
        Ok(Token::Number(acc))
    }

    fn read_digits(&mut self) -> Result<String> {
        let mut acc = String::new();
        while let Some(c) = self.reader.peek()? {
            if !c.is_ascii_digit() {
                break;
            }
            self.reader.skip();
            acc.push(c);
        }
        Ok(acc)
    }

    fn read_keyword(&mut self) -> Result<String> {
        let mut acc = String::new();
        while let Some(c) = self.reader.peek()? {
            if is_keyword(c) {
                self.reader.skip();
                acc.push(c);
            } else {
                break;
            }
        }
        Ok(acc)
    }
}

fn is_keyword(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}
