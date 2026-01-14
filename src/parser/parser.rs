use crate::{
    Error, Result,
    parser::{
        code_reader::Reader,
        tokenizer::{Token, Tokenizer},
    },
    types::{
        self, AtomicBool, Block, Expr, Function, Match, Number, Operator, Print, Program, Regex,
        Template, Type, Variable,
    },
};
use std::str::FromStr;

pub struct Parser<R: Reader>(pub(crate) Tokenizer<R>);

impl<R: Reader> Parser<R> {
    pub fn parse(&mut self) -> Result<Program> {
        self.ignore_newlines()?;

        let mut prog = Program::default();
        while let Some(t) = self.0.peek()? {
            match t {
                Token::Fn => {
                    self.0.skip()?;
                    let name = match self.0.next_or_err()? {
                        Token::Ident(name) => name,
                        other => return Err(Error::WrongToken(other)),
                    };
                    self.expect(Token::Bracket('('))?;
                    let mut args = Vec::new();

                    if let Some(Token::Bracket(')')) = self.0.peek()? {
                        self.0.skip()?;
                        // no args
                    } else {
                        loop {
                            let arg = match self.0.next_or_err()? {
                                Token::Ident(name) => name,
                                other => return Err(Error::WrongToken(other)),
                            };
                            args.push(arg);
                            match self.0.next_or_err()? {
                                Token::Delimiter(',') => (),
                                Token::Bracket(')') => break,
                                other => return Err(Error::WrongToken(other)),
                            }
                        }
                    }
                    self.expect(Token::Bracket('{'))?;
                    let body = self.read_until(Token::Bracket('}'))?;
                    let func = Function { args, body };
                    prog.funs.insert(name, func);
                }
                Token::Var => {
                    self.0.skip()?;
                    let key = match self.0.next_or_err()? {
                        Token::GlobalVar(s) => s,
                        other => return Err(Error::WrongToken(other)),
                    };
                    self.expect(Token::Assign)?;
                    let val = self.expr()?;
                    match self.0.next()? {
                        None | Some(Token::Delimiter(';' | '\n')) => (),
                        Some(other) => return Err(Error::WrongToken(other)),
                    }
                    prog.vars.push((key, val));
                }
                Token::End => {
                    self.0.skip()?;
                    self.expect(Token::Bracket('{'))?;
                    self.ignore_newlines()?;
                    loop {
                        if let Some(Token::Bracket('}')) = self.0.peek()? {
                            self.0.skip()?;
                            break;
                        }
                        let block = self.block()?;
                        if !block.0.is_empty() {
                            prog.end.push(block);
                        }
                    }
                    self.ignore_newlines()?
                }
                _ => {
                    let block = self.block()?;
                    if !block.0.is_empty() {
                        prog.body.push(block);
                    }
                }
            }

            if let Some(Token::Delimiter(';')) = self.0.peek()? {
                self.0.skip()?;
            }
            self.ignore_newlines()?;
        }
        Ok(prog)
    }

    pub(crate) fn block(&mut self) -> Result<Block> {
        let mut acc = Vec::new();
        loop {
            if self.0.peek()?.is_none() {
                break;
            }
            let expr = self.expr()?;
            if let Expr::Const(v) = expr {
                return Err(Error::ConstantExpr(v));
            }
            acc.push(expr);
            match self.0.peek()? {
                Some(Token::Delimiter('\n')) | None => {
                    self.0.skip()?;
                    break;
                }
                Some(Token::Delimiter(';')) => {
                    self.0.skip()?;
                    self.ignore_newlines()?;
                    break;
                }
                Some(Token::Delimiter(',')) => {
                    self.0.skip()?;
                    self.ignore_newlines()?
                }
                Some(Token::Bracket('}')) => break,
                Some(other) => return Err(Error::WrongToken(other)),
            }
        }
        Ok(Block(acc))
    }

    pub(crate) fn expr(&mut self) -> Result<Expr> {
        let lhs = self.operation()?;

        match self.0.peek()? {
            Some(Token::Assign) => {
                self.0.skip()?;
                match &lhs {
                    Expr::Get(key) => {
                        let expr = self.operation()?;
                        return Ok(Expr::Set(key.clone(), Vec::new(), Box::new(expr)));
                    }
                    Expr::GetField(cont, keys) => {
                        let Expr::Get(cont) = cont.as_ref() else {
                            return Err(Error::Custom(format!("{} is not an identifier", cont)));
                        };
                        let expr = self.operation()?;
                        return Ok(Expr::Set(cont.clone(), keys.clone(), Box::new(expr)));
                    }
                    // we don't expect assign after anything else
                    _ => return Err(Error::WrongToken(Token::Assign)),
                }
            }
            Some(Token::AddAssign) => {
                self.0.skip()?;
                match &lhs {
                    Expr::Get(key) => {
                        let expr = self.operation()?;
                        return Ok(Expr::AddAssign(key.clone(), Vec::new(), Box::new(expr)));
                    }
                    Expr::GetField(cont, keys) => {
                        let Expr::Get(cont) = cont.as_ref() else {
                            return Err(Error::Custom(format!("{} is not an identifier", cont)));
                        };
                        let expr = self.operation()?;
                        return Ok(Expr::AddAssign(cont.clone(), keys.clone(), Box::new(expr)));
                    }
                    // we don't expect assign after anything else
                    _ => return Err(Error::WrongToken(Token::AddAssign)),
                }
            }
            _ => (),
        }

        // optimization: pre-compile regex'es
        if let Expr::Op(op @ (Operator::Matches | Operator::NotMatches), lhs, rhs) = &lhs
            && let Expr::Const(Type::String(s)) = rhs.as_ref()
        {
            let rhs = Regex::from_str(s)?;
            return Ok(Expr::Op(
                *op,
                lhs.clone(),
                Box::new(Expr::Const(rhs.into())),
            ));
        }
        Ok(lhs)
    }

    fn atom(&mut self) -> Result<Expr> {
        let Some(t) = self.0.next()? else {
            return Err(Error::EndOfInput);
        };
        let atom = match t {
            Token::Dot => Expr::Get(Variable::Line),
            Token::Wildcard => Expr::Const(Type::Any),
            Token::Null => Expr::Const(Type::Null),
            Token::Bool(s) => Expr::Const(Type::Bool(bool::from_str(&s).unwrap())),
            Token::Number(ref s) => {
                let n = Number::from_str(s)?;
                Expr::Const(Type::Number(n))
            }
            Token::String(ref s) => Expr::Const(Type::String(crate::unescape(s))),
            Token::Regex(ref s) => {
                let r = Regex::from_str(s)?;
                Expr::Const(Type::Regex(r))
            }
            Token::Ident(s) if s == "if" => {
                let cond = self.expr()?;
                let yes = if let Some(Token::Bracket('{')) = self.0.peek()? {
                    self.0.skip()?;
                    self.read_until(Token::Bracket('}'))?
                } else {
                    vec![self.expr()?]
                };
                let no = if let Some(Token::Ident(s)) = self.0.peek()?
                    && s == "else"
                {
                    self.0.skip()?;
                    if let Some(Token::Bracket('{')) = self.0.peek()? {
                        self.0.skip()?;
                        self.read_until(Token::Bracket('}'))?
                    } else {
                        vec![self.expr()?]
                    }
                } else {
                    vec![Expr::Const(Type::Null)]
                };
                Expr::If(Box::new(cond), yes, no)
            }
            Token::Ident(s) if s == "has" => {
                self.expect(Token::Bracket('('))?;
                match self.atom()? {
                    v @ Expr::Get(_) => {
                        self.expect(Token::Bracket(')'))?;
                        Expr::Apply(s, vec![v])
                    }
                    other => return Err(Error::Custom(format!("{} is not a variable", other))),
                }
            }
            Token::Ident(s) if s == "match" => {
                self.expect(Token::Bracket('('))?;
                let Some(parser) = self.matcher()? else {
                    return Err(Error::WrongToken(self.0.next_or_err()?));
                };
                self.expect(Token::Bracket(')'))?;
                Expr::Match(parser)
            }
            Token::Ident(s) if s == "between" => {
                self.expect(Token::Bracket('('))?;
                let start = match self.matcher()? {
                    Some(matcher) => matcher,
                    None => Match::Expr(Box::new(self.expr()?)),
                };
                self.expect(Token::Delimiter(','))?;
                let stop = match self.matcher()? {
                    Some(matcher) => matcher,
                    None => Match::Expr(Box::new(self.expr()?)),
                };
                self.expect(Token::Bracket(')'))?;
                Expr::Between(start, stop, AtomicBool::default())
            }
            Token::Ident(s) if s == "print" => {
                self.expect(Token::Bracket('('))?;
                let printer = self.printer()?;
                self.expect(Token::Bracket(')'))?;
                Expr::Print(printer)
            }
            Token::Ident(s) if s == "next" => {
                if let Some(Token::Bracket('(')) = self.0.peek()? {
                    self.0.skip()?;
                    self.expect(Token::Bracket(')'))?;
                }
                Expr::Action(types::Action::Next)
            }
            Token::Ident(s) if s == "exit" => {
                if let Some(Token::Bracket('(')) = self.0.peek()? {
                    self.0.skip()?;
                    self.expect(Token::Bracket(')'))?;
                }
                Expr::Action(types::Action::Exit)
            }
            Token::Ident(s) if s == "case" => self.case()?,
            Token::Ident(s) if s == "N" => Expr::Get(Variable::LineNumber),
            Token::Ident(s) | Token::QuotedIdent(s) => self.maybe_apply(s)?,
            Token::GlobalVar(s) => Expr::Get(Variable::Global(s)),
            Token::EnvVar(s) => Expr::Get(Variable::Env(s)),
            Token::Bracket('(') => {
                self.ignore_newlines()?;
                let inside = self.operation()?;
                self.ignore_newlines()?;
                self.expect(Token::Bracket(')'))?;
                inside
            }
            Token::Bracket('[') => Expr::NewArray(self.read_until(Token::Bracket(']'))?),
            Token::Bracket('{') => {
                let mut acc = Vec::new();
                if let Some(Token::Bracket('}')) = self.0.peek()? {
                    self.0.skip()?;
                    return Ok(Expr::NewMap(acc));
                }
                loop {
                    self.ignore_newlines()?;
                    let key = match self.0.next_or_err()? {
                        Token::Ident(key) => key,
                        other => return Err(Error::WrongToken(other)),
                    };
                    self.expect(Token::Colon)?;
                    let val = self.operation()?;
                    acc.push((key, val));
                    self.ignore_newlines()?;
                    match self.0.next_or_err()? {
                        Token::Bracket('}') => break,
                        Token::Delimiter(',') => continue,
                        other => return Err(Error::WrongToken(other)),
                    }
                }
                Expr::NewMap(acc)
            }
            other => return Err(Error::WrongToken(other)),
        };

        let mut acc = Vec::new();
        loop {
            if let Some(Token::Bracket('[')) = self.0.peek()? {
                self.0.skip()?;
            } else {
                if acc.is_empty() {
                    break;
                }
                return Ok(Expr::GetField(Box::new(atom), acc));
            }

            if let Some(Token::Colon) = self.0.peek()? {
                let lhs = Expr::Const(Type::Number(Number::ZERO));
                self.0.skip()?;
                if let Some(Token::Bracket(']')) = self.0.peek()? {
                    self.0.skip()?;
                    return Ok(Expr::Slice(Box::new(atom), Box::new(lhs), None));
                }
                let key = self.expr()?;
                self.expect(Token::Bracket(']'))?;

                if acc.is_empty() {
                    return Ok(Expr::Slice(
                        Box::new(atom),
                        Box::new(lhs),
                        Some(Box::new(key)),
                    ));
                } else {
                    return Ok(Expr::Slice(
                        Box::new(Expr::GetField(Box::new(atom), acc)),
                        Box::new(lhs),
                        Some(Box::new(key)),
                    ));
                }
            }

            let lhs = self.operation()?;

            let Some(token) = self.0.peek()? else {
                return Err(Error::EndOfInput);
            };

            match token {
                Token::Colon => {
                    self.0.skip()?;
                    if let Some(Token::Bracket(']')) = self.0.peek()? {
                        self.0.skip()?;
                        if acc.is_empty() {
                            return Ok(Expr::Slice(Box::new(atom), Box::new(lhs), None));
                        } else {
                            return Ok(Expr::Slice(
                                Box::new(Expr::GetField(Box::new(atom), acc)),
                                Box::new(lhs),
                                None,
                            ));
                        }
                    }
                    let rhs = self.operation()?;
                    self.expect(Token::Bracket(']'))?;
                    if acc.is_empty() {
                        return Ok(Expr::Slice(
                            Box::new(atom),
                            Box::new(lhs),
                            Some(Box::new(rhs)),
                        ));
                    } else {
                        return Ok(Expr::Slice(
                            Box::new(Expr::GetField(Box::new(atom), acc)),
                            Box::new(lhs),
                            Some(Box::new(rhs)),
                        ));
                    }
                }
                Token::Bracket(']') => {
                    self.0.skip()?;
                    acc.push(lhs)
                }
                other => return Err(Error::WrongToken(other)),
            };
        }

        Ok(atom)
    }

    fn maybe_apply(&mut self, name: String) -> Result<Expr> {
        match self.0.peek()? {
            Some(Token::Bracket('(')) => {
                self.0.skip()?;
                let mut args = self.read_until(Token::Bracket(')'))?;
                // optimization: pre-compile regex if possible
                if (name == "sub" || name == "re")
                    && let Some(Expr::Const(Type::String(regex))) = args.get(1)
                {
                    let regex = Regex::from_str(regex)?;
                    args[1] = Expr::Const(Type::Regex(regex))
                }
                Ok(Expr::Apply(name, args))
            }
            _ => Ok(Expr::Get(Variable::Local(name))),
        }
    }

    fn case(&mut self) -> Result<Expr> {
        let expr = self.expr()?;
        self.expect(Token::Bracket('{'))?;
        self.ignore_newlines()?;
        let mut acc = Vec::new();
        loop {
            let mut cond = Vec::new();
            loop {
                cond.push(self.expr()?);
                match self.0.next_or_err()? {
                    Token::Delimiter('|') => self.ignore_newlines()?,
                    Token::Bracket('{') => break,
                    other => return Err(Error::WrongToken(other)),
                }
            }
            let body = self.read_until(Token::Bracket('}'))?;
            acc.push((cond, body));

            match self.0.next_or_err()? {
                Token::Delimiter(';') | Token::Delimiter('\n') => {
                    self.ignore_newlines()?;
                    if let Some(Token::Bracket('}')) = self.0.peek()? {
                        self.0.skip()?;
                        break;
                    }
                }
                Token::Bracket('}') => break,
                other => return Err(Error::WrongToken(other)),
            }
        }
        Ok(Expr::Case(Box::new(expr), acc))
    }

    fn read_until(&mut self, delim: Token) -> Result<Vec<Expr>> {
        let mut body = Vec::new();

        // empty body
        if let Some(token) = self.0.peek()?
            && token == delim
        {
            self.0.skip()?;
            return Ok(body);
        }

        loop {
            self.ignore_newlines()?;
            let expr = self.expr()?;
            body.push(expr);
            self.ignore_newlines()?;

            match self.0.next_or_err()? {
                Token::Delimiter(',') => (),
                token if token == delim => break,
                other => return Err(Error::WrongToken(other)),
            }
        }
        Ok(body)
    }

    fn operation(&mut self) -> Result<Expr> {
        self.expr_bp(0)
    }

    fn expr_bp(&mut self, min_bp: u8) -> Result<Expr> {
        // Pratt parser
        // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
        let mut lhs = if let Some(Token::Op(op @ (Operator::Add | Operator::Sub | Operator::Not))) =
            self.0.peek()?
        {
            self.0.skip()?;
            let ((), r_bp) = op.prefix_bp();
            let rhs = self.expr_bp(r_bp)?;
            if let Expr::Const(rhs @ Type::Number(val)) = &rhs {
                match op {
                    Operator::Add => return Ok(Expr::Const(rhs.clone())),
                    Operator::Sub => return Ok(Expr::Const(Type::Number(-*val))),
                    _ => (),
                }
            }
            Expr::UnaryOp(op, Box::new(rhs))
        } else {
            self.atom()?
        };

        loop {
            let op = match self.0.peek()? {
                Some(Token::Op(Operator::Not)) => break,
                Some(Token::Op(op)) => op,
                // end of expression
                None => break,
                Some(ref t) if is_boundary(t) => break,
                Some(_) => break,
            };

            // inflix
            let (l_bp, r_bp) = op.inflix_bp();
            if l_bp < min_bp {
                break;
            }
            self.0.skip()?;

            let rhs = self.expr_bp(r_bp)?;
            lhs = Expr::Op(op, Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn printer(&mut self) -> Result<Print> {
        let res = match self.0.peek()? {
            Some(Token::String(ref s)) => {
                self.0.skip()?;
                let t = Template::from_str(s)?;
                Print::Template(t)
            }
            Some(Token::Ident(s)) if s == "json" => {
                self.0.skip()?;
                Print::Json
            }
            Some(Token::Ident(s)) if s == "pretty" => {
                self.0.skip()?;
                Print::PrettyJson
            }
            Some(Token::Ident(s)) if s == "logfmt" => {
                self.0.skip()?;
                Print::Logfmt
            }
            Some(Token::Template(ref s)) => {
                self.0.skip()?;
                let template = Template::from_str(s)?;
                Print::Template(template)
            }
            _ => Print::Expr(Box::new(self.expr()?)),
        };
        Ok(res)
    }

    fn matcher(&mut self) -> Result<Option<Match>> {
        let val = match self.0.peek()? {
            Some(Token::Ident(s)) if s == "json" => {
                self.0.skip()?;
                Match::Json
            }
            Some(Token::Ident(s)) if s == "logfmt" => {
                self.0.skip()?;
                Match::Logfmt
            }
            Some(Token::Ident(s)) if s == "csv" || s == "tsv" => {
                self.0.skip()?;
                self.expect(Token::Bracket('('))?;
                let mut columns = Vec::new();
                if let Some(Token::Bracket(')')) = self.0.peek()? {
                    self.0.skip()?;
                } else {
                    loop {
                        match self.0.next_or_err()? {
                            Token::Ident(name) => columns.push(Some(name.to_string())),
                            Token::Wildcard => columns.push(None),
                            other => return Err(Error::WrongToken(other)),
                        }
                        match self.0.next_or_err()? {
                            Token::Bracket(')') => break,
                            Token::Delimiter(',') => (),
                            other => return Err(Error::WrongToken(other)),
                        }
                    }
                }
                if s == "csv" {
                    Match::Csv(columns)
                } else {
                    Match::Tsv(columns)
                }
            }
            Some(Token::Regex(ref s)) => {
                self.0.skip()?;
                let regex = Regex::from_str(s)?;
                if regex.is_capturing() {
                    Match::Regex(regex)
                } else {
                    Match::RegexMatches(regex)
                }
            }
            Some(Token::Template(ref s)) => {
                self.0.skip()?;
                let template = Template::from_str(s)?;
                let regex = Regex::try_from(template)?;
                if regex.is_capturing() {
                    Match::Regex(regex)
                } else {
                    Match::RegexMatches(regex)
                }
            }
            _ => return Ok(None),
        };
        Ok(Some(val))
    }

    fn ignore_newlines(&mut self) -> Result<()> {
        while let Some(Token::Delimiter('\n')) = self.0.peek()? {
            self.0.skip()?;
        }
        Ok(())
    }

    fn expect(&mut self, expected: Token) -> Result<()> {
        match self.0.next()? {
            Some(token) if token == expected => Ok(()),
            Some(other) => Err(Error::WrongToken(other)),
            None => Err(Error::EndOfInput),
        }
    }
}

impl Operator {
    fn inflix_bp(self) -> (u8, u8) {
        use Operator::*;
        match self {
            // Assign => (2, 1),
            Or => (3, 4),
            And => (5, 6),
            Matches | NotMatches | In => (8, 7),
            Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual => (9, 10),
            Add | Sub => (11, 12),
            Mul | Div | Mod => (13, 16),
            Pow => (20, 19),
            Join => (21, 22),
            _ => unreachable!(),
        }
    }

    fn prefix_bp(self) -> ((), u8) {
        use Operator::*;
        match self {
            Add | Sub | Not => ((), 18),
            _ => unreachable!(),
        }
    }
}

fn is_boundary(token: &Token) -> bool {
    use Token::*;
    matches!(
        token,
        Delimiter(_) | Bracket(_) | Colon | Question | Assign | AddAssign
    )
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::{
        parser::{code_reader::StringReader, tokenizer::Tokenizer},
        types::{Block, Expr, Match, Print, Regex, Template, Type, Variable},
    };
    use std::str::FromStr;
    use test_case::test_case;

    #[test_case(
        "42",
        "42";
        "constant"
    )]
    #[test_case(
        "foo(x, 1+2/3, bar(), \"string\")",
        "foo(x, (1 + (2 / 3)), bar(), \"string\")";
        "application"
    )]
    #[test_case(
        "(1 + 2.3e-3) / X - -4.501",
        "(((1 + 0.0023) / X) - -4.501)";
        "arithmetic"
    )]
    #[test_case(
        "(1+2.3e-3)/X--4.501",
        "(((1 + 0.0023) / X) - -4.501)";
        "arithmetic no spaces"
    )]
    #[test_case(
        "x = 1 + 2 / 3",
        "x = (1 + (2 / 3))";
        "assign"
    )]
    #[test_case(
        "A and B or C and not D or not E and F",
        "(((A and B) or (C and not(D))) or (not(E) and F))";
        "logic"
    )]
    #[test_case(
        "[1,\n1+2,2/5*7-8,[]\n]",
        "[1, (1 + 2), (((2 / 5) * 7) - 8), []]";
        "list"
    )]
    #[test_case(
        "{\nfoo: 1,\nbar: 1+2,baz:2/5*7-8,\nqux:{}\n}",
        "{foo: 1, bar: (1 + 2), baz: (((2 / 5) * 7) - 8), qux: {}}";
        "map"
    )]
    fn expr(input: &str, expected: &str) {
        let reader = StringReader::from(input.to_string());
        let tokenizer = Tokenizer::new(reader);
        let mut parser = Parser(tokenizer);
        let result = parser.expr().unwrap();
        assert_eq!(format!("{}", result), expected)
    }

    #[test_case(
        "",
        Block::default();
        "blank"
    )]
    #[test_case(
        "match(~'<all>'), print(~'result=<all>')",
        Block(vec![
            Expr::Match(Match::Regex(Regex::try_from(Template::from_str("<all>").unwrap()).unwrap())),
            Expr::Print(Print::Template(Template::from_str("result=<all>").unwrap()))
        ]);
        "templates on parsing and formatting"
    )]
    #[test_case(
        "match(^$)",
        Block(vec![Expr::Match(Match::RegexMatches(Regex::from_str("^$").unwrap()))]);
        "empty full line regex"
    )]
    #[test_case(
        "s = sub(., /[a-z]/, 'X'), print(s)",
        Block(vec![
            Expr::Set(Variable::Local("s".to_string()), Vec::new(), Box::new(Expr::Apply(
                "sub".to_string(),
                vec![
                    Expr::Get(Variable::Line),
                    Expr::Const(Type::Regex(Regex::from_str("[a-z]").unwrap())),
                    Expr::Const(Type::String("X".to_string())),
                ]
            ))),
            Expr::Print(Print::Expr(Box::new(Expr::Get(Variable::Local("s".to_string()))))),
        ]);
        "regex in sub"
    )]
    fn instruction(input: &str, expected: Block) {
        let mut parser = Parser::from(input.to_string());
        let result = parser.block().unwrap();
        assert_eq!(result, expected)
    }
}
