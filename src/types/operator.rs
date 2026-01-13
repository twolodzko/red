#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Equal,
    NotEqual,
    Matches,
    NotMatches,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    And,
    Or,
    Not,
    In,
    Join,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operator::*;
        match self {
            Equal => "==",
            NotEqual => "!=",
            Matches => "=~",
            NotMatches => "!~",
            Less => "<",
            LessEqual => "<=",
            Greater => ">",
            GreaterEqual => ">=",
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",
            Pow => "^",
            And => "and",
            Or => "or",
            Not => "not",
            In => "in",
            Join => "++",
        }
        .fmt(f)
    }
}
