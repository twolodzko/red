use clap::Parser;
use red::{Error, Program, parser};
use std::{
    io::{BufRead, BufReader},
    path::PathBuf,
};

const DETAILS: &str = color_print::cstr!(
    r#"<u><s>Language</s></u>

  The language consists of blocks of expressions:

    <s>expr1, expr2, ... ;</s>

  each expression is considered as an assertion, if it fails, the execution of a block is aborted and next block is executed. Assertion fails when it returns <s>false</s>, e.g. when failing to match or parse the input, or when a logical expression returns false.

  The expressions can do different things:

   * Filter lines, e.g. by line number (<s>N == 5</s>) or content (<s>. =~ /foobar/</s>).
   * Parse them, e.g. <s>match(json)</s> or <s>match(~"time=<<time>> <<_>> message=<<data>>")</s>.
   * Use assertions for further filtering, e.g. <s>value >>= 100</s>.
   * Transform the data, e.g. <s>transformed = sub(text, /[0-9]/, "X")</s>.
   * Print the data, e.g. <s>print(msg)</s>, <s>print(logfmt)</s>, or <s>print(~"result=<<&count>>")</s>.
   * Aggregate some results, e.g. <s>if isnum(x) { &total += x }</s>.

  It is inspired by sed, AWK, but also Grafana's LogQL, and others. It's main purpose is text processing, so the data is by default assumed to be strings. Dynamic typing converts the data values to appropriate types when needed (e.g. to numbers when conducting mathematical operations).

  <u><s>Syntax</s></u>
    <s># comment
    fn name( arg, ... ) { expr, ... } ;
    var &name = expr ;
    expr1, expr2, ... ;
    end { expr1, expr2, ... ; } ;</s>

  <u><s>Data types</s></u>
    <s>_</s> (wildcard)
    <s>null</s>
    <s>true</s>, <s>false</s>
    <s>0</s>, <s>-768</s>, <s>1046</s>, <s>3.14</s>, <s>1e-579</s>
    <s>"foo", 'hello, world!'</s>
    <s>[1, 2, 3, []]</s>
    <s>{ a: 1, b: [], c: null }</s>
    <s>/regex/, ^regex$</s>

  <u><s>Variables</s></u>
    <s>name</s> local variable
    <s>`name`</s> local variable (quoted), can be used for variable names containing special characters, or equal to reserved keywords
    <s>&name</s> global variable
    <s>$name</s> environment variable
    <s>.</s> the input line
    <s>N</s> the number of the line processed (counting starts at zero)

  <u><s>match(parser) takes the any of the following as an argument</s></u>
    <s>logfmt</s> parse logfmt, assign the fields to local variables
    <s>json</s> parse JSON, assign the fields to local variables
    <s>csv</s>, <s>tsv</s> parse CSV or TSV, assign the fields to local variables given the provided names
    <s>/regex/</s>, <s>^regex$</s> parse given the regular expressions, assign the matched variables to local variables based on the names of the matching groups
    <s>~"<<value>> <<_>>"</s> parse given the template, assign the matched variables to local variables based on the names of the matching groups <s><<name>></s>

  <s>between(start, stop)</s> matches multiple lines starting from the line that matches <s>start</s> (a parser or an expression), and ending on the line that matches <s>stop</s> (inclusive).

  <u><s>print(formatter) takes the any of the following as an argument</s></u>
    <s>logfmt</s> local variables as lofgmt
    <s>json</s>, <s>pretty</s> local variables as JSON
    or an expression that is executed before printing

  <u><s>Templates</s></u>
    String templates take form of <s>~"literal <<variable>> <<_>>"</s>, where the <s><<variable>></s> part is dynamic, and the <s><<_>></s> part is a wildcard pattern which is ignored. It behaves differently depending on where it is used:
     * In <s>match(template)</s> it is used as a parser, where <s><<variable>></s>'s can be names of local variables, the parts of a string are assigned to them, if the whole template pattern matches.
     * Otherwise, the template is filled with values of the evaluated <s><<variable>></s>'s and the result is collected to a string.

  <u><s>Actions</s></u>
    <s>next()</s> stop processing current block of commands and move to the next one
    <s>exit()</s> stop processing the file and execute the `end` block

  <u><s>Mathematical operators</s></u>
    <s>+</s>, <s>-</s>, <s>*</s>, <s>/</s>, <s>%</s> (remainder), <s>^</s> (power)

  <u><s>Other operators</s></u>
    <s>and</s>, <s>or</s>, <s><<</s>, <s><<=</s>, <s>>></s>, <s>>>=</s>, <s>==</s>, <s>!=</s>, <s>++</s> (join)

  <u><s>The match operators =~, !~</s></u>
    The operators treat the right-hand side as a regular expression and try matching the left-hand side string against it.

  <u><s>The append operator +=</s></u>
    The operator is overloaded and behaves differently based on the type of the value on its left-hand side:
     * number: it is equivalent to <s>lhs = lhs + rhs</s>,
     * string: it is equivalent to <s>lhs = lhs ++ rhs</s>,
     * array: it appends the value to the array, so it is <s>lhs[len(lhs)+1] = rhs</s>,
     * count: it increments the counter for the <s>rhs</s> key, <s>lhs[rhs] += 1</s>.

  <u><s>Conditionals</s></u>
    <s>if cond { yes } else { no }</s>

    <s>case what {
      val1 {
         ...
      }
      val2a | val2b { ... }
    }</s>

  <u><s>Iterating and looping</s></u>
    Tail-call optimization allows for looping using recursive function calls.

  <u><s>Special functions</s></u>
    <s>has(variable)</s> check if <s>variable</s> is initialized
    <s>local()</s> dump local variables to a map object

  <u><s>Build-in functions</s></u>
"#
);

static ABOUT: std::sync::LazyLock<String> = std::sync::LazyLock::new(|| {
    let mut s = String::new();
    s.push_str(DETAILS);
    for (name, _, args, desc) in red::buildins::PROCEDURES.iter() {
        s.push_str(&color_print::cformat!(
            "    <s>{}({})</s> {}\n",
            name,
            args,
            desc
        ));
    }
    s
});

#[derive(clap::Parser)]
#[clap(after_long_help = &*ABOUT)]
struct Args {
    /// Always print the matched line after a successfully executed block
    #[arg(short, long)]
    print_matched: bool,

    #[command(flatten)]
    script: Script,

    /// Files that are processed
    #[arg(name = "FILE")]
    files: Vec<PathBuf>,
}

#[derive(clap::Parser)]
#[group(multiple = true, required = true)]
struct Script {
    /// Commands that are executed
    #[arg(allow_hyphen_values = true)]
    command: Option<String>,

    /// Read the commands from the file
    #[arg(short = 'f', long = "file")]
    path: Option<PathBuf>,
}

fn parse_args() -> Args {
    let mut args = Args::parse();
    if args.script.path.is_some()
        && let Some(arg) = args.script.command
    {
        // it's not a command, dumbo
        args.files.insert(0, arg.into());
        args.script.command = None;
    }
    args
}

fn init(args: &Args) -> Result<Program, Error> {
    if let Some(path) = &args.script.path {
        let mut parser = parser::Parser::try_from(path)?;
        parser.parse()
    } else if let Some(command) = &args.script.command {
        let mut parser = parser::Parser::from(command.to_string());
        parser.parse()
    } else {
        unreachable!()
    }
}

macro_rules! fail {
    ( $msg:expr ) => {{
        eprintln!("{}", $msg);
        std::process::exit(1);
    }};
}

fn main() {
    let args = parse_args();
    let mut program = match init(&args) {
        Ok(program) => program,
        Err(msg) => fail!(msg),
    };
    program.print_matched = args.print_matched;

    let out = &mut std::io::stdout().lock();
    if let Err(msg) = if args.files.is_empty() {
        let inp = BufReader::new(std::io::stdin());
        program.process_reader(inp.lines(), out)
    } else {
        program.process_files(&args.files, out)
    } {
        fail!(msg)
    }
}
