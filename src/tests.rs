use crate::{
    eval::{self, Context},
    parser::{Parser, StringReader, Tokenizer},
    reader::Reader,
    types::Map,
};
use test_case::test_case;

pub(crate) struct WriteMock;

impl std::io::Write for WriteMock {
    fn write(&mut self, _: &[u8]) -> std::io::Result<usize> {
        Ok(0)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[test_case(
        "flatten([])",
        "[]";
        "flatten empty"
    )]
#[test_case(
        "flatten([[],[[]]])",
        "[]";
        "flatten array of empty arrays"
    )]
#[test_case(
        "flatten([1, [2], [3,4], [[5], [6, [7, 8]]]])",
        "[1, 2, 3, 4, 5, 6, 7, 8]";
        "flatten array of nested arrays"
    )]
#[test_case(
        "flatten({})",
        "{}";
        "flatten empty map"
    )]
#[test_case(
        "flatten({foo: 1, bar: {baz: 2, fiz: {qux: 3, nil: {} }}})",
        "{\"foo\": 1, \"bar.baz\": 2, \"bar.fiz.qux\": 3, \"bar.fiz.nil\": {}}";
        "flatten non-empty map"
    )]
#[test_case(
        "unique([])",
        "[]";
        "unique empty"
    )]
#[test_case(
        "unique([1,9,2,1,3,3,2,4,1,5,9])",
        "[1, 9, 2, 3, 4, 5]";
        "unique"
    )]
#[test_case(
        "sort([1,9,1,3,3,2,4,1,5,9])",
        "[1, 1, 1, 2, 3, 3, 4, 5, 9, 9]";
        "sort"
    )]
#[test_case(
        "sqrt(2 * 2) == 2",
        "true";
        "sqrt"
    )]
#[test_case(
        "abs(123)",
        "123";
        "abs"
    )]
#[test_case(
        "round(3.14159, 2)",
        "\"3.14\"";
        "round"
    )]
#[test_case(
        "split('abc', '')",
        "[\"a\", \"b\", \"c\"]";
        "split with empty sep"
    )]
#[test_case(
        "num('3.14')",
        "3.14";
        "to number"
    )]
#[test_case(
        "[1, 2, _] == [_, 2, 3]",
        "true";
        "compare lists with wildcards"
    )]
#[test_case(
        "{a: 1, b: _} == {b: 2, a: 1}",
        "true";
        "compare maps with wildcards"
    )]
#[test_case(
        "{a: 1, b: 2} == {b: 2, a: 1}",
        "true";
        "compare maps"
    )]
#[test_case(
        "''[:]",
        "\"\"";
        "slice empty string"
    )]
#[test_case(
        "[1,2,3][:]",
        "[1, 2, 3]";
        "full slice of array"
    )]
#[test_case(
        "[0,1,2,3,4,5,6][3:5]",
        "[3, 4, 5]";
        "slice of array"
    )]
#[test_case(
        "'abcdefg'[3:5]",
        "\"def\"";
        "slice of string"
    )]
#[test_case(
        "'abcdefg'[:2]",
        "\"abc\"";
        "left-open slice of string"
    )]
#[test_case(
        "'abcdefg'[4:]",
        "\"efg\"";
        "right-open slice of string"
    )]
#[test_case(
        "upper('abcdefg')[3:5]",
        "\"DEF\"";
        "slice of function return"
    )]
#[test_case(
        "'abcdefg'[3]",
        "\"d\"";
        "get nth char of string"
    )]
#[test_case(
        "[1,2,3][100:200]",
        "[]";
        "slice out-of-bounds for array"
    )]
#[test_case(
        "'abc'[100:200]",
        "\"\"";
        "slice out-of-bounds for string"
    )]
#[test_case(
        "'abcdefg'[100]",
        "\"\"";
        "get char out of range of string"
    )]
#[test_case(
        "12345[2]",
        "\"3\"";
        "get nth char of number"
    )]
#[test_case(
        "{ a: (1+2+3)/2, b: 2 }['a']",
        "3";
        "get from map"
    )]
#[test_case(
        "[] ++ []",
        "[]";
        "join empty arrays"
    )]
#[test_case(
        "[1,2] ++ [3,4,5]",
        "[1, 2, 3, 4, 5]";
        "join arrays"
    )]
#[test_case(
        "{a: 1, b: 2} ++ {c: 3}",
        "{\"a\": 1, \"b\": 2, \"c\": 3}";
        "join maps"
    )]
#[test_case(
        "'ac' ++ '/' ++ 'dc'",
        "\"ac/dc\"";
        "join strings"
    )]
#[test_case(
        "sub('abracadabra', '[a-c]', '#')",
        "\"##r###d##r#\"";
        "sub without limit"
    )]
#[test_case(
        "sub('abracadabra', '[a-c]', '#', 3)",
        "\"##r#cadabra\"";
        "sub with limit"
    )]
#[test_case(
        "if 2 + 2 == 4 { 'o'++'k' } else { 'wrong' }",
        "\"ok\"";
        "if else positive cond"
    )]
#[test_case(
        "if 2 + 2 != 4 { 'wrong' } else { 'o'++'k' }",
        "\"ok\"";
        "if else negative cond"
    )]
#[test_case(
        "if true { 'ok' } else { error('wrong') }",
        "\"ok\"";
        "if else ignores branch"
    )]
#[test_case(
        "if true { if false { 1 } else { 2 } } else { 3 }",
        "2";
        "nested if else"
    )]
#[test_case(
        "case 2+2 == 4 { true { 'ok' }; false { 'wrong' } }",
        "\"ok\"";
        "simple match"
    )]
#[test_case(
        "case 2+3 { 0 { 'incorrect' }; 10/2 { 'ok' }; _ { 'also wrong' } }",
        "\"ok\"";
        "case with exprs"
    )]
#[test_case(
        "case {a:1,b:2} { {} { 'incorrect' }; null { 'wrong' }; {b:4/2,a:_} { 'ok' }; _ { 'also wrong' } }",
        "\"ok\"";
        "case maps"
    )]
#[test_case(
        "case 2+3 { 0 | null | 5+1 { 'incorrect' }; 10/2 | 5 { 'ok' }; _ { 'also wrong' } }",
        "\"ok\"";
        "case alternate conditions"
    )]
#[test_case(
        "str(null)",
        "\"\"";
        "null to string"
    )]
#[test_case(
        "bool('true')",
        "true";
        "bool from string"
    )]
#[test_case(
        "'' + 5",
        "NaN";
        "add string to number"
    )]
#[test_case(
        "num('') + 5",
        "5";
        "add string to number with cast"
    )]
#[test_case(
        "unescape('\n\thello!\n')",
        "\"\n\thello!\n\"";
        "unescape string"
    )]
#[test_case(
        "re('127.0.0.1', '(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)')",
        "[\"127.0.0.1\", \"127\", \"0\", \"0\", \"1\"]";
        "regex extract"
    )]
#[test_case(
        "rev('bad')",
        "\"dab\"";
        "rev string"
    )]
#[test_case(
        "keys({a:1,b:2,c:3})",
        "[\"a\", \"b\", \"c\"]";
        "keys"
    )]
#[test_case(
        "vals({a:1,b:2,c:3})",
        "[1, 2, 3]";
        "vals"
    )]
#[test_case(
        "if true { false, true }",
        "false";
        "assertions work in bodies"
    )]
#[test_case(
        "[1,2,3,4,5][-3]",
        "3";
        "negative indexes"
    )]
#[test_case(
        "[[1,2,3,4,5]][-1][-3]",
        "3";
        "negative indexes when nested"
    )]
#[test_case(
        "'hello!'[-2]",
        "\"o\"";
        "negative indexes for string"
    )]
fn eval_expr(input: &str, expected: &str) {
    let mut parser = Parser::from(input.to_string());
    let expr = parser.expr().unwrap();
    let mut ctx = Context::new(WriteMock {});
    let data = &mut Map::new();
    let result = eval::eval(&expr, data, &mut ctx).unwrap();
    assert_eq!(format!("{:?}", result), expected)
}

#[test]
#[cfg_attr(windows, ignore)]
fn env_var() {
    let mut parser = Parser::from("$USER".to_string());
    let expr = parser.expr().unwrap();
    let mut ctx = Context::new(WriteMock {});
    let data = &mut Map::new();
    let result = eval::eval(&expr, data, &mut ctx).unwrap();
    assert_eq!(format!("{}", result), std::env::var("USER").unwrap())
}

#[test]
fn error() {
    let mut parser = Parser::from("error('hello, world!')".to_string());
    let expr = parser.expr().unwrap();
    let mut ctx = Context::new(WriteMock {});
    let data = &mut Map::new();
    let result = eval::eval(&expr, data, &mut ctx);
    let Err(msg) = result else {
        unreachable!("example did not return an error")
    };
    assert_eq!(msg.to_string(), "hello, world!")
}

#[test_case(
        "end { print('hello, world!') }",
        "hello, world!\n";
        "trivial"
    )]
#[test_case(
        "end { msg = 'hello, world!', print(~'<msg>') }",
        "hello, world!\n";
        "variable"
    )]
#[test_case(
        r"fn hello(who) { 'hello, ' ++ who ++ '!' }
        end { msg = hello('world'), print(~'<msg>') }",
        "hello, world!\n";
        "custom function"
    )]
fn end(input: &str, expected: &str) {
    let mut parser = Parser::from(input.to_string());
    let program = parser.parse().unwrap();

    let mut out: Vec<u8> = Vec::new();
    let reader = Reader::from(input.as_bytes());
    program.process_reader(reader, &mut out).unwrap();

    assert_eq!(String::from_utf8_lossy(&out), expected)
}

#[test_case(
        "2 + 2 == 5, print('wrong!')";
        "no match"
    )]
#[test_case(
        "match(~'something else'), print('wrong!')";
        "filtering by constant negative case"
    )]
#[test_case(
        "match(/msg=wat/), print('wrong!')";
        "filtering negative case"
    )]
fn instruction_negative(input: &str) {
    let reader = StringReader::from(input.to_string());
    let tokenizer = Tokenizer::new(reader);
    let mut parser = Parser(tokenizer);
    let block = parser.block().unwrap();
    let mut out: Vec<u8> = Vec::new();
    let mut ctx = Context::new(&mut out);
    ctx.index = 1;
    ctx.line = "hello, world!".to_string();
    assert!(!block.run(&mut ctx).unwrap());
    assert!(out.is_empty());
}

#[test_case(
        "print(.)",
        "prefix msg=hello obj=world";
        "print line"
    )]
#[test_case(
        "match(/msg=(?<msg>.*) obj=(?<obj>.*)/), print(json)",
        "{\"msg\":\"hello\",\"obj\":\"world\"}";
        "parse regex and format json"
    )]
#[test_case(
        "match(logfmt), print(json)",
        "{\"msg\":\"hello\",\"obj\":\"world\"}";
        "parse logfmt and format json"
    )]
#[test_case(
        "match(~'msg=<msg> obj=<obj>'), msg =~ 'hello', obj = 'earth', print(logfmt)",
        "msg=hello obj=earth";
        "parse template and format as logfmt"
    )]
#[test_case(
        "match(logfmt), print(logfmt)",
        "msg=hello obj=world";
        "logfmt"
    )]
#[test_case(
        "match(logfmt), obj = upper(obj), print(logfmt)",
        "msg=hello obj=WORLD";
        "procedures"
    )]
#[test_case(
        "out = .[7:], print(logfmt)",
        "out=\"msg=hello obj=world\"";
        "dot extract line"
    )]
#[test_case(
        "map = {}, map['foo'] = 42, arr = [1], arr[0] = 2, print(logfmt)",
        "map=\"{\\\"foo\\\":42}\" arr=\"[2]\"";
        "map assign"
    )]
#[test_case(
        "&foo = 42, &bar = { x: &foo + 1 }, out = &bar['x'] + 10, print(logfmt)",
        "out=53";
        "global variable is not printed"
    )]
#[test_case(
        "&m = {a:1}, before = &m['a'], &m['a'] = 2, after = &m['a'], print(logfmt)",
        "before=1 after=2";
        "get set maps"
    )]
#[test_case(
        "&m = {a:{b:1}}, before = &m['a']['b'], &m['a']['b'] = 2, after = &m['a']['b'], print(logfmt)",
        "before=1 after=2";
        "get set nested maps"
    )]
#[test_case(
        "&m = {a:[1,2,3,4,5]}, out = &m['a'][1:2], print(logfmt)",
        "out=\"[2,3]\"";
        "get from map and slice"
    )]
#[test_case(
        "match(/msg=hello/), print('ok')",
        "ok";
        "filtering"
    )]
#[test_case(
        "match(~'prefix msg=hello obj=world'), print('ok')",
        "ok";
        "filtering by constant"
    )]
#[test_case(
        "`logfmt` = 'ok', print(`logfmt`)",
        "ok";
        "quoted identifier"
    )]
#[test_case(
        ". = .[0:2], print(.)",
        "pre";
        "replace line"
    )]
fn instruction(input: &str, expected: &str) {
    let reader = StringReader::from(input.to_string());
    let tokenizer = Tokenizer::new(reader);
    let mut parser = Parser(tokenizer);
    let block = parser.block().unwrap();
    let mut out: Vec<u8> = Vec::new();
    let mut ctx = Context::new(&mut out);
    ctx.index = 13;
    ctx.line = "prefix msg=hello obj=world".to_string();
    assert!(block.run(&mut ctx).unwrap());
    assert_eq!(String::from_utf8_lossy(&out), format!("{expected}\n"));
}

#[test]
fn parse_csv() {
    let mut parser = Parser::from("match(csv(a,b,_,c,d,_,e)), print(logfmt)".to_string());
    let block = parser.block().unwrap();
    let mut out: Vec<u8> = Vec::new();
    let mut ctx = Context::new(&mut out);
    ctx.index = 1;
    ctx.line = "foo,\"bar\",42,3.14,\"hello, world!\",baz,null".to_string();
    let expected = "a=foo b=bar c=3.14 d=\"hello, world!\" e=null\n";
    assert!(block.run(&mut ctx).unwrap());
    assert_eq!(String::from_utf8_lossy(&out), expected);
}

#[test]
fn parse_tsv() {
    let mut parser = Parser::from("match(tsv(a,b,_,c,d,_,e)), print(logfmt)".to_string());
    let block = parser.block().unwrap();
    let mut out: Vec<u8> = Vec::new();
    let mut ctx = Context::new(&mut out);
    ctx.index = 1;
    ctx.line = "foo\tbar\t42\t3.14\thello, world!\tbaz\tnull".to_string();
    let expected = "a=foo b=bar c=3.14 d=\"hello, world!\" e=null\n";
    assert!(block.run(&mut ctx).unwrap());
    assert_eq!(String::from_utf8_lossy(&out), expected);
}

#[test]
fn count_total() {
    let input = r#"
            x=1 message="hello, world!"
            x=12 message=
            x=123 message=hi
            This is a completely different formatting.
            "#;

    let code = r#"
        var &acc = 0
        match(/x=(?<x>\d+)/), &acc += num(x)
        end { print(~"total=<&acc>") }
        "#;

    let mut parser = Parser::from(code.to_string());
    let program = parser.parse().unwrap();

    let mut out: Vec<u8> = Vec::new();
    let reader = Reader::from(input.as_bytes());
    program.process_reader(reader, &mut out).unwrap();

    assert_eq!(String::from_utf8_lossy(&out), "total=136\n");
}

#[test_case(
    r#"
    msg = case [. % 3, . % 5] {
            [0, 0] { "Fizz Buzz" }
            [0, _] { "Fizz" }
            [_, 0] { "Buzz" }
            _else  { . }
        }, print(msg)
    "#;
    "direct match"
)]
#[test_case(
    r#"
    fn fizzbuzz(x) {
        case [x % 3, x % 5] {
            [0, 0] { "Fizz Buzz" }
            [0, _] { "Fizz" }
            [_, 0] { "Buzz" }
                _   { x }
        }
    }
    print(~"<fizzbuzz(.)>")
    "#;
    "fn match"
)]
#[test_case(
    r#"
    . % 3 == 0, . % 5 == 0, print("Fizz Buzz"), next ;
    . % 3 == 0, print("Fizz"), next ;
    . % 5 == 0, print("Buzz"), next ;
    print(~"<.>") ;
    "#;
    "instructions"
)]
fn fizz_buzz(code: &str) {
    let input = "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n";
    let expected = "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizz Buzz\n16\n17\nFizz\n19\nBuzz\n";

    let mut parser = Parser::from(code.to_string());
    let program = parser.parse().unwrap();

    let mut out: Vec<u8> = Vec::new();
    let reader = Reader::from(input.as_bytes());
    program.process_reader(reader, &mut out).unwrap();

    assert_eq!(String::from_utf8_lossy(&out), expected);
}

#[test_case(
    "N == 1, print(.); N == 2, print(.)";
    "line indexing"
)]
#[test_case(
    "N >= 1, N < 3, print(.)";
    "body statements"
)]
#[test_case(
    "match(^[12]$), print(.)";
    "regex filtering"
)]
#[test_case(
    "match(^[12]$), print(.)";
    "regex in body"
)]
#[test_case(
    "match(/(?<x>\\d+)/), x >= 1, x < 3, print(.)";
    "filtering by line content"
)]
#[test_case(
    "case . { 1 | 2 { true }; _ { false } }, print(.)";
    "filtering by content match"
)]
#[test_case(
    r"
    N > 0, N < 3, print(.), next
    N > 0, exit
    ";
    "using actions"
)]
#[test_case(
    "N < 3, match(^[1-9]$), print(.)";
    "index and regex"
)]
#[test_case(
    "between(. == 1, . == 2), print(.)";
    "between"
)]
fn line_filtering(code: &str) {
    let input = "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n";
    let expected = "1\n2\n";

    let mut parser = Parser::from(code.to_string());
    let program = parser.parse().unwrap();

    let mut out: Vec<u8> = Vec::new();
    let reader = Reader::from(input.as_bytes());
    program.process_reader(reader, &mut out).unwrap();

    assert_eq!(String::from_utf8_lossy(&out), expected);
}
