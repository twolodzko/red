# $\color{Red}\Large{\textbf{red}}$

*Like sed, or AWK, but in Rust, so it's red.*

The language consists of blocks of expressions:

```text
expr1, expr2, ... ;
```

each expression is considered as an assertion, if it fails, the execution of a block is aborted
and next block is executed. Assertion fails when it returns `false`, e.g. when failing to match
or parse the input, or when a logical expression returns false.

The expressions can do different things:

* Filter lines, e.g. by line number (`N == 5`) or content (`. =~ /foobar/`).
* Parse them, e.g. `match(json)` or `match(~"time=<time> <_> message=<data>")`.
* Use assertions for further filtering, e.g. `value >= 100`.
* Transform the data, e.g. `transformed = sub(text, /[0-9]/, "X")`.
* Print the data, e.g. `print(msg)`, `print(logfmt)`, or `print(~"result=<&count>")`.
* Aggregate some results, e.g. `if isnum(x) { &total += x }`.

It is inspired by sed, AWK, but also Grafana's LogQL, and others. It's main purpose is text processing,
so the data is by default assumed to be strings. Dynamic typing converts the data values to appropriate
types when needed (e.g. to numbers when conducting mathematical operations).

For more details check `red --help`.

## Syntax

```text
# comment
fn name( arg, ... ) { expr, ... } ;
var &name = expr ;
expr1, expr2, ... ;
end { expr1, expr2, ... ; } ;
```

### Data types

```text
_                          # any (wildcard)
null                       # null
true, false                # bool
0, -768, 1046              # integer
3.14, 1e-579               # float
"foo", 'hello, world!'     # string
[1,2,3, []]                # array
{ a: 1, b: [], c: null }   # map
/regex/, ^regex$           # regular expression
```

### Variables

* `name` - local
* ``` `name` ``` - local (quoted), can be used for variable names containing special characters, or equal to reserved keywords
* `&name` - global variables
* `$name` - environment variables
* `.` - the input line
* `N` - the number of the line processed (counting starts at zero)

### `match()`

Parse command accepts the following arguments:

* `~"<foo> = <bar> ... <_>"` - parse given the template, assign the matched variables to local variables
  based on the names of the matching groups `<name>`.
* `/regex/` or `^regex$` - parse given the regular expressions, assign the matched variables to local variables
  based on the names of the matching groups.
* `json` - parse JSON, assign the fields to local variables.
* `logfmt` - parse logfmt, assign the fields to local variables.
* `csv`, `tsv` - parse CSV or TSV, assign the fields to local variables given the provided names.

it would succeed on successful parsing of the input using the parser.

`between(start, stop)` matches multiple lines starting from the line that matches `start` (a parser or an expression),
and ending on the line that matches `stop` (inclusive). It works in a similar way as AWK's `start,stop` range matching.

### `print()`

Print command accepts the following arguments:

* `json`, `pretty` - format local variables as JSON
* `logfmt` - as above, but logfmt
* or an expression which is evaluated before printing

### Templates

String templates take form of `~"literal <variable> <_>"`, where the `<variable>` part is dynamic, and the `<_>` part is a wildcard pattern which is ignored. It behaves differently depending on where it is used:

* In `match(template)` it is used as a parser, where `<variable>`'s can be names of local variables, the parts of a string are assigned to them, if the whole template pattern matches.
* Otherwise, the template is filled with values of the evaluated `<variable>`'s and the result is collected to a string.

### Actions

* `next` - stop processing current block of commands and move to the next one
* `exit` - stop processing the file and execute the `end` block

### The match operators `=~`, `!~`

The operators treat the right-hand side as a regular expression and try matching the left-hand side string against it.

### The append operator `+=`

The operator is overloaded and behaves differently based on the type of the value on its left-hand side:

* number: it is equivalent to `lhs = lhs + rhs`,
* string: it is equivalent to `lhs = lhs ++ rhs`,
* array: it appends the value to the array, so it's `lhs[len(lhs)+1] = rhs`,
* count: it increments the counter for the `rhs` key, `lhs[rhs] += 1`.

### Conditionals

```text
if cond { yes } else { no }

case what {
  foo {
    ...
  }
  bar { ... }
}
```

### Custom functions

Functions have local variables only, they can read and save global variables.
The local variables are not propagated outside of the function body.

```text
fn name(arg1, arg2, ...) {
  expr1, expr2, ...
}
```

### Iterating and looping

Tail-call optimization allows for looping using recursive function calls.

### `end` block

The `end { ... }` block is executed after all the input lines were processed.
If multiple `end` blocks are declared, they are merged.

### Type casting rules

* In most cases, we would be extracting strings from strings. The values would
  be interpreted as valuers of other types (e.g. numbers), when the calling
  operation or function would explicitly require other types (e.g. mathematical operators).
* All mathematical operations and build-in functions will interpret
  arguments as numbers, otherwise return NaN's.
* The string manipulation build-in functions would try converting the arguments to
  strings, but only for the atomic values, they would throw an error for collections
  (map, array).
* The comparison operators like `==` or `>` will try casting values to
  numbers if the other value is a number; they will treat the values as
  strings if the other value is a string; otherwise they would compare
  the values as-is.
* Some operators (e.g. `++`) or build-in functions (e.g. `rev`) are overloaded
  so will behave differently on different inputs.
* The operators and build-in functions would rather fail with type error
  than try to cast the types.

### red vs AWK

|                              | AWK                                                            | red                                                                   |
|------------------------------|----------------------------------------------------------------|-----------------------------------------------------------------------|
| Print all lines              | `{ print $0 }`                                                 | `print(.)`                                                            |
| Count lines                  | `END { print NR }`                                             | `end { print N }`                                                     |
| Print lines 2-4              | `NR==2, NR==4`                                                 | `between(N==1,N==3), print(.)`                                        |
| Print lines containing "red" | `/red/`                                                        | `. =~ /red/, print(.)`                                                |
| Print line with index        | `{ print NR, $0 }`                                             | `print(~"<N+1> <.>")`                                                 |
| Print specific fields        | `BEGIN { FS=";" } { print $2, $5 }`                            | `a = split(., ";"), print(~"<a[1]> <a[4]>")`                          |
| Find longest line length     | `{ if (length($0) > max) max = length($0) } END { print max }` | `var &max = 0; if (len(.) > &max) &max = len(.); end { print(&max) }` |
| Reverse lines order          | `{ a[i++]=$0 } END { for (j=i-1; j>=0;) print a[j--] }`        | `var &a = []; &a += .; end { print(join(rev(&a), "\n")) }`            |

## Grammar

```text
EOL := "\n" | ";"
NAME := [a-ZA-Z0-9_]
INT    /* rust integer */
FLOAT  /* rust float */
REGEX  /* rust regex */

program := ( ( var | fn | block | end ) EOL )*

var := "var" global "=" const
fn := "fn" ident "(" ( ident ( "," ident )* )? ")" body
block := expr [ "," expr ]+
end := "{" block ( EOL block )+ "}"

expr := value | extract | operation | apply | assign | match | between | print | action | if | case | template
match := "match" "(" matcher ")"
between := "between" "(" ( matcher | expr ) "," ( matcher | expr ) ")"
matcher := regex | template | string | "json" | "logfmt" | "csv"
regex := "/" REGEX "/" | "^" REGEX "$"
template := "~" "\"" ( string | "<" NAME ">" | "<_>" )* "\""
print := "print" "(" formatter ")"
formatter := "json" | "pretty" | "logfmt" | "escape" | logical
action := "next" | "exit"
operation := expr ( op expr )?
op := cmp_op | logical_op | arith_op
cmp_op := "==" | "!=" | "=~" | "!~" | "<" | "<=" | ">" | ">="
logical_op := "and" | "or"
arith_op := "+" | "-" | "*" | "/" | "%"
assign := ident assign_op expr
assign_op := "=" | "+="
value := ident | number | string | "true" | "false" | "null" | "_" | array | map
name := [a-zA-Z] NAME*
ident := name | "`" [^`]* "`"
apply := name "(" expr ( "," expr )* ")"
number := INT | FLOAT
array := "[" ( expr ( "," expr )+ ) "]"
map := "{" ( keyval ( "," keyval )+ ) "}"
keyval := ident ":" expr
string := "\"" [^"]* "\""
extract := expr ( "[" range | expr "]" )+
range := INT ( ":" INT )?
if := "if" expr if_branch ( "else" if_branch )?
if_branch := expr | "{" body "}"
case := "case" expr "{" case_branch ( EOL case_branch )* "}"
case_branch := expr "{" body "}"
body := "{" ( expr ( "," expr )+ )* "}"
```
