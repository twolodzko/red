# red - line editor

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

* `json` - format local variables as JSON
* `pretty` - as above, but pretty JSON
* `logfmt` - as above, but logfmt
* `~"template <key>"` - interpolate the template with the local variables
* `escape` - print line but escape it
* or an expression which is evaluated before printing

### Actions

* `next` - stop processing current block of commands and move to the next one
* `exit` - stop processing the file and execute the `end` block

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

expr := value | extract | operation | apply | assign | match | between | print | action | if | case
match := "match" "(" matcher ")"
between := "between" "(" ( matcher | expr ) "," ( matcher | expr ) ")"
matcher := regex | template | string | "json" | "logfmt" | "csv"
regex := "/" REGEX "/" | "^" REGEX "$"
template := "~" "\"" ( string | "<" NAME ">" | "<_>" )* "\""
print := "print" "(" formatter ")"
formatter := template | "json" | "pretty" | "logfmt" | "escape" | logical
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
