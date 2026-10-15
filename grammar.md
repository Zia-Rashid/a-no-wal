# a'no:wal grammar

## 1. Notation

- EBNF-style
- The grammar is deliberately loose about precedence: `<binary>` and `<unary>` are flat
here. 
- Precedence and associativity in section 3, and the rules in section 4 for where BNF is ambiguous.
 
## 2. Lexical rules

- **Source:** Identifiers and keywords are ASCII, Unicode (including Oneida) is
  allowed inside strings and comments.
- **Comments:** `%%` to the end of the line
- **Statement ends:** `;` or a newline. lines ending in an operator, `,`, `(`, `[` or an unclosed `<` continues.
  A newline before `(` or `[` always ends the statement, so `a` + newline + `(b)` doesnt work 
  for a(b)
  Lexer drops the newline after those (and after `{` `:` `=` `=>` `->` `.` `..` `` ` `` `$`).
  Except `<`, `>`, `?` -> parser decides, since
  `p: <x: 1>` and `type a = int?` end a statement but `a >` and `c ?` don't
- **Numbers** are lexed unsigned-> the parser folds a leading `-` into literal before
  range check. `_` can sit between two digits as visual sep. A float needs digits
  on both sides of `.`, so `0..5` is `0` `..` `5`.
- **`>` tokens** are emitted one at a time with a flag to see
  if it touches the next character. The expr parser will read two touching `>` as `>>`; 
  the type parser reads them as two closers (`tree<tree<int>>`), same goes for `<<`
- **Strings** are normalised at lex time 
- **Pre-pass:** before parsing: i.e. one scan over the tokens to collect every `type <id>`, so
  parser knows all user-declared type names(structs) 

## 3. Operator precedence 

OCaml-based, highest to lowest. Binding powers are for the Pratt loop -> an infix op is
`(left, right)`. Left-assoc means `left < right`, right-assoc means `left > right`.
Non-assoc gets parsed as left-assoc, then a second op from the same level is an error.

| # | Operators | Assoc | Pratt bp | OCaml counterpart |
|---|---|---|---|---|
| 1 | postfix: `e.f`, `e.sz`, `f(…)`, `e[…]` | left | 19 | `.`, application |
| 2 | prefix: `-` `!` `~` | prefix | r = 17 | prefix `-`, `not`, `lnot` |
| 3 | `<<` `>>` | right | (16, 15) | `**…` `lsl` `lsr` `asr` |
| 4 | `*` `/` `%` `&` `\|` `^` | left | (13, 14) | `*…` `/…` `%…` `land` `lor` `lxor` |
| 5 | `+` `-` | left | (11, 12) | `+…` `-…` |
| 6 | `..` `..=`, then optional `by e` | none | (9, 10) | the `::` / `@` `^` level |
| 7 | `==` `!=` `<` `>` `<=` `>=` | chain | (7, 8) | `=…` `<…` `>…` (left in OCaml) |
| 8 | `&&` | right | (6, 5) | `&&` |
| 9 | `\|\|` | right | (4, 3) | `\|\|` |
| 10 | `c ? a : b` | right | (2, 1) | `if … then … else` |
| 11 | `break e`, `ahket e` | prefix, lowest | r = 0 | `raise e` |

deviations from OCaml (and why):
- **Comparisons chain**, `0 <= i < n` means `0 <= i && i < n`. Nesting
  either way (`a < (b < c)` or `(a < b) < c`) would compare a number with a `lihwa`(bool), which
  would be a type error, and OCaml only type-checks it by accident. So the parser collects into 
  one chain node and desugares into `&&`s with the caveats:
  - one direction per chain: `<`/`<=` together, or `>`/`>=` together. `==` can join either
    (`a == b < c`). but `a < b > c` is an error.
  - `!=` doesn't chain. `a != b` is fine, but `a != b != c` wont compares `a` with `c`
    so write `a != b && b != c`
- **Calls and indexing are postfix** (`f(x)`, `xs[i]`) like C/JS, and bind tightest along
  with `.`. Prefix `-` sits below them(orecedence wise), so: `-f(x)` is `-(f(x))`, `-p.x` is `-(p.x)`.
- **`!` is logical not**, not OCaml's deref
- **`..` replaces `::`**, between arithmetic and comparison: `0..n-1` is `0..(n-1)`,
  `i < 0..5` is an error. `by` is strie belonging to the range and is parsed as
  `0..n by k+1` is `0..n by (k+1)`.
- **`?:`**: middle is parsed from bp 0 up to the `:`, else-branch at bp 1, so
  `a ? b : c ? d : e` = `a ? b : (c ? d : e)`.
- **`break e` / `ahket e`** eat everything to their right that can still be an expr,
  stopping at `:`, `,`, `)`, `}` or end of statement. so `c ? break x : y` works.
- **Bitwise** ops: shifts above `*` (like `lsl`), `& | ^` with `*`
  (like `land`/`lor`/`lxor`). `~` is bitwise not. Inside an object literal `>>` needs
  parens, same as `>`.  
Places where a token changes meaning:
- **`<` at the start of an expr** -> object literal. In infix
  position it's less-than.
- **Inside an object literal** field values are parsed with `>` switched off as an op, so
  `>` closes the object. Only `>` needs parens (`<big: (a > b)>`), `&&`, `?:` and `>=`
  work like usual
- **`=>`** in `katsa`(switch) arms, **`|`** in `type` defs, **`:`** in declarations and **`=`** in
  reassignment are separators, not ops. `=` can never be part of an expr

**Types** get their own mini Pratt table:

| # | Form | Assoc |
|---|---|---|
| 1 | postfix `T?`, type arguments `T<…>` | left |
| 2 | `(…) -> T` | right |

`(int) -> int?` is `(int) -> (int?)`. An optional function needs parens: `((int) -> int)?`.
Functions returning functions chain to the right: `(int) -> (int) -> int` is
`(int) -> ((int) -> int)`. `(int, int) -> int` is one arrow taking two args.

## 4. Disambiguation rules

1. **Decl vs expr at the start of a statement.** Try `( <type> | "atalo" ) <id> ":"`
   first. If it works it's a `<declaration>`, otherwise rewind and parse an `<expr>`...
   (tokens are an SoA array so rewinding is just resetting an index)
2. **`x? y: z`** parses both ways. It's a declaration only when `x` is a type keyword or a
   declared type name (from the pre-pass), otherwise it's a ternary.  
3. **`<id> ":"` vs `<id> "="` vs `<id> "::"`** at the start of a statement -> new binding,
   reassignment, or non-recursive function def (or a named IIFE if every param has an
   initial value).
4. **`<`**: in prefix it starts an object literal, in infix position it's LT,
   after a type name or `cast`/`otala`, it opens type args. 
5. **`type t = …`**: typedef if right side is clearly a type, otherwise a constructor list
6. **Patterns:** a bare `<id>` in a `katsa` arm is a constant constructor if the IDs in scope,  
   otherwise it's a binding. `_` in a pattern is the wildcard, in expr its a placeholder
7. **`ahket` / `break`** only take an operand if the next token can start an expr

## 5. Syntax

```bnf
<program> ::= ( <statement> <sep> )* <statement>?

<sep> ::= ";" | <newline>                        // newline ends a statement only if it can end there

<comment> ::= "%%" <any char except newline>*    // single `%` is modulo

<statement> ::= <expr>
              | <declaration>
              | <assignment>
              | <reassign>
              | <func-def>
              | <type-def>
              | <export>
              | <use>

<expr> ::= <number>
         | <string>
         | <boolean>
         | <null>
         | <id>
         | <array>
         | <object>
         | <field-access>
         | <index>
         | <func>
         | <call>
         | <unary>
         | <binary>
         | <ternary>                              // shorthand for <if>
         | <if>                                   // if is an expression
         | <katsa>
         | <placeholder>                          // `_` arguments 
         | <for>                                  // loops are expressions 
         | <range>
         | <break>
         | <return>                               // ahket is an expression 
         | <block>                                // IIFE with no args: { … } has a value
         | <iife>
         | <quote>
         | <unquote>                              // only inside a <quote>
         | <cast>

<declaration> ::= ( <type> | "atalo" ) <id> ":" <expr>         // initialiser required 

<assignment> ::= <id> ":" <expr>                 // new binding (may shadow)

<reassign> ::= <id> "=" <expr>                   // update an existing binding ; type void

<export> ::= "export" ( <func-def> | <type-def> ) // top level only 
<use> ::= "use" <id> "." <id> ( "as" <id> )?      // one function or type per `use`

<cast> ::= "cast" "<" <type> ">" "(" <expr> ")"  // `cast<T?>` gives nil on failure

<return> ::= "ahket" <expr>?                     // `-ahket-` == go back / visit / come back in Oneida
                                                
<break> ::= "break" <expr>?                      // exit loop w/ current state, or with <expr>

<if> ::= "if" "(" <expr> ")" <block>
         ( "then" "(" <expr> ")" <block> )*      // `then` == else-if
         ( "else" <block> )?                     // value = taken branch's block; no else → void

<for> ::= "for" "(" <id> "of" <expr> ")" <state>? <block>   // <expr> : range, array or ahsli
        | "for" "(" "ever" ")" <state>? <block>
                                                 // value = final state, or void without <state>
<state> ::= "with" "(" <state-var> ( "," <state-var> )* ")"   // loop-carried state
<state-var> ::= ( <type> | "atalo" )? <id> ":" <expr>          // initial value; body's last expr = next state

<range> ::= <expr> ( ".." | "..=" ) <expr> ( "by" <expr> )?
                                                 // `..` excludes end, `..=` includes it
                                                 // `by` = stride; default ±1 from direction

<katsa> ::= "katsa" "(" <expr> ")" "{" <arm> ( "," <arm> )* ","? "}"   // pattern match. `kátsaˀ ka·yʌ́` == which one (Oneida)
<arm> ::= <pattern> "=>" ( <expr> | <block> )   // `_` in the body = the value being matched
<pattern> ::= <id> "(" <pattern> ( "," <pattern> )* ")"   // constructor with fields: rect(w, h), some(v)
            | <id>                               // constant constructor if one is in scope
                                                 // (leaf, red), otherwise binds the value
            | "nil"                              // option's empty case (a keyword, not an <id>)
            | <number> | <string> | <boolean>
            | "_"                                // wildcard

<type-def> ::= "type" <id> <type-params>? "=" <variant> ( "|" <variant> )*   // ADT 
             | "type" <id> <type-params>? "=" <type>                         // alias 
                                  // alias when the right side is clearly a type (keyword, [ ], < >,
                                  // ( ) ->, T?, T<…>, or a bare declared type name); else ADT
<type-params> ::= "<" <id> ( "," <id> )* ">"     // type tree<a> = ...
<variant> ::= <id> ( "(" <type> ( "," <type> )* ")" )?   // constructor: circle(kl), leaf

<block> ::= "{" ( <statement> <sep> )* <expr>? <sep>? "}"   // trailing expr = implicit return value

<func> ::= "otala" <type-params>? "(" <parameters> ")" <ret-type>? <block>
                                  // anonymous (\ expression). `oˀta·lá·` == clan in Oneida
<func-def> ::= "otala" <type-params>? <id> "(" <parameters> ")" <ret-type>? <block>
                                                 // otala<a> f(...): fresh `a`, shadows an outer one
             | <id> "::" "(" <parameters> ")" <ret-type>? <block>  // surface only:
                                                 // non-recursive (the name inside means the *previous* binding)
                                                 // `otala <id>` is recursive (letrec)
<ret-type> ::= "->" <type>

<iife> ::= "otala" <id>? "(" <state-var> ( "," <state-var> )* ")" <ret-type>? <block>
         | <id> "::" "(" <state-var> ( "," <state-var> )* ")" <ret-type>? <block>
                                                 // every parameter initialised → called on the spot
                                                 // named form may call itself (named let)

<quote> ::= "`" <block>                          // code as data: a Term handl
<unquote> ::= "$" ( <id> | "(" <expr> ")" )      // run now, splice the resulting Term

<call> ::= <expr> "(" <arguments> ")"            // all arguments at once; `_` = partial application;
                                                 // missing trailing args filled by name from scope 

<field-access> ::= <expr> "." <id>               // typed with row polymorphism
                                                 // except `.sz`: built-in size of ahsli/array/range 

<index> ::= <expr> "[" <expr> "]"                // array element xs[i], or one character s[i] : ahsli

<unary> ::= <unary-op> <expr>

<binary> ::= <expr> <binary-op> <expr>

<ternary> ::= <expr> "?" <expr> ":" <expr>      // == if (e) { a } else { b }

<placeholder> ::= "_"                            // next parameter, left to right
                | "_" <digit>+                   // parameter by position: _0, _1, …
                                                 // surface only: rewritten to `otala` 

<array> ::= "[" <arguments> "]"

<object> ::= "<" <fields> ">"                    // p: <x: 1, y: 2>
                                                 // `>` in a field value needs ( ); 

// none of the type names are set in stone yet
// Level: surface + checker only. The checker rewrites every form into the type core
// (type variable, con name [args], ->, record/row). Core terms carry no types (erased).
<type> ::= "int"
         | "kl"      // "float"  // `-ˀkl-` == to float
         | "lihwa"   // "bool"   // stem for words pertaining to truth, explanation, interpretation
         | "ahsli"   // "string" // `ahsli·yé` == string/thread
         | "void"
         | "range"                // placeholder name; value of `a..b by s`
         | <id> <type-args>?      // user type (shape, tree<int>); undeclared name = type variable
         | "[" <type> "]"         // array         → con array [T]
         | "(" ( <type> ( "," <type> )* )? ")" "->" <type>
                                  // function      → one n-ary arrow, not curried 
         | "<" <type-field> ( "," <type-field> )* ">"
                                  // object        → record (row-polymorphic, 1.1)
         | <type> "?"             // option: `int?` == option<int>; `nil` or a value (checker inserts `some`)

<type-args> ::= "<" <type> ( "," <type> )* ">"
<type-field> ::= <id> ":" <type>

<parameters> ::= ( <param> ( "," <param> )* )?
<param> ::= ( <type> | "atalo" )? <id>          // type annotation optional; `atalo` = "infer" (`-atʌlo-` == friend)
<arguments> ::= ( <expr> ( "," <expr> )* )?
<fields> ::= ( <field> ( "," <field> )* )?
<field> ::= <id> ":" <expr>

<unary-op> ::= "-" | "!" | "~"              // `~` = bitwise not
                                            // precedence and associativity: table in section 3

<binary-op> ::= "+"  | "-"  | "*"  | "/"  | "%"
              | "&"  | "|"  | "^"  | "<<" | ">>"         // bitwise, int only 
              | "==" | "!=" | "<"  | ">"  | "<=" | ">="
              | "&&" | "||"

// Numbers are lways lexed unsigned and the parser folds a leading `-` into the literal.
// `_` may only sit between two digits and is dropped by the lexer.
<number> ::= <integer> | <float>
<integer> ::= <dec-int>                                         // int (32-bit, grows to 64)
            | "0x" <hex-digit> ( "_"? <hex-digit> )*            // 0xff, 0xFF_FF
            | "0b" <bin-digit> ( "_"? <bin-digit> )*            // 0b1010_0101
<dec-int> ::= <digit> ( "_"? <digit> )*                         // 1_000_000; leading 0 is still decimal
<float> ::= <dec-int> "." <dec-int> <exponent>?                 // kl: 1.5, 1.5e-3
          | <dec-int> <exponent>                                // kl: 1e9 (an exponent always means kl)
<exponent> ::= ( "e" | "E" ) ( "+" | "-" )? <dec-int>
<hex-digit> ::= <digit> | "a" | ... | "f" | "A" | ... | "F"
<bin-digit> ::= "0" | "1"

<string> ::= '"' ( <char> | <escape> )* '"'      // UTF-8 allowed inside strings; NFC-normalised 
<escape> ::= "\\" ( "n" | "t" | "r" | "0" | "\\" | '"' )
           | "\\u{" <hex-digit>+ "}"             // any Unicode code point

<boolean> ::= "true" | "false"

<null> ::= "nil"                                 // the empty case of an option type

// Reserved words: never an <id>. Not an expression form on their own.
<keyword> ::= "otala" | "ahket" | "katsa" | "type"                  // definitions, return, match
            | "if" | "then" | "else"                                // conditionals
            | "for" | "of" | "ever" | "with" | "by" | "break"      // loops and ranges
            | "nil" | "true" | "false"                              // literals
            | "int" | "kl" | "lihwa" | "ahsli" | "void" | "range"   // built-in types
            | "atalo"                                               // "infer this" sugar on bindings
            | "cast" | "export" | "use" | "as"                      // casts and modules
// Not keywords: `some` (prelude constructor), `sz` (built-in property, can't be a field name),
// `_` / `_0`… (placeholders). `with`/`by`/`of`/`ever` could become contextual later.

<id> ::= <letter> ( <letter> | <digit> | "_" )* butnot <keyword>  // ASCII only; can't start with `_`

<digit> ::= "0" | "1" | "2" | "3" | "4"
          | "5" | "6" | "7" | "8" | "9"

<letter> ::= "a" | "b" | ... | "z"
           | "A" | "B" | ... | "Z"
```