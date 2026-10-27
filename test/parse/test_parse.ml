(* Parser tests (testing plan, part 3.2). Each test prints the tree (`--dump=ast`
   format) and any diagnostics. *)

open Anwl

let parse s =
  let src = Source.of_string ~name:"t.t" s in
  let diag = Diag.create () in
  let toks = Lexer.lex src diag in
  let ast = Parser.parse toks diag in
  (ast, toks, diag)

let show s =
  let ast, toks, diag = parse s in
  print_string (Print_tree.program ast toks);
  List.iter (fun d -> print_endline (Diag.format d)) (Diag.to_list diag)

(* Source printer output. *)
let reprint s =
  let ast, toks, _ = parse s in
  print_string (Print_src.program ast toks)

(* ---------- precedence: one test per row of the grammar doc's table ---------- *)

let%expect_test "1. postfix binds tightest, left to right" =
  show "a.b.c(d)[e]";
  [%expect {| (index (call (get (get a b) c) d) e) |}]

let%expect_test "2. prefix sits below postfix" =
  show "-f(x)\n-x.sz\n!a && b\n~a & b";
  [%expect {|
    (- (call f x))
    (- (get x sz))
    ((! a) && b)
    ((~ a) & b)
    |}]

let%expect_test "2. a leading `-` folds into a number literal" =
  show "-5\n- 2.5\n-(5)\na - 5\n-5.sz";
  [%expect {|
    -5
    -2.5
    (- 5)
    (a - 5)
    (- (get 5 sz))
    |}]

let%expect_test "3. shifts are right-associative and above `*`" =
  show "1 << 2 << 3\na * b << c\na >> b";
  [%expect {|
    (1 << (2 << 3))
    (a * (b << c))
    (a >> b)
    |}]

let%expect_test "4. `* / % & | ^` are one left-associative level" =
  show "a * b / c % d\na & b | c ^ d";
  [%expect {|
    (((a * b) / c) % d)
    (((a & b) | c) ^ d)
    |}]

let%expect_test "5. `+ -` are left-associative, below `*`" =
  show "a - b - c\na + b * c";
  [%expect {|
    ((a - b) - c)
    (a + (b * c))
    |}]

let%expect_test "6. ranges: below arithmetic, `by` binds like the end" =
  show "0..n-1\n0..=10\n0..n by k+1\ni < 0..5";
  [%expect {|
    (0 .. (n - 1))
    (0 ..= 10)
    (0 .. n by (k + 1))
    (i < (0 .. 5))
    |}]

let%expect_test "6. ranges don't chain" =
  show "0..1..2";
  [%expect {|
    <error>
    t.t:1:5
    ranges can't be chained
    |}]

let%expect_test "7. comparisons chain" =
  show "0 <= i < n\na == b == c\na == b < c\nx > y >= z\n(a < b) < c\na != b";
  [%expect {|
    (0 <= i < n)
    (a == b == c)
    (a == b < c)
    (x > y >= z)
    ((a < b) < c)
    (a != b)
    |}]

let%expect_test "7. bad chains: mixed directions, `!=`" =
  show "a < b > c\na != b != c\na != b < c\na < b != c";
  [%expect {|
    <error>
    <error>
    <error>
    <error>
    t.t:1:7
    a chain can't mix `<` and `>`; use `&&`
    t.t:2:8
    `!=` can't be chained; use `&&`
    t.t:3:8
    `!=` can't be chained; use `&&`
    t.t:4:7
    `!=` can't be chained; use `&&`
    |}]

let%expect_test "8-9. `&&` and `||` are right-associative, `&&` tighter" =
  show "a && b && c\na || b && c";
  [%expect {|
    (a && (b && c))
    (a || (b && c))
    |}]

let%expect_test "10. `?:` is right-associative" =
  show "a ? b : c ? d : e\n(a ? b : c) ? d : e";
  [%expect {|
    (?: a b (?: c d e))
    (?: (?: a b c) d e)
    |}]

let%expect_test "11. `break`/`ahket` take everything to their right" =
  show "c ? break x : y\nahket a + b\nbreak\nahket";
  [%expect {|
    (?: c (break x) y)
    (ahket (a + b))
    (break)
    (ahket)
    |}]

(* ---------- disambiguation rules (grammar doc, part 4) ---------- *)

let%expect_test "rule 1: declaration vs expression" =
  show "int x: 5\ntree<int> t: leaf\natalo y: 2\n[int] xs: []\nf(x)\na < b";
  [%expect {|
    (decl int x 5)
    (decl tree<int> t leaf)
    (decl atalo y 2)
    (decl [int] xs (array))
    (call f x)
    (a < b)
    |}]

let%expect_test "rule 2: `x? y: z` is a ternary unless `x` is a type" =
  show "x? y: z\nint? y: z\ntype opt = a | b\nopt? y: z";
  [%expect {|
    (?: x y z)
    (decl int? y z)
    type opt = a | b
    (decl opt? y z)
    |}]

let%expect_test "rule 3: `:` binds, `=` reassigns, `::` defines" =
  show "x: 1\nx = 2\nf :: (a) { a }";
  [%expect {|
    (bind x 1)
    (set x 2)
    (fn:: f (a) (block a))
    |}]

let%expect_test "rule 4: `<` starts an object in prefix position" =
  show "p: <x: 1, y: <z: 2>>\na < b\nq: <>";
  [%expect {|
    (bind p (obj x: 1 y: (obj z: 2)))
    (a < b)
    (bind q (obj))
    |}]

let%expect_test "rule 4: `>` closes an object; `>=`, `?:`, `&&` still work" =
  show "o: <big: (a > b), ge: a >= b, t: c ? 1 : 2, both: a && b>";
  [%expect {| (bind o (obj big: (a > b) ge: (a >= b) t: (?: c 1 2) both: (a && b))) |}]

let%expect_test "rule 4: `>` without parens in an object ends it" =
  show "o: <big: a > b>";
  [%expect {|
    <error>
    t.t:1:14
    expected the end of the statement
    |}]

let%expect_test "rule 5: alias vs ADT" =
  show "type a = int?\ntype b = [int]\ntype c = <x: int>\ntype d = tree<int>\ntype e = leaf\ntype f = a\ntype g<x> = node(x, g<x>) | nothing";
  [%expect {|
    type a = int?
    type b = [int]
    type c = <x: int>
    type d = tree<int>
    type e = leaf
    type f = a
    type g<x> = node(x, g<x>) | nothing
    |}]

let%expect_test "rule 6: patterns" =
  show "katsa (v) { some(x) => x, nil => 0, red => 1, -1 => 2, \"s\" => 3, _ => 4, }";
  [%expect {| (katsa v some(x) => x nil => 0 red => 1 -1 => 2 "s" => 3 _ => 4) |}]

let%expect_test "rule 7: `ahket` takes an operand only if one can start" =
  show "c ? ahket : 1\nahket\nx";
  [%expect {|
    (?: c (ahket) 1)
    (ahket)
    x
    |}]

(* ---------- forms ---------- *)

let%expect_test "functions and every IIFE form" =
  show "otala f(int a, b) -> int { a }\notala<a> id(a x) { x }\ng: otala(x) { x }\nv: { 1 }\nw: otala(x: 1, y: 2) { x + y }\nn: otala go(i: 0) { go(i + 1) }\nm: loop :: (k: 3) { k }";
  [%expect {|
    (fn f (int a, b) -> int (block a))
    (fn<a> id (a x) (block x))
    (bind g (fn (x) (block x)))
    (bind v (block 1))
    (bind w (fn x: 1 y: 2 (block (x + y))))
    (bind n (fn go i: 0 (block (call go (i + 1)))))
    (bind m (fn:: loop k: 3 (block k)))
    |}]

let%expect_test "parameters: all initialised or none" =
  show "otala(x: 1, y) { x }";
  [%expect {|
    <error>
    t.t:1:1
    give every parameter a value, or none
    |}]

let%expect_test "if/then/else, across lines" =
  show "if (a) {\n  1\n}\nthen (b) { 2 }\nelse {\n  3\n}";
  [%expect {| (if a (block 1) then b (block 2) else (block 3)) |}]

let%expect_test "loops" =
  show "for (x of xs) { print(x) }\nfor (ever) with (int n: 0, acc: 1) { break acc }";
  [%expect {|
    (for x xs (block (call print x)))
    (for ever (with int n: 0 acc: 1) (block (break acc)))
    |}]

let%expect_test "quote and unquote" =
  show "q: `{ $x + $(f(y)) }\nbad: $x";
  [%expect {|
    (bind q (quote (block ((unquote x) + (unquote (call f y))))))
    <error>
    t.t:2:6
    `$` only works inside a quote
    |}]

let%expect_test "casts" =
  show "cast<int?>(s)\ncast<tree<int>>(t)\ncast<(int) -> int>(f)";
  [%expect {|
    (cast int? s)
    (cast tree<int> t)
    (cast (int) -> int f)
    |}]

let%expect_test "export and use" =
  show "export otala f(x) { x }\nexport type t = a | b\nexport g :: (x) { x }\nuse m.f\nuse m.g as h\nexport x: 1";
  [%expect {|
    (export (fn f (x) (block x)))
    (export type t = a | b)
    (export (fn:: g (x) (block x)))
    use m.f
    use m.g as h
    <error>
    t.t:6:8
    expected a function or type definition
    |}]

let%expect_test "types" =
  show "(int) -> int? f: g\n((int) -> int)? h: g\n(int) -> (int) -> int k: g\n(int, kl) -> void m: g";
  [%expect {|
    (decl (int) -> int? f g)
    (decl ((int) -> int)? h g)
    (decl (int) -> (int) -> int k g)
    (decl (int, kl) -> void m g)
    |}]

(* ---------- newlines ---------- *)

let%expect_test "newlines inside brackets and after `<`, `>`, `?`" =
  show "xs: [\n  1,\n  2\n]\np: <\n  x: 1\n>\nb: a >\n  c\nt: c ?\n  1 : 2\nf(\n  a\n)";
  [%expect {|
    (bind xs (array 1 2))
    (bind p (obj x: 1))
    (bind b (a > c))
    (bind t (?: c 1 2))
    (call f a)
    |}]

let%expect_test "a newline before `(` ends the statement" =
  show "a\n(b)";
  [%expect {|
    a
    b
    |}]

(* ---------- recovery ---------- *)

let%expect_test "three separate errors are reported, in order" =
  show "x: )\ny: 1\nz: (2\nw: 3\n{ v: ] }\nok: 4";
  [%expect {|
    <error>
    (bind y 1)
    <error>
    (bind w 3)
    (block <error>)
    (bind ok 4)
    t.t:1:4
    expected an expression
    t.t:3:6
    expected `)`
    t.t:5:6
    expected an expression
    |}]

let%expect_test "a statement with two problems reports one" =
  show "x: ) + ]\ny: 2";
  [%expect {|
    <error>
    (bind y 2)
    t.t:1:4
    expected an expression
    |}]

let%expect_test "an unclosed block" =
  show "f: otala() {\n  1";
  [%expect {|
    <error>
    t.t:2:4
    expected `}`
    |}]

let%expect_test "lexer errors aren't reported twice" =
  show "x: 1 é 2";
  [%expect {|
    <error>
    t.t:1:6
    unexpected character `é` (names are ASCII only)
    |}]

(* ---------- source printer ---------- *)

let%expect_test "source printer: minimal parentheses" =
  reprint "(a + b) * c\na - (b - c)\n(a - b) - c\n-(5)\n(-5).sz\n(a < b) < c\n0..(1..2)\n(0..1)..2\n(a ? b : c) ? d : e\n(break x) + 1";
  [%expect {|
    (a + b) * c
    a - (b - c)
    a - b - c
    -(5)
    (-5).sz
    (a < b) < c
    0 .. (1 .. 2)
    (0 .. 1) .. 2
    (a ? b : c) ? d : e
    (break x) + 1
    |}]

let%expect_test "source printer: forms" =
  reprint "otala<a> f(a x, int y) -> a { if (y > 0) { x } else { f(x, y - 1) } }\np: <big: (a > b), c: 1>\nkatsa (s) { circle(r) => r, _ => 0 }\nfor (i of 0..n) with (acc: 0) { acc + i }\ntype t<a> = leaf | node(t<a>, a)\nuse m.f as g";
  [%expect {|
    otala<a> f(a x, int y) -> a {
      if (y > 0) {
        x
      } else {
        f(x, y - 1)
      }
    }
    p: <big: (a > b), c: 1>
    katsa (s) {
      circle(r) => r,
      _ => 0,
    }
    for (i of 0 .. n) with (acc: 0) {
      acc + i
    }
    type t<a> = leaf | node(t<a>, a)
    use m.f as g
    |}]

(* ---------- found in review ---------- *)

let%expect_test "`<<` needs two plain `<`: `a <<= b` isn't a shift" =
  show "a <<= b\nc >>= d";
  [%expect {|
    <error>
    <error>
    t.t:1:4
    expected an expression
    t.t:2:4
    expected an expression
    |}]

let%expect_test "`katsa` needs at least one arm" =
  show "katsa (x) {}";
  [%expect {|
    <error>
    t.t:1:1
    `katsa` needs at least one arm
    |}]

let%expect_test "an unclosed `(` is reported at the end of its line; the next line survives" =
  show "x: (1 + 2\ny: 3\nf(a,\n  b\nz: 4";
  [%expect {|
    <error>
    (bind y 3)
    <error>
    (bind z 4)
    t.t:1:10
    expected `)`
    t.t:4:4
    expected `,` or a closing bracket
    |}]
