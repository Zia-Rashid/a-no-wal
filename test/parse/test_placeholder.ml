(* Placeholder rewrite and parent links (testing plan, part 3.3). *)

open Anwl

let rewrite s =
  let src = Source.of_string ~name:"t.t" s in
  let diag = Diag.create () in
  let toks = Lexer.lex src diag in
  let ast = Placeholder.rewrite (Parser.parse toks diag) toks diag in
  print_string (Print_tree.program ast toks);
  List.iter (fun d -> print_endline (Diag.format d)) (Diag.to_list diag)

(* one case per kind of boundary *)
let%expect_test "boundaries" =
  rewrite
    "a: map(xs, _ * 2)\n\
     b: print(_)\n\
     c: map(xs, f(_, 10))\n\
     d: map(xss, map(_, _ + 1))\n\
     inc: _ + 1\n\
     e: map(xs, _)";
  [%expect {|
    (bind a (call map xs (fn_ 1 (_0 * 2))))
    (bind b (fn_ 1 (call print _0)))
    (bind c (call map xs (fn_ 1 (call f _0 10))))
    (bind d (call map xss (fn_ 1 (call map _0 (fn_ 1 (_0 + 1))))))
    (bind inc (fn_ 1 (_0 + 1)))
    (bind e (fn_ 1 (call map xs _0)))
    |}]

let%expect_test "numbering: each `_` is the next parameter; `_k` can repeat or reorder" =
  rewrite "a: fold(xs, 0, _ + _)\nb: sort(xs, _1 < _0)\nc: map(xs, _0 * _0)";
  [%expect {|
    (bind a (call fold xs 0 (fn_ 2 (_0 + _1))))
    (bind b (call sort xs (fn_ 2 (_1 < _0))))
    (bind c (call map xs (fn_ 1 (_0 * _0))))
    |}]

let%expect_test "parentheses group but aren't boundaries" =
  rewrite "a: map(xs, (_ + 1) * 2)";
  [%expect {| (bind a (call map xs (fn_ 1 ((_0 + 1) * 2)))) |}]

let%expect_test "array elements and object fields are boundaries" =
  rewrite "a: [_ + 1, 2]\nb: <f: _ * 2>";
  [%expect {|
    (bind a (array (fn_ 1 (_0 + 1)) 2))
    (bind b (obj f: (fn_ 1 (_0 * 2))))
    |}]

let%expect_test "never crosses a block or a function" =
  rewrite "f: otala(x) { _ + x }\ng: { 1; _ * 2 }";
  [%expect {|
    (bind f (fn (x) (block (fn_ 1 (_0 + x)))))
    (bind g (block 1 (fn_ 1 (_0 * 2))))
    |}]

let%expect_test "`_` in a pattern is the wildcard" =
  rewrite "katsa (v) { _ => 0 }";
  [%expect {| (katsa v _ => 0) |}]

let%expect_test "errors: skipped index, mixed styles, `_` as a name" =
  rewrite "a: _0 + _2\nb: _ + _1\n_: 5";
  [%expect {|
    (bind a (fn_ 2 (_0 + _2)))
    (bind b (fn_ 2 (_0 + _1)))
    <error>
    t.t:3:1
    `_` isn't a name
    t.t:1:4
    `_1` is never used
    t.t:2:8
    use either `_` or `_0`, `_1`, ... in one function
    |}]

let%expect_test "warning: a statement that is only a `_` function" =
  rewrite "_ + 1\nprint(_)\nf: { _ * 2 }";
  [%expect {|
    (fn_ 1 (_0 + 1))
    (fn_ 1 (call print _0))
    (bind f (block (fn_ 1 (_0 * 2))))
    t.t:1:1
    warning: unused function value
    t.t:2:1
    warning: unused function value
    |}]

let%expect_test "parent links" =
  let src = Source.of_string ~name:"t.t" "x: f(a, b + 1)" in
  let diag = Diag.create () in
  let toks = Lexer.lex src diag in
  let ast = Parser.parse toks diag in
  let parent = Parent.compute ast in
  (* node 0 is the program *)
  Array.iteri
    (fun i p -> if i > 0 then Printf.printf "%d %s <- %d\n" i (Print_src.stmt { Print_src.t = ast; toks } 0 i) p)
    parent;
  [%expect {|
    1 x: f(a, b + 1) <- 0
    2 f(a, b + 1) <- 1
    3 f <- 2
    4 a <- 2
    5 b + 1 <- 2
    6 b <- 5
    7 1 <- 5
    |}]

let%expect_test "`_` in a katsa arm body is the matched value" =
  rewrite
    "a: katsa (n) { 0 => \"zero\", _ => cast<ahsli>(_) }\n\
     b: katsa (n) { _ => print(_) }\n\
     c: katsa (m) { some(v) => katsa (v) { _ => _ + 1 }, nil => 0 }\n\
     d: katsa (n) { _ => { log(_); _ } }";
  [%expect {|
    (bind a (katsa n 0 => "zero" _ => (cast ahsli _matched)))
    (bind b (katsa n _ => (call print _matched)))
    (bind c (katsa m some(v) => (katsa v _ => (_matched + 1)) nil => 0))
    (bind d (katsa n _ => (block (call log _matched) _matched)))
    |}]

let%expect_test "a nested `otala` or the scrutinee still makes a `_` function" =
  rewrite "a: katsa (n) { _ => otala(x) { x + _ } }\nb: katsa (f(_)) { _ => 0 }\nc: katsa (n) { _ => _0 }";
  [%expect {|
    (bind a (katsa n _ => (fn (x) (block (fn_ 1 (x + _0))))))
    (bind b (katsa (fn_ 1 (call f _0)) _ => 0))
    (bind c (katsa n _ => _matched))
    t.t:3:21
    use `_` for the matched value
    |}]
