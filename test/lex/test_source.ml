(* Source positions and diagnostics. *)

open Anwl

let pos s offset =
  let line, col = Source.line_col (Source.of_string ~name:"t.t" s) offset in
  Printf.printf "%d -> %d:%d\n" offset line col

let%expect_test "line and column are 1-based" =
  pos "ab\ncd\n" 0;
  pos "ab\ncd\n" 1;
  pos "ab\ncd\n" 3;
  pos "ab\ncd\n" 4;
  [%expect {|
    0 -> 1:1
    1 -> 1:2
    3 -> 2:1
    4 -> 2:2
    |}]

let%expect_test "the newline itself belongs to its line" =
  pos "ab\ncd" 2;
  [%expect {| 2 -> 1:3 |}]

let%expect_test "end of file, with and without a final newline" =
  pos "ab" 2;
  pos "ab\n" 3;
  pos "" 0;
  [%expect {|
    2 -> 1:3
    3 -> 2:1
    0 -> 1:1
    |}]

let%expect_test "columns count code points, not bytes" =
  (* `ʌ́` is two code points (ʌ + U+0301), four bytes *)
  let s = "\"ʌ́\" x" in
  pos s (String.index s 'x');
  [%expect {| 7 -> 1:6 |}]

let%expect_test "CRLF: `\\r` counts as a column, lines still split on `\\n`" =
  pos "a\r\nb" 3;
  [%expect {| 3 -> 2:1 |}]

let%expect_test "diagnostic format, errors and warnings, in report order" =
  let src = Source.of_string ~name:"dir/prog.t" "x: 1\ny: x +\n" in
  let bag = Diag.create () in
  Diag.warning bag src ~start:0 ~stop:1 "`x` is never used";
  Diag.error bag src ~start:8 ~stop:9 "x was lihwa, we expected int.";
  List.iter (fun d -> print_endline (Diag.format d)) (Diag.to_list bag);
  Printf.printf "errors: %d, has_errors: %b\n" (Diag.error_count bag) (Diag.has_errors bag);
  [%expect {|
    dir/prog.t:1:1
    warning: `x` is never used
    dir/prog.t:2:4
    x was lihwa, we expected int.
    errors: 1, has_errors: true
    |}]

let%expect_test "warnings alone don't count as errors" =
  let src = Source.of_string ~name:"t.t" "x" in
  let bag = Diag.create () in
  Diag.warning bag src ~start:0 ~stop:1 "w";
  Printf.printf "has_errors: %b\n" (Diag.has_errors bag);
  [%expect {| has_errors: false |}]

let%expect_test "interning gives one id per distinct string" =
  let pool = Intern.create () in
  let a = Intern.intern pool "kes" and b = Intern.intern pool "kun" in
  let a' = Intern.intern pool "kes" in
  Printf.printf "%b %b %s\n" (a = a') (a = b) (Intern.get pool b);
  [%expect {| true false kun |}]
