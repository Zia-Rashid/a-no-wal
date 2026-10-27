(* Lexer tests (testing plan, part 3.1). *)

open Anwl

(* Prints each token as `tag:text` on one line (newlines as `⏎`, Eof omitted), then any
   diagnostics. *)
let show s =
  let src = Source.of_string ~name:"t.t" s in
  let diag = Diag.create () in
  let toks = Lexer.lex src diag in
  let parts = ref [] in
  for i = 0 to Lexer.count toks - 1 do
    match Lexer.tag toks i with
    | Token.Eof -> ()
    | Token.Newline -> parts := "⏎" :: !parts
    | t -> parts := (Token.name t ^ ":" ^ Lexer.text toks i) :: !parts
  done;
  print_endline (String.concat " " (List.rev !parts));
  List.iter (fun d -> print_endline (Diag.format d)) (Diag.to_list diag)

(* Value helpers for the first String / Int token in [s]. *)
let first tag s =
  let src = Source.of_string ~name:"t.t" s in
  let toks = Lexer.lex src (Diag.create ()) in
  let rec find i = if Lexer.tag toks i = tag then i else find (i + 1) in
  (toks, find 0)

let string_value s = let toks, i = first Token.String s in Lexer.string_value toks i
let int_digits s = let toks, i = first Token.Int s in Lexer.int_digits toks i

(* ---------- numbers ---------- *)

let%expect_test "integer forms" =
  show "255 0xff 0xFF 0b1111_1111 1_000_000 007";
  [%expect {| int:255 int:0xff int:0xFF int:0b1111_1111 int:1_000_000 int:007 |}]

let%expect_test "int digits strip separators and prefixes" =
  List.iter
    (fun s -> let b, d = int_digits s in Printf.printf "%s -> base %d %s\n" s b d)
    [ "0xff_ff"; "0b1010_0101"; "1_000"; "007" ];
  [%expect {|
    0xff_ff -> base 16 ffff
    0b1010_0101 -> base 2 10100101
    1_000 -> base 10 1000
    007 -> base 10 007
    |}]

let%expect_test "separator errors" =
  show "0x_ff";
  show "1__0";
  show "10_";
  [%expect {|
    int:0x_ff
    t.t:1:3
    `_` in a number must sit between two digits
    int:1__0
    t.t:1:2
    `_` in a number must sit between two digits
    int:10_
    t.t:1:3
    `_` in a number must sit between two digits
    |}]

let%expect_test "_10 is a placeholder, not a number" =
  show "_10 _ _0";
  [%expect {| placeholder:_10 placeholder:_ placeholder:_0 |}]

let%expect_test "floats" =
  show "1.5 0.25 1e9 1.5e-3 2E10 1e+3";
  [%expect {| float:1.5 float:0.25 float:1e9 float:1.5e-3 float:2E10 float:1e+3 |}]

let%expect_test "1. and .5 are not floats (the parser rejects them later)" =
  show "1. .5";
  [%expect {| int:1 .:. .:. int:5 |}]

let%expect_test "ranges and field access on numbers" =
  show "0..5 0..=5 1.sz";
  [%expect {| int:0 ..:.. int:5 int:0 ..=:..= int:5 int:1 .:. ident:sz |}]

let%expect_test "bad exponents, prefixes and digits" =
  show "1e";
  show "1e+";
  show "0x";
  show "0X1F";
  show "0b102";
  show "10px";
  [%expect {|
    float:1e
    t.t:1:2
    an exponent needs digits
    float:1e+
    t.t:1:2
    an exponent needs digits
    int:0x
    t.t:1:1
    `0x` needs at least one hex digit
    int:0X1F
    t.t:1:1
    number prefixes are lowercase: write `0x`
    int:0b102
    t.t:1:5
    invalid digit `2` in a binary literal
    int:10px
    t.t:1:3
    unexpected `px` right after a number
    |}]

(* ---------- strings ---------- *)

let%expect_test "escapes" =
  print_endline (String.escaped (string_value {|"a\nb\t\"q\"\\ \0"|}));
  print_endline (string_value {|"\u{41}\u{1F600}"|});
  [%expect {|
    a\nb\t\"q\"\\ \000
    A😀
    |}]

let%expect_test "NFC: composed and decomposed á give the same value" =
  let composed = string_value "\"\xc3\xa1\"" in       (* U+00E1 *)
  let decomposed = string_value "\"a\xcc\x81\"" in    (* a + U+0301 *)
  Printf.printf "equal: %b, bytes: %d\n" (composed = decomposed) (String.length decomposed);
  [%expect {| equal: true, bytes: 2 |}]

let%expect_test "Oneida text in strings" =
  show {|s: "kátsaˀ ka·yʌ́"|};
  [%expect {| ident:s ::: string:"kátsaˀ ka·yʌ́" |}]

let%expect_test "string errors" =
  show {|"abc|};
  show "\"line one\nline two\"";
  show {|"\q"|};
  show {|"\u{}" "\u{110000}" "\u{D800}" "\u{41"|};
  [%expect {|
    string:"abc
    t.t:1:1
    unclosed string (strings end on their line)
    string:"line one ⏎ ident:line ident:two string:"
    t.t:1:1
    unclosed string (strings end on their line)
    t.t:2:9
    unclosed string (strings end on their line)
    string:"\q"
    t.t:1:2
    unknown escape `\q`
    string:"\u{}" string:"\u{110000}" string:"\u{D800}" string:"\u{41"
    t.t:1:2
    `\u{…}` takes 1 to 6 hex digits
    t.t:1:9
    `\u{…}` is above U+10FFFF
    t.t:1:22
    `\u{…}` is a surrogate, not a character
    t.t:1:33
    `\u{…}` needs hex digits and a closing `}`
    |}]

let%expect_test "bad `\\u{…}` escapes decode to U+FFFD, never past the string's end" =
  (* found by the fuzzer: an unclosed `\u{` used to raise Not_found *)
  List.iter
    (fun s -> print_endline (String.escaped (string_value s)))
    [ {|"a\u{"|}; {|"\u{41"} "|}; {|"\u{zz}"|}; {|"\u{1234567}"|} ];
  [%expect {|
    a\239\191\189
    \239\191\189
    \239\191\189zz}
    \239\191\189
    |}]

(* ---------- comments, operators, `>` joining ---------- *)

let%expect_test "%% comments vs % modulo" =
  show "a % b %% the rest is a comment\nc";
  [%expect {| ident:a %:% ident:b ⏎ ident:c |}]

let%expect_test "joined > and <" =
  show "tree<tree<int>> a >> 2 a > > 2 1 << 3 a >= b a <= b";
  [%expect {| ident:tree <:< ident:tree <:< kw_int:int >(joined):> >:> ident:a >(joined):> >:> int:2 ident:a >:> >:> int:2 int:1 <(joined):< <:< int:3 ident:a >=:>= ident:b ident:a <=:<= ident:b |}]

let%expect_test "multi-character operators" =
  show ":: : .. ..= . -> => == = != ! && & || | ^ ~ ? ` $";
  [%expect {| ::::: ::: ..:.. ..=:..= .:. ->:-> =>:=> ==:== =:= !=:!= !:! &&:&& &:& ||:|| |:| ^:^ ~:~ ?:? `:` $:$ |}]

(* ---------- keywords and names ---------- *)

let%expect_test "every keyword" =
  show (String.concat " " (List.map fst Token.keywords));
  [%expect {| otala:otala ahket:ahket katsa:katsa type:type if:if then:then else:else for:for of:of ever:ever with:with by:by break:break nil:nil true:true false:false kw_int:int kw_kl:kl lihwa:lihwa ahsli:ahsli void:void range:range atalo:atalo cast:cast export:export use:use as:as |}]

let%expect_test "keyword prefixes are names" =
  show "otalax iff kes kun some sz";
  [%expect {| ident:otalax ident:iff ident:kes ident:kun ident:some ident:sz |}]

let%expect_test "names can't start with _" =
  show "_abc";
  [%expect {|
    ident:_abc
    t.t:1:1
    names can't start with `_`
    |}]

let%expect_test "non-ASCII outside strings" =
  show "kátsa: 1";
  [%expect {|
    ident:k invalid:á ident:tsa ::: int:1
    t.t:1:2
    unexpected character `á` (names are ASCII only)
    |}]

(* ---------- newlines ---------- *)

let%expect_test "blank lines and comment lines collapse into one newline" =
  show "a\n\n%% note\n   \nb\r\nc";
  [%expect {| ident:a ⏎ ident:b ⏎ ident:c |}]

let%expect_test "no newline token at the start of a file" =
  show "\n\n%% header\nx";
  [%expect {| ident:x |}]

let%expect_test "a line ending in an operator, `,`, `(` or `[` continues" =
  show "a +\nb";
  show "f(a,\n  b)";
  show "xs: [\n1]";
  show "x =\n%% comment in between\n\n  2";
  show "ok: a &&\nb ||\nc";
  [%expect {|
    ident:a +:+ ident:b
    ident:f (:( ident:a ,:, ident:b ):)
    ident:xs ::: [:[ int:1 ]:]
    ident:x =:= int:2
    ident:ok ::: ident:a &&:&& ident:b ||:|| ident:c
    |}]

let%expect_test "a newline before `(` or `[` ends the statement" =
  show "a\n(b)";
  show "a\n[0]";
  [%expect {|
    ident:a ⏎ (:( ident:b ):)
    ident:a ⏎ [:[ int:0 ]:]
    |}]

let%expect_test "`<`, `>` and `?` keep their newline (the parser decides)" =
  show "p: <x: 1>\nq";
  show "type a = int?\nb";
  show "c ?\nd";
  [%expect {|
    ident:p ::: <:< ident:x ::: int:1 >:> ⏎ ident:q
    type:type ident:a =:= kw_int:int ?:? ⏎ ident:b
    ident:c ?:? ⏎ ident:d
    |}]

(* ---------- encoding ---------- *)

let%expect_test "invalid UTF-8 is reported once, at the first bad byte" =
  show "a \xff b \xfe";
  [%expect {|
    ident:a invalid:� ident:b invalid:�
    t.t:1:3
    invalid UTF-8
    |}]

(* ---------- positions ---------- *)

let%expect_test "error positions count code points, not bytes" =
  show "s: \"ʌ́ʌ́\" é";
  [%expect {|
    ident:s ::: string:"ʌ́ʌ́" invalid:é
    t.t:1:11
    unexpected character `é` (names are ASCII only)
    |}]
