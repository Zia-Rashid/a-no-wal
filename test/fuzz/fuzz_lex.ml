(* Lexer fuzzing (testing plan, part 3.11).

   Any input may produce errors, but lexing must never raise an OCaml exception, and the
   token table must stay well-formed. Two generators: raw bytes, and fragments of real
   syntax glued together at random (which reaches far deeper into the lexer). *)

open Anwl

let fragments =
  [| "a"; "x1"; "_"; "_0"; "_a"; "otala"; "katsa"; "int"; "0"; "7"; "0x"; "0xff"; "0b";
     "0b102"; "1_0"; "_"; "1."; ".5"; "1e"; "1e+"; "2.5e-3"; ".."; "..="; "."; "\""; "\\";
     "\\u{"; "\\u{41}"; "\\u{110000}"; "}"; "\\n"; "\\q"; "%"; "%%"; "\n"; "\r\n"; " ";
     "\t"; "<"; ">"; ">>"; "<<"; "?"; ":"; "::"; "="; "=="; "=>"; "->"; "!"; "!="; "&&";
     "|"; "("; ")"; "["; "]"; "{"; "}"; ","; ";"; "`"; "$"; "á"; "ʌ́"; "\xff"; "\xc3";
     "\xe2\x82"; "\x00" |]

let gen_bytes = QCheck2.Gen.(string_size ~gen:char (0 -- 200))

let gen_syntax =
  QCheck2.Gen.(
    map (String.concat "") (list_size (0 -- 60) (map (Array.get fragments) (0 -- (Array.length fragments - 1)))))

(* Lex [s] and check every invariant; returns false (or raises) on a violation. *)
let well_formed s =
  let src = Source.of_string ~name:"fuzz.t" s in
  let diag = Diag.create () in
  let toks = Lexer.lex src diag in
  let n = Lexer.count toks in
  let len = String.length s in
  let ok = ref true in
  let fail msg = Printf.eprintf "%s\n" msg; ok := false in
  if n = 0 || Lexer.tag toks (n - 1) <> Token.Eof || Lexer.start toks (n - 1) <> len then
    fail "last token must be Eof at the end of the input";
  for i = 0 to n - 2 do
    let a = Lexer.start toks i and b = Lexer.stop toks i in
    if not (a < b) then fail (Printf.sprintf "token %d is empty" i);
    if b > Lexer.start toks (i + 1) then fail (Printf.sprintf "token %d overlaps the next" i);
    if Lexer.tag toks i = Token.Eof then fail "Eof before the end";
    (* value helpers must cope with whatever text the lexer accepted *)
    (match Lexer.tag toks i with
     | Token.String -> ignore (Lexer.string_value toks i)
     | Token.Int -> ignore (Lexer.int_digits toks i)
     | Token.Newline ->
       if i = 0 then fail "newline at the start";
       if i > 0 && Token.continues_line (Lexer.tag toks (i - 1)) then
         fail "newline kept after a continuing token"
     | _ -> ignore (Lexer.text toks i))
  done;
  ignore (Lexer.dump toks);
  List.iter (fun d -> ignore (Diag.format d)) (Diag.to_list diag);
  !ok

let tests =
  [ QCheck2.Test.make ~name:"random bytes lex cleanly" ~count:2000 ~print:String.escaped
      gen_bytes well_formed;
    QCheck2.Test.make ~name:"random syntax fragments lex cleanly" ~count:5000
      ~print:String.escaped gen_syntax well_formed ]

let () = exit (QCheck_base_runner.run_tests ~verbose:false tests)
