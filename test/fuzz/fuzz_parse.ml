(* Parser properties: printing then parsing gives the same tree, and random tokens never
   crash the parser (testing plan, parts 3.2 and 3.11).

   Round trip: random programs are generated as source with every sub-expression in
   parentheses, so they always parse. Printing the tree with the source printer (minimal
   parentheses) and parsing that again must give the same tree, compared through the
   tree printer. *)

open Anwl

let parse s =
  let diag = Diag.create () in
  let toks = Lexer.lex (Source.of_string ~name:"fuzz.t" s) diag in
  let ast = Parser.parse toks diag in
  (ast, toks, diag)

(* ---------- random programs ---------- *)

open QCheck2.Gen

let atom =
  oneof_list
    [ "a"; "b"; "xs"; "f"; "0"; "7"; "0x1f"; "1.5"; "2e3"; "-5"; "-2.5"; "\"s\""; "true";
      "false"; "nil" ]

let binop =
  oneof_list [ "+"; "-"; "*"; "/"; "%"; "&"; "|"; "^"; "<<"; ">>"; "=="; "!="; "<"; ">"; "<=";
           ">="; "&&"; "||" ]

let typ =
  oneof_list [ "int"; "int?"; "[kl]"; "tree<int>"; "tree<tree<int>>"; "(int) -> int?";
           "((int) -> int)?"; "<x: int, y: [ahsli]>"; "(int, kl) -> void" ]

let rec expr n =
  if n <= 0 then atom
  else
    let e = expr (n / 2) in
    let p fmt = map (fun s -> "(" ^ s ^ ")") fmt in
    oneof_weighted
      [ (2, atom);
        (4, p (map3 (fun a o b -> a ^ " " ^ o ^ " " ^ b) e binop e));
        (1, p (map3 (fun a b c -> a ^ " < " ^ b ^ " <= " ^ c) e e e));
        (1, p (map3 (fun a b c -> a ^ " >= " ^ b ^ " == " ^ c) e e e));
        (1, p (map2 (fun o a -> o ^ a) (oneof_list [ "-"; "!"; "~" ]) e));
        (1, p (map3 (fun a b s -> a ^ " .. " ^ b ^ s) e e (oneof [ pure ""; map (( ^ ) " by ") e ])));
        (1, p (map3 (fun a b c -> a ^ " ? " ^ b ^ " : " ^ c) e e e));
        (1, map (fun a -> a ^ ".x") e);
        (1, map2 (fun a b -> a ^ "(" ^ b ^ ", 1)") e e);
        (1, map2 (fun a b -> a ^ "[" ^ b ^ "]") e e);
        (1, map2 (fun a b -> "[" ^ a ^ ", " ^ b ^ "]") e e);
        (1, map2 (fun a b -> "<x: (" ^ a ^ "), y: " ^ b ^ ">") e e);
        (1, map (fun a -> "otala(a, int b) { " ^ a ^ " }") e);
        (1, map2 (fun a b -> "{ x: " ^ a ^ "; " ^ b ^ " }") e e);
        (1, map3 (fun a b c -> "if (" ^ a ^ ") { " ^ b ^ " } else { " ^ c ^ " }") e e e);
        (1, map3 (fun a b c -> "katsa (" ^ a ^ ") { some(x) => " ^ b ^ ", _ => " ^ c ^ " }") e e e);
        (1, p (map2 (fun k a -> k ^ " " ^ a) (oneof_list [ "break"; "ahket" ]) e));
        (1, map2 (fun t a -> "cast<" ^ t ^ ">(" ^ a ^ ")") typ e);
        (1, map (fun a -> "`{ $x + " ^ a ^ " }") e);
        (1, map (fun a -> "for (i of xs) with (acc: 0) { " ^ a ^ " }") e) ]

let statement =
  sized_size (int_bound 12) (fun n ->
      let e = expr n in
      oneof_weighted
        [ (3, map (fun a -> "x: " ^ a) e);
          (2, map2 (fun t a -> t ^ " y: " ^ a) typ e);
          (1, map (fun a -> "x = " ^ a) e);
          (2, e) ])

let program = map (String.concat "\n") (list_size (1 -- 4) statement)

let round_trip s =
  let ast, toks, diag = parse s in
  if Diag.has_errors diag then (prerr_endline "generated program doesn't parse"; false)
  else begin
    let printed = Print_src.program ast toks in
    let ast2, toks2, diag2 = parse printed in
    let same = Print_tree.program ast toks = Print_tree.program ast2 toks2 in
    if Diag.has_errors diag2 || not same then prerr_endline ("printed as:\n" ^ printed);
    (not (Diag.has_errors diag2)) && same
  end

(* ---------- token soup ---------- *)

let fragments =
  [| "a"; "x"; "_"; "_1"; "1"; "-"; "2.5"; "\"s\""; "otala"; "katsa"; "if"; "then"; "else";
     "for"; "of"; "ever"; "with"; "by"; "break"; "ahket"; "type"; "export"; "use"; "as"; "cast";
     "atalo"; "int"; "nil"; "("; ")"; "["; "]"; "{"; "}"; "<"; ">"; ">>"; "<<"; ","; ";"; ":";
     "::"; "="; "=="; "=>"; "->"; "?"; "."; ".."; "..="; "+"; "*"; "&&"; "!"; "`"; "$"; "\n";
     " " |]

let soup = map (String.concat " ") (list_size (0 -- 40) (oneof_array fragments))

let no_crash s =
  let ast, toks, diag = parse s in
  let ast = Placeholder.rewrite ast toks diag in
  ignore (Print_tree.program ~idx:true ast toks);
  ignore (Print_src.program ast toks);
  ignore (Parent.compute ast);
  true

let tests =
  [ QCheck2.Test.make ~name:"print then parse gives the same tree" ~count:3000 ~print:Fun.id
      program round_trip;
    QCheck2.Test.make ~name:"random tokens never crash the parser" ~count:5000 ~print:String.escaped
      soup no_crash ]

let () = exit (QCheck_base_runner.run_tests ~verbose:false tests)
