(* Token tags. The lexer stores one tag byte per token. *)

type t =
  (* literals and names *)
  | Ident
  | Int  (* decimal, 0x… or 0b…; value read from the text by the parser *)
  | Float
  | String
  | Placeholder  (* `_`, `_0`, `_1`, … *)
  (* keywords *)
  | Kw_otala | Kw_ahket | Kw_katsa | Kw_type
  | Kw_if | Kw_then | Kw_else
  | Kw_for | Kw_of | Kw_ever | Kw_with | Kw_by | Kw_break
  | Kw_nil | Kw_true | Kw_false
  | Kw_int | Kw_kl | Kw_lihwa | Kw_ahsli | Kw_void | Kw_range
  | Kw_atalo
  | Kw_cast | Kw_export | Kw_use | Kw_as
  (* brackets and separators *)
  | LParen | RParen | LBrace | RBrace | LBracket | RBracket
  | Comma | Semi | Newline
  | Colon | ColonColon
  | Dot | DotDot | DotDotEq
  | Question | Arrow | FatArrow
  | Backtick | Dollar
  (* operators *)
  | Eq | EqEq | Bang | BangEq
  | Lt | Lt_joined  (* `<` immediately followed by `<`: the parser decides `<<` vs two `<` *)
  | Gt | Gt_joined  (* `>` immediately followed by `>`: `>>`, or two closers in `tree<tree<int>>` *)
  | LtEq | GtEq
  | Plus | Minus | Star | Slash | Percent
  | Amp | AmpAmp | Pipe | PipePipe | Caret | Tilde
  (* special *)
  | Invalid  (* a character the lexer couldn't use; already reported *)
  | Eof

let all =
  [| Ident; Int; Float; String; Placeholder;
     Kw_otala; Kw_ahket; Kw_katsa; Kw_type;
     Kw_if; Kw_then; Kw_else;
     Kw_for; Kw_of; Kw_ever; Kw_with; Kw_by; Kw_break;
     Kw_nil; Kw_true; Kw_false;
     Kw_int; Kw_kl; Kw_lihwa; Kw_ahsli; Kw_void; Kw_range;
     Kw_atalo;
     Kw_cast; Kw_export; Kw_use; Kw_as;
     LParen; RParen; LBrace; RBrace; LBracket; RBracket;
     Comma; Semi; Newline;
     Colon; ColonColon;
     Dot; DotDot; DotDotEq;
     Question; Arrow; FatArrow;
     Backtick; Dollar;
     Eq; EqEq; Bang; BangEq;
     Lt; Lt_joined; Gt; Gt_joined;
     LtEq; GtEq;
     Plus; Minus; Star; Slash; Percent;
     Amp; AmpAmp; Pipe; PipePipe; Caret; Tilde;
     Invalid; Eof |]

(* Constant constructors are represented by their declaration index, which is exactly
   the byte we store. [all] lists them in declaration order; the check below keeps the
   two in sync. *)
let to_int (t : t) : int = Obj.magic t
let of_int i : t = all.(i)

let () =
  assert (Array.length all <= 256);
  Array.iteri (fun i t -> assert (to_int t = i)) all

let keywords =
  [ "otala", Kw_otala; "ahket", Kw_ahket; "katsa", Kw_katsa; "type", Kw_type;
    "if", Kw_if; "then", Kw_then; "else", Kw_else;
    "for", Kw_for; "of", Kw_of; "ever", Kw_ever; "with", Kw_with; "by", Kw_by;
    "break", Kw_break;
    "nil", Kw_nil; "true", Kw_true; "false", Kw_false;
    "int", Kw_int; "kl", Kw_kl; "lihwa", Kw_lihwa; "ahsli", Kw_ahsli;
    "void", Kw_void; "range", Kw_range;
    "atalo", Kw_atalo;
    "cast", Kw_cast; "export", Kw_export; "use", Kw_use; "as", Kw_as ]

let keyword_table =
  let h = Hashtbl.create 64 in
  List.iter (fun (s, t) -> Hashtbl.add h s t) keywords;
  h

let keyword s = Hashtbl.find_opt keyword_table s

(* A line ending in one of these tokens always continues on the next line, so the lexer
   drops the newline after it. `<`, `>` and `?` can go either way: `p: <x: 1>` and
   `type a = int?` end a statement, but `a >` and `c ?` don't. The parser decides those. *)
let continues_line = function
  | Plus | Minus | Star | Slash | Percent
  | Amp | AmpAmp | Pipe | PipePipe | Caret | Tilde | Bang
  | EqEq | BangEq | LtEq | GtEq
  | Eq | FatArrow | Arrow | Colon | ColonColon
  | Dot | DotDot | DotDotEq
  | Comma | LParen | LBracket | LBrace
  | Backtick | Dollar -> true
  | _ -> false

(* Short lowercase name, for dumps and tests. *)
let name = function
  | Ident -> "ident" | Int -> "int" | Float -> "float" | String -> "string"
  | Placeholder -> "placeholder"
  | Kw_otala -> "otala" | Kw_ahket -> "ahket" | Kw_katsa -> "katsa" | Kw_type -> "type"
  | Kw_if -> "if" | Kw_then -> "then" | Kw_else -> "else"
  | Kw_for -> "for" | Kw_of -> "of" | Kw_ever -> "ever" | Kw_with -> "with"
  | Kw_by -> "by" | Kw_break -> "break"
  | Kw_nil -> "nil" | Kw_true -> "true" | Kw_false -> "false"
  | Kw_int -> "kw_int" | Kw_kl -> "kw_kl" | Kw_lihwa -> "lihwa" | Kw_ahsli -> "ahsli"
  | Kw_void -> "void" | Kw_range -> "range"
  | Kw_atalo -> "atalo"
  | Kw_cast -> "cast" | Kw_export -> "export" | Kw_use -> "use" | Kw_as -> "as"
  | LParen -> "(" | RParen -> ")" | LBrace -> "{" | RBrace -> "}"
  | LBracket -> "[" | RBracket -> "]"
  | Comma -> "," | Semi -> ";" | Newline -> "newline"
  | Colon -> ":" | ColonColon -> "::"
  | Dot -> "." | DotDot -> ".." | DotDotEq -> "..="
  | Question -> "?" | Arrow -> "->" | FatArrow -> "=>"
  | Backtick -> "`" | Dollar -> "$"
  | Eq -> "=" | EqEq -> "==" | Bang -> "!" | BangEq -> "!="
  | Lt -> "<" | Lt_joined -> "<(joined)" | Gt -> ">" | Gt_joined -> ">(joined)"
  | LtEq -> "<=" | GtEq -> ">="
  | Plus -> "+" | Minus -> "-" | Star -> "*" | Slash -> "/" | Percent -> "%"
  | Amp -> "&" | AmpAmp -> "&&" | Pipe -> "|" | PipePipe -> "||"
  | Caret -> "^" | Tilde -> "~"
  | Invalid -> "invalid" | Eof -> "eof"
