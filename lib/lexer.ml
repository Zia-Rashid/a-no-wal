(* lexer turns the source text into an SoA of tokens 

   Two columns: [tag] (one byte per token) and [start] (byte offset) 
   Lexing wont stop on errors so they are'll reported at the end, it'll
   emit an [Invalid] token and carry on
*)

type tokens = {
  src : Source.t;
  tag : Bytes.t;
  start : int array;
  count : int;  (* including eof *)
}

let count t = t.count
let tag t i = Token.of_int (Char.code (Bytes.get t.tag i))
let start t i = t.start.(i)

(* ---------- character classes ---------- *)

let is_digit c = c >= '0' && c <= '9'
let is_bin c = c = '0' || c = '1'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_ident_char c = is_alpha c || is_digit c || c = '_'

(* Length of the UTF-8 character at [s.[i]]: 1 for ASCII and for invalid bytes. *)
let utf8_len s i =
  let d = String.get_utf_8_uchar s i in
  if Uchar.utf_decode_is_valid d then Uchar.utf_decode_length d else 1

let utf8_valid_at s i = Uchar.utf_decode_is_valid (String.get_utf_8_uchar s i)

(* ---------- scanning ----------

   [scan s pos ~err] reads one token starting at [pos] (never a blank or a comment) and
   returns its tag and end. [err start stop msg] reports a problem.

   The token store keeps only each token's start. [stop] finds the end by scanning the
   token again with an [err] that ignores problems; the result is the same as the first
   time because scanning has no state. *)

type err = int -> int -> string -> unit

(* Digits accepted by [ok], with single `_` separators between digits. *)
let scan_digits s pos ok ~(err : err) =
  let len = String.length s in
  let rec go i =
    if i < len && ok s.[i] then go (i + 1)
    else if i < len && s.[i] = '_' then begin
      let j = ref i in
      while !j < len && s.[!j] = '_' do incr j done;
      if !j - i > 1 || !j >= len || not (ok s.[!j]) then
        err i !j "`_` in a number must sit between two digits";
      go !j
    end
    else i
  in
  go pos

(* Letters, digits or `_` glued onto the end of a number: `10px`, `0b102`. *)
let scan_trailing s e ~(err : err) ~bad_digit =
  let len = String.length s in
  if e < len && is_ident_char s.[e] then begin
    let j = ref e in
    while !j < len && is_ident_char s.[!j] do incr j done;
    let text = String.sub s e (!j - e) in
    (match bad_digit with
     | Some base when is_hex s.[e] ->
       err e (e + 1) (Printf.sprintf "invalid digit `%c` in a %s literal" s.[e] base)
     | _ -> err e !j (Printf.sprintf "unexpected `%s` right after a number" text));
    !j
  end
  else e

let scan_exponent s pos ~(err : err) =
  (* s.[pos] is 'e' or 'E' *)
  let len = String.length s in
  let p = pos + 1 in
  let p = if p < len && (s.[p] = '+' || s.[p] = '-') then p + 1 else p in
  if p < len && is_digit s.[p] then scan_digits s p is_digit ~err
  else begin
    err pos p "an exponent needs digits";
    p
  end

let scan_number s pos ~(err : err) : Token.t * int =
  let len = String.length s in
  let prefixed c = s.[pos] = '0' && pos + 1 < len && s.[pos + 1] = c in
  if prefixed 'x' || prefixed 'b' then begin
    let hex = s.[pos + 1] = 'x' in
    let ok = if hex then is_hex else is_bin in
    let base = if hex then "hex" else "binary" in
    let p = pos + 2 in
    let e =
      if p < len && ok s.[p] then scan_digits s p ok ~err
      else if p + 1 < len && s.[p] = '_' && ok s.[p + 1] then begin
        (* `0x_ff`: one error for the misplaced `_`, then read the digits *)
        err p (p + 1) "`_` in a number must sit between two digits";
        scan_digits s (p + 1) ok ~err
      end
      else begin
        err pos p (Printf.sprintf "`%s` needs at least one %s digit" (String.sub s pos 2) base);
        p
      end
    in
    (* `0b102`: the `2` is reported as a bad digit; hex can't have one *)
    let bad_digit = if hex then None else Some base in
    (Token.Int, scan_trailing s e ~err ~bad_digit)
  end
  else if prefixed 'X' || prefixed 'B' then begin
    err pos (pos + 2)
      (Printf.sprintf "number prefixes are lowercase: write `0%c`"
         (Char.lowercase_ascii s.[pos + 1]));
    let e = pos + 2 in
    let e = if e < len && is_hex s.[e] then scan_digits s e is_hex ~err:(fun _ _ _ -> ()) else e in
    (Token.Int, scan_trailing s e ~err:(fun _ _ _ -> ()) ~bad_digit:None)
  end
  else begin
    let e = scan_digits s pos is_digit ~err in
    let tag, e =
      (* `1.5` is a float; `1..5` and `1.sz` are not *)
      if e + 1 < len && s.[e] = '.' && is_digit s.[e + 1] then begin
        let e = scan_digits s (e + 1) is_digit ~err in
        if e < len && (s.[e] = 'e' || s.[e] = 'E') then (Token.Float, scan_exponent s e ~err)
        else (Token.Float, e)
      end
      else if e < len && (s.[e] = 'e' || s.[e] = 'E') then (Token.Float, scan_exponent s e ~err)
      else (Token.Int, e)
    in
    (tag, scan_trailing s e ~err ~bad_digit:None)
  end

(* An escape starting at the backslash; returns the position after it. *)
let scan_escape s i ~(err : err) =
  let len =  String.length s in
  if i + 1 >= len then i + 1
  else
    match s.[i + 1] with
    | 'n' | 't' | 'r' | '0' | '\\' | '"' -> i + 2
    | 'u' when i + 2 < len && s.[i + 2] = '{' ->
      let j = ref (i + 3) in
      while !j < len && is_hex s.[!j] do incr j done;
      let digits = !j - (i + 3) in
      if !j >= len || s.[!j] <> '}' then begin
        err i !j "`\\u{…}` needs hex digits and a closing `}`";
        !j
      end
      else begin
        let close = !j + 1 in
        if digits = 0 || digits > 6 then err i close "`\\u{…}` takes 1 to 6 hex digits"
        else begin
          let v = int_of_string ("0x" ^ String.sub s (i + 3) digits) in
          if v > 0x10FFFF then err i close "`\\u{…}` is above U+10FFFF"
          else if v >= 0xD800 && v <= 0xDFFF then
            err i close "`\\u{…}` is a surrogate, not a character"
        end;
        close
      end
    | _ ->
      let n = utf8_len s (i + 1) in
      let stop = i + 1 + n in
      err i stop
        (Printf.sprintf "unknown escape `\\%s`" (String.sub s (i + 1) n));
      stop

(* A string literal. Raw newlines aren't allowed inside (use `\n`), so an unclosed
   string is reported at the end of its line rather than swallowing the file. *)
let scan_string s pos ~(err : err) =
  let len = String.length s in
  let rec go i =
    if i >= len || s.[i] = '\n' then begin
      err pos i "unclosed string (strings end on their line)";
      i
    end
    else
      match s.[i] with
      | '"' -> i + 1
      | '\\' -> go (scan_escape s i ~err)
      | _ -> go (i + 1)
  in
  go (pos + 1)

(* Blanks: spaces, tabs, carriage returns and `%%` comments. Newlines are tokens. *)
let skip_blanks s pos =
  let len = String.length s in
  let rec go i =
    if i >= len then i
    else
      match s.[i] with
      | ' ' | '\t' | '\r' -> go (i + 1)
      | '%' when i + 1 < len && s.[i + 1] = '%' ->
        let j = ref i in
        while !j < len && s.[!j] <> '\n' do incr j done;
        go !j
      | _ -> i
  in
  go pos

(* A newline token swallows any blank lines and comment lines after it. *)
let scan_newline s pos =
  let len = String.length s in
  let rec go i =
    let i = skip_blanks s i in
    if i < len && s.[i] = '\n' then go (i + 1) else i
  in
  go (pos + 1)

let scan_word s pos ~(err : err) : Token.t * int =
  let len = String.length s in
  let j = ref pos in
  while !j < len && is_ident_char s.[!j] do incr j done;
  let e = !j in
  if s.[pos] = '_' then begin
    (* `_` or `_` followed by digits only is a placeholder *)
    let all_digits = ref true in
    for k = pos + 1 to e - 1 do if not (is_digit s.[k]) then all_digits := false done;
    if !all_digits then (Token.Placeholder, e)
    else begin
      err pos e
        "names can't start with `_`";
      (Token.Ident, e)
    end
  end
  else
    match Token.keyword (String.sub s pos (e - pos)) with
    | Some kw -> (kw, e)
    | None -> (Token.Ident, e)

let scan s pos ~(err : err) : Token.t * int =
  let len = String.length s in
  let next = if pos + 1 < len then s.[pos + 1] else '\000' in
  let next2 = if pos + 2 < len then s.[pos + 2] else '\000' in
  let one (t : Token.t) = (t, pos + 1) and two (t : Token.t) = (t, pos + 2) in
  match s.[pos] with
  | '\n' -> (Token.Newline, scan_newline s pos)
  | c when is_digit c -> scan_number s pos ~err
  | c when is_alpha c || c = '_' -> scan_word s pos ~err
  | '"' -> (Token.String, scan_string s pos ~err)
  | '(' -> one LParen | ')' -> one RParen
  | '{' -> one LBrace | '}' -> one RBrace
  | '[' -> one LBracket | ']' -> one RBracket
  | ',' -> one Comma | ';' -> one Semi
  | ':' -> if next = ':' then two ColonColon else one Colon
  | '.' ->
    if next = '.' then (if next2 = '=' then (Token.DotDotEq, pos + 3) else two DotDot)
    else one Dot
  | '?' -> one Question
  | '`' -> one Backtick | '$' -> one Dollar
  | '=' -> if next = '=' then two EqEq else if next = '>' then two FatArrow else one Eq
  | '!' -> if next = '=' then two BangEq else one Bang
  | '<' -> if next = '=' then two LtEq else if next = '<' then one Lt_joined else one Lt
  | '>' -> if next = '=' then two GtEq else if next = '>' then one Gt_joined else one Gt
  | '+' -> one Plus
  | '-' -> if next = '>' then two Arrow else one Minus
  | '*' -> one Star | '/' -> one Slash | '%' -> one Percent
  | '&' -> if next = '&' then two AmpAmp else one Amp
  | '|' -> if next = '|' then two PipePipe else one Pipe
  | '^' -> one Caret | '~' -> one Tilde
  | c when Char.code c >= 0x80 && not (utf8_valid_at s pos) ->
    (* bad UTF-8: already reported once for the whole file by [check_utf8] *)
    (Token.Invalid, pos + 1)
  | c ->
    let stop = pos + utf8_len s pos in
    let shown = String.sub s pos (stop - pos) in
    if Char.code c >= 0x80 then
      err pos stop
        (Printf.sprintf "unexpected character `%s` (names are ASCII only)" shown)
    else err pos stop (Printf.sprintf "unexpected character `%s`" (String.escaped shown));
    (Token.Invalid, stop)

(* ---------- the token store ---------- *)

let no_err : err = fun _ _ _ -> ()

let stop t i =
  match tag t i with
  | Token.Eof -> String.length t.src.Source.text
  | _ -> snd (scan t.src.Source.text t.start.(i) ~err:no_err)

let text t i = String.sub t.src.Source.text t.start.(i) (stop t i - t.start.(i))

(* Report the first invalid UTF-8 byte, if any. *)
let check_utf8 (src : Source.t) diag =
  let s = src.text in
  let len = String.length s in
  let i = ref 0 in
  let bad = ref (-1) in
  while !bad < 0 && !i < len do
    let d = String.get_utf_8_uchar s !i in
    if Uchar.utf_decode_is_valid d then i := !i + Uchar.utf_decode_length d
    else bad := !i
  done;
  if !bad >= 0 then
    Diag.error diag src ~start:!bad ~stop:(!bad + 1)
      "invalid UTF-8"

let lex (src : Source.t) (diag : Diag.bag) : tokens =
  check_utf8 src diag;
  let s = src.text in
  let len = String.length s in
  let err start stop msg = Diag.error diag src ~start ~stop msg in
  let tags = Buffer.create (len / 3 + 16) in
  let starts = ref (Array.make (len / 3 + 16) 0) in
  let n = ref 0 in
  let push (t : Token.t) pos =
    if !n = Array.length !starts then begin
      let bigger = Array.make (2 * !n) 0 in
      Array.blit !starts 0 bigger 0 !n;
      starts := bigger
    end;
    Buffer.add_char tags (Char.chr (Token.to_int t));
    !starts.(!n) <- pos;
    incr n
  in
  let pos = ref (skip_blanks s 0) in
  while !pos < len do
    let t, e = scan s !pos ~err in
    (* A newline is dropped at the very start of the file, and after a token that can't
       end a line. *)
    let drop =
      t = Token.Newline
      && (!n = 0 || Token.continues_line (Token.of_int (Char.code (Buffer.nth tags (!n - 1)))))
    in
    if not drop then push t !pos;
    pos := skip_blanks s e
  done;
  push Token.Eof len;
  { src; tag = Buffer.to_bytes tags; start = Array.sub !starts 0 !n; count = !n }

(* ---------- literal values ---------- *)

(* The decoded value of a String token: escapes applied, then NFC-normalised so that
   `==` can compare bytes. *)
let string_value t i =
  let s = t.src.Source.text in
  let a = t.start.(i) + 1 in
  let b = stop t i - 1 in  (* before the closing quote *)
  let b = if b >= a && s.[b] = '"' then b else stop t i in
  let buf = Buffer.create (b - a) in
  let k = ref a in
  while !k < b do
    (match s.[!k] with
     | '\\' when !k + 1 < b ->
       (match s.[!k + 1] with
        | 'n' -> Buffer.add_char buf '\n'; k := !k + 2
        | 't' -> Buffer.add_char buf '\t'; k := !k + 2
        | 'r' -> Buffer.add_char buf '\r'; k := !k + 2
        | '0' -> Buffer.add_char buf '\000'; k := !k + 2
        | '\\' -> Buffer.add_char buf '\\'; k := !k + 2
        | '"' -> Buffer.add_char buf '"'; k := !k + 2
        | 'u' when !k + 2 < b && s.[!k + 2] = '{' ->
          (* read it the way [scan_escape] does; a bad escape (already reported)
             becomes U+FFFD *)
          let j = ref (!k + 3) in
          while !j < b && is_hex s.[!j] do incr j done;
          let digits = !j - (!k + 3) in
          let closed = !j < b && s.[!j] = '}' in
          (match
             if closed && digits >= 1 && digits <= 6 then
               int_of_string_opt ("0x" ^ String.sub s (!k + 3) digits)
             else None
           with
           | Some v when Uchar.is_valid v -> Buffer.add_utf_8_uchar buf (Uchar.of_int v)
           | _ -> Buffer.add_utf_8_uchar buf Uchar.rep);
          k := if closed then !j + 1 else !j
        | _ -> Buffer.add_char buf s.[!k + 1]; k := !k + 2)
     | c -> Buffer.add_char buf c; incr k)
  done;
  Uunf_string.normalize_utf_8 `NFC (Buffer.contents buf)

(* The digits of an Int token without `_`, with their base: `0xff_ff` -> (16, "ffff"). *)
let int_digits t i =
  let text = text t i in
  let strip s = String.concat "" (String.split_on_char '_' s) in
  if String.length text > 2 && text.[0] = '0' && text.[1] = 'x' then
    (16, strip (String.sub text 2 (String.length text - 2)))
  else if String.length text > 2 && text.[0] = '0' && text.[1] = 'b' then
    (2, strip (String.sub text 2 (String.length text - 2)))
  else (10, strip text)

(* ---------- dump (`--dump=tokens`) ---------- *)

let dump t =
  let b = Buffer.create 1024 in
  Buffer.add_string b " idx │ tag          │ start │ text\n";
  Buffer.add_string b "─────┼──────────────┼───────┼─────────\n";
  for i = 0 to t.count - 1 do
    let shown =
      match tag t i with
      | Token.Newline -> "\\n"
      | Token.Eof -> ""
      | _ -> text t i
    in
    Buffer.add_string b
      (Printf.sprintf "%4d │ %-12s │ %5d │ %s\n" i (Token.name (tag t i)) t.start.(i) shown)
  done;
  Buffer.contents b
