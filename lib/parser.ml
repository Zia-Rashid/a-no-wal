(* Parser: tokens make up surface AST

   Statements and the keyword forms are recursive descent; expressions use a pratt loop
   w/ the binding powers described in grammar doc part 3

   Errors: the first error in a statement is reported and raises [Fail]; the statement
   loop catches it, skips to the next `;`, newline or `}`, and carries on. So each
   statement reports at most one error.

   Newlines: the lexer already drops a newline after a token that can't end a line. The
   parser also skips newlines where a statement can't end: after an infix operator (that
   covers `<`, `>` and `?`, which the lexer leaves alone), inside brackets before `,` or a
   closer, and before a `{`, `then` or `else` that continues a form. *)

module T = Token
module A = Ast

exception Fail

type p = {
  toks : Lexer.tokens;
  diag : Diag.bag;
  b : A.builder;
  type_names : (string, unit) Hashtbl.t;  (* from the pre-pass *)
  mutable pos : int;
  mutable last : int;  (* last non-newline token consumed, for spans *)
  mutable stmt_start : int;  (* first token of the statement being parsed *)
  mutable speculating : bool;  (* errors fail silently while trying a declaration *)
  mutable quote_depth : int;
}

(* ---------- tokens ---------- *)

let peek p = Lexer.tag p.toks p.pos

let peek_at p k =
  let i = p.pos + k in
  if i < Lexer.count p.toks then Lexer.tag p.toks i else T.Eof

let text p i = Lexer.text p.toks i

let advance p =
  let i = p.pos in
  if peek p <> T.Eof then begin
    p.pos <- i + 1;
    if Lexer.tag p.toks i <> T.Newline then p.last <- i
  end;
  i

let error p tok msg =
  (* If newlines were skipped to reach [tok] (e.g. an unclosed `(`), the problem is at the
     end of the previous line: report there, and back up to the newline so recovery keeps
     the next line. Only within the current statement, so recovery always moves forward. *)
  let start, stop =
    if tok = p.pos && tok > 0 && Lexer.tag p.toks (tok - 1) = T.Newline && p.last >= p.stmt_start
    then begin
      p.pos <- p.last + 1;
      let e = Lexer.stop p.toks p.last in
      (e, e)
    end
    else (Lexer.start p.toks tok, Lexer.stop p.toks tok)
  in
  (* an Invalid token was already reported by the lexer *)
  if not (p.speculating || Lexer.tag p.toks tok = T.Invalid) then
    Diag.error p.diag p.toks.src ~start ~stop msg;
  raise Fail

let expect p tag what = if peek p = tag then advance p else error p p.pos ("expected " ^ what)

let skip_nl p = while peek p = T.Newline do ignore (advance p) done

(* The next token after any newlines, without consuming them. *)
let peek_past_nl p =
  let k = ref 0 in
  while peek_at p !k = T.Newline do incr k done;
  peek_at p !k

let is_close_angle = function T.Gt | T.Gt_joined -> true | _ -> false
let is_open_angle = function T.Lt | T.Lt_joined -> true | _ -> false

let close_angle p = if is_close_angle (peek p) then advance p else error p p.pos "expected `>`"

(* ---------- nodes ---------- *)

let leaf p tag tok = A.add p.b tag ~main:tok ~lhs:A.none ~rhs:A.none ~first:tok ~last:tok

(* [first] is the node's first token; its last is the last token consumed so far. *)
let node p tag ~main ~first ~lhs ~rhs = A.add p.b tag ~main ~lhs ~rhs ~first ~last:p.last
let list p tag ~main ~first kids = A.add_list p.b tag ~main ~first ~last:p.last kids
let first_of p n = A.b_first p.b n

(* `a, b, c` up to a closing token; newlines around items are skipped. [trailing] allows
   `a, b,` before the closer. *)
let comma_list ?(trailing = false) p ~is_close item =
  skip_nl p;
  let rec go acc =
    if is_close (peek p) && (acc = [] || trailing) then List.rev acc
    else begin
      let x = item p in
      skip_nl p;
      if peek p = T.Comma then (ignore (advance p); skip_nl p; go (x :: acc))
      else List.rev (x :: acc)
    end
  in
  let xs = go [] in
  if is_close (peek p) then (ignore (advance p); xs) else error p p.pos "expected `,` or a closing bracket"

let is_type_keyword = function
  | T.Kw_int | T.Kw_kl | T.Kw_lihwa | T.Kw_ahsli | T.Kw_void | T.Kw_range -> true
  | _ -> false

let can_start_type t =
  is_type_keyword t || is_open_angle t
  || (match t with T.Ident | T.LBracket | T.LParen -> true | _ -> false)

let can_start_expr = function
  | T.Int | T.Float | T.String | T.Ident | T.Placeholder | T.Kw_true | T.Kw_false | T.Kw_nil
  | T.LParen | T.LBracket | T.LBrace | T.Lt | T.Lt_joined | T.Minus | T.Bang | T.Tilde
  | T.Kw_otala | T.Kw_if | T.Kw_for | T.Kw_katsa | T.Kw_ahket | T.Kw_break | T.Backtick
  | T.Dollar | T.Kw_cast -> true
  | _ -> false

(* ---------- types ---------- *)

let rec typ p =
  let t = ref (type_atom p) in
  while peek p = T.Question do
    let q = advance p in
    t := node p A.T_opt ~main:q ~first:(first_of p !t) ~lhs:!t ~rhs:A.none
  done;
  !t

and type_atom p =
  let tok = p.pos in
  match peek p with
  | t when is_type_keyword t -> ignore (advance p); list p A.T_named ~main:tok ~first:tok []
  | T.Ident ->
    ignore (advance p);
    let args = if is_open_angle (peek p) then (ignore (advance p); comma_list p ~is_close:is_close_angle typ) else [] in
    list p A.T_named ~main:tok ~first:tok args
  | T.LBracket ->
    ignore (advance p);
    let t = typ p in
    ignore (expect p T.RBracket "`]`");
    node p A.T_array ~main:tok ~first:tok ~lhs:t ~rhs:A.none
  | T.LParen ->
    (* `(T, U) -> R`, or just `(T)` for grouping *)
    ignore (advance p);
    let params = comma_list p ~is_close:(( = ) T.RParen) typ in
    if peek p = T.Arrow then begin
      ignore (advance p);
      let ret = typ p in
      list p A.T_fn ~main:tok ~first:tok (params @ [ ret ])
    end
    else (match params with [ t ] -> t | _ -> error p p.pos "expected `->`")
  | t when is_open_angle t ->
    ignore (advance p);
    let field p =
      let name = expect p T.Ident "a field name" in
      ignore (expect p T.Colon "`:`");
      let t = typ p in
      node p A.T_field ~main:name ~first:name ~lhs:t ~rhs:A.none
    in
    list p A.T_obj ~main:tok ~first:tok (comma_list p ~is_close:is_close_angle field)
  | _ -> error p p.pos "expected a type"

let type_params p =
  let tok = advance p in  (* `<` *)
  let param p = leaf p A.T_param (expect p T.Ident "a type parameter") in
  list p A.Type_params ~main:tok ~first:tok (comma_list p ~is_close:is_close_angle param)

(* ---------- expressions ---------- *)

(* Infix binding powers, from the precedence table in the grammar doc (part 3). *)
let binary_bp = function
  | T.Lt_joined | T.Gt_joined -> Some (16, 15)  (* `<<` `>>` *)
  | T.Star | T.Slash | T.Percent | T.Amp | T.Pipe | T.Caret -> Some (13, 14)
  | T.Plus | T.Minus -> Some (11, 12)
  | T.AmpAmp -> Some (6, 5)
  | T.PipePipe -> Some (4, 3)
  | _ -> None

let is_cmp = function T.EqEq | T.BangEq | T.Lt | T.Gt | T.LtEq | T.GtEq -> true | _ -> false

(* Direction of a comparison in a chain: `<` up, `>` down, `==` either. *)
let direction = function T.Lt | T.LtEq -> 1 | T.Gt | T.GtEq -> -1 | _ -> 0

(* [no_gt]: inside an object literal, `>` closes the object instead of comparing. *)
let rec expr p ~min ~no_gt =
  let lhs = ref (prefix p ~no_gt) in
  let first = first_of p !lhs in
  (* the kind of operator that built [lhs] so far: a range can't be followed by another
     range, and a comparison can be extended into a chain *)
  let built = ref `Other in
  let chain_dir = ref 0 in
  let continue = ref true in
  while !continue do
    let t = peek p in
    let op = p.pos in
    if no_gt && is_close_angle t then continue := false
    else if t = T.Dot || t = T.LParen || t = T.LBracket then begin
      lhs := postfix p !lhs ~first;
      built := `Other
    end
    else
      match binary_bp t with
      | Some (l, r) when l >= min ->
        ignore (advance p);
        (* `<<` and `>>` are two touching tokens; the second must be a plain `<` / `>`
           (`a <<= b` is `<` then `<=`, not a shift) *)
        if t = T.Lt_joined || t = T.Gt_joined then begin
          let second = peek p in
          if not (if t = T.Lt_joined then is_open_angle second else is_close_angle second) then
            error p p.pos "expected an expression";
          ignore (advance p)
        end;
        skip_nl p;
        let rhs = expr p ~min:r ~no_gt in
        lhs := node p A.Binop ~main:op ~first ~lhs:!lhs ~rhs;
        built := `Other
      | Some _ -> continue := false
      | None ->
        if is_cmp t && 7 >= min then begin
          ignore (advance p);
          skip_nl p;
          let rhs = expr p ~min:8 ~no_gt in
          if !built = `Cmp then begin
            (* extend a chain: `!=` never chains, and `<` and `>` don't mix *)
            let prev = Lexer.tag p.toks (A.b_main p.b !lhs) in
            if t = T.BangEq || prev = T.BangEq then error p op "`!=` can't be chained; use `&&`";
            let d = direction t in
            if d <> 0 && !chain_dir <> 0 && d <> !chain_dir then
              error p op "a chain can't mix `<` and `>`; use `&&`";
            if d <> 0 then chain_dir := d;
            lhs := node p A.Chain ~main:op ~first ~lhs:!lhs ~rhs
          end
          else begin
            chain_dir := direction t;
            lhs := node p A.Binop ~main:op ~first ~lhs:!lhs ~rhs
          end;
          built := `Cmp
        end
        else if (t = T.DotDot || t = T.DotDotEq) && 9 >= min then begin
          if !built = `Range then error p op "ranges can't be chained";
          ignore (advance p);
          skip_nl p;
          let hi = expr p ~min:10 ~no_gt in
          let step =
            if peek p = T.Kw_by then (ignore (advance p); expr p ~min:10 ~no_gt) else A.none
          in
          lhs := list p A.Range ~main:op ~first [ !lhs; hi; step ];
          built := `Range
        end
        else if t = T.Question && 2 >= min then begin
          ignore (advance p);
          skip_nl p;
          let a = expr p ~min:0 ~no_gt in
          ignore (expect p T.Colon "`:`");
          let b = expr p ~min:1 ~no_gt in
          lhs := list p A.Ternary ~main:op ~first [ !lhs; a; b ];
          built := `Other
        end
        else continue := false
  done;
  !lhs

and postfix p lhs ~first =
  let tok = advance p in
  match Lexer.tag p.toks tok with
  | T.Dot ->
    let name = expect p T.Ident "a field name" in
    node p A.Field ~main:name ~first ~lhs ~rhs:A.none
  | T.LParen ->
    let args = comma_list p ~is_close:(( = ) T.RParen) (fun p -> expr p ~min:0 ~no_gt:false) in
    list p A.Call ~main:tok ~first (lhs :: args)
  | _ (* `[` *) ->
    skip_nl p;
    let i = expr p ~min:0 ~no_gt:false in
    skip_nl p;
    ignore (expect p T.RBracket "`]`");
    node p A.Index ~main:tok ~first ~lhs ~rhs:i

and prefix p ~no_gt =
  let tok = p.pos in
  match peek p with
  | T.Int -> leaf p A.Int (advance p)
  | T.Float -> leaf p A.Float (advance p)
  | T.String -> leaf p A.Str (advance p)
  | T.Kw_true -> leaf p A.True (advance p)
  | T.Kw_false -> leaf p A.False (advance p)
  | T.Kw_nil -> leaf p A.Nil (advance p)
  | T.Placeholder -> leaf p A.Placeholder (advance p)
  | T.Ident when peek_at p 1 = T.ColonColon -> fn_colon p
  | T.Ident -> leaf p A.Ident (advance p)
  | T.LParen ->
    ignore (advance p);
    skip_nl p;
    let e = expr p ~min:0 ~no_gt:false in
    skip_nl p;
    ignore (expect p T.RParen "`)`");
    e
  | T.LBracket ->
    ignore (advance p);
    let xs = comma_list p ~is_close:(( = ) T.RBracket) (fun p -> expr p ~min:0 ~no_gt:false) in
    list p A.Array ~main:tok ~first:tok xs
  | t when is_open_angle t -> obj p
  | T.Minus | T.Bang | T.Tilde as t ->
    ignore (advance p);
    let e = expr p ~min:17 ~no_gt in
    let lit = A.b_tag p.b e in
    if t = T.Minus && (lit = A.Int || lit = A.Float) && A.b_main p.b e = tok + 1
       && A.b_lhs p.b e = A.none
    then begin
      (* `-5` is one literal; `-(5)` stays a negation *)
      A.set_lhs p.b e 1;
      A.set_first p.b e tok;
      e
    end
    else
      let tag = match t with T.Minus -> A.Neg | T.Bang -> A.Not | _ -> A.Bnot in
      node p tag ~main:tok ~first:tok ~lhs:e ~rhs:A.none
  | T.LBrace -> block p
  | T.Kw_otala -> fn p
  | T.Kw_if -> if_ p
  | T.Kw_for -> for_ p
  | T.Kw_katsa -> katsa p
  | T.Kw_ahket | T.Kw_break as t ->
    ignore (advance p);
    (* only if an operand can start here (grammar doc, part 4, rule 7) *)
    let arg = if can_start_expr (peek p) then expr p ~min:0 ~no_gt else A.none in
    node p (if t = T.Kw_ahket then A.Return else A.Break) ~main:tok ~first:tok ~lhs:arg ~rhs:A.none
  | T.Backtick ->
    ignore (advance p);
    p.quote_depth <- p.quote_depth + 1;
    let body = Fun.protect ~finally:(fun () -> p.quote_depth <- p.quote_depth - 1) (fun () -> block p) in
    node p A.Quote ~main:tok ~first:tok ~lhs:body ~rhs:A.none
  | T.Dollar ->
    if p.quote_depth = 0 then error p tok "`$` only works inside a quote";
    ignore (advance p);
    let e =
      match peek p with
      | T.Ident -> leaf p A.Ident (advance p)
      | T.LParen -> paren_expr p
      | _ -> error p p.pos "expected a name or `(` after `$`"
    in
    node p A.Unquote ~main:tok ~first:tok ~lhs:e ~rhs:A.none
  | T.Kw_cast ->
    ignore (advance p);
    if not (is_open_angle (peek p)) then error p p.pos "expected `<`";
    ignore (advance p);
    let t = typ p in
    ignore (close_angle p);
    let e = paren_expr p in
    node p A.Cast ~main:tok ~first:tok ~lhs:t ~rhs:e
  | _ -> error p p.pos "expected an expression"

and paren_expr p =
  ignore (expect p T.LParen "`(`");
  skip_nl p;
  let e = expr p ~min:0 ~no_gt:false in
  skip_nl p;
  ignore (expect p T.RParen "`)`");
  e

(* `<x: 1, y: 2>` *)
and obj p =
  let tok = advance p in
  let field p =
    let name = expect p T.Ident "a field name" in
    ignore (expect p T.Colon "`:`");
    let v = expr p ~min:0 ~no_gt:true in
    node p A.Field_init ~main:name ~first:name ~lhs:v ~rhs:A.none
  in
  list p A.Object ~main:tok ~first:tok (comma_list p ~is_close:is_close_angle field)

and block p =
  skip_nl p;
  let tok = expect p T.LBrace "`{`" in
  let stmts = statements p ~in_block:true in
  ignore (expect p T.RBrace "`}`");
  list p A.Block ~main:tok ~first:tok stmts

(* A parameter or `with` state variable: `[type | atalo] name [: init]`. *)
and param tag p =
  let ty =
    match peek p with
    | T.Kw_atalo -> leaf p A.Atalo (advance p)
    | T.Ident when (match peek_at p 1 with T.Comma | T.RParen | T.Colon -> true | _ -> false) ->
      A.none
    | _ -> typ p
  in
  let name = expect p T.Ident "a name" in
  let init = if peek p = T.Colon then (ignore (advance p); expr p ~min:0 ~no_gt:false) else A.none in
  if tag = A.State_var && init = A.none then error p p.pos "expected `:` and a starting value";
  node p tag ~main:name ~first:(if ty = A.none then name else first_of p ty) ~lhs:ty ~rhs:init

(* `(params) [-> T] { body }`, shared by `otala` and `::`. All parameters have an initial
   value (an IIFE) or none do. *)
and signature p ~kw =
  let lp = expect p T.LParen "`(`" in
  let ps = comma_list p ~is_close:(( = ) T.RParen) (param A.Param) in
  let with_init = List.filter (fun i -> A.b_rhs p.b i <> A.none) ps in
  if with_init <> [] && List.length with_init <> List.length ps then
    error p kw "give every parameter a value, or none";
  let params = list p A.Params ~main:lp ~first:lp ps in
  let ret = if peek p = T.Arrow then (ignore (advance p); typ p) else A.none in
  let body = block p in
  (params, ret, body)

and fn p =
  let kw = advance p in
  let tparams = if is_open_angle (peek p) then type_params p else A.none in
  let name = if peek p = T.Ident then leaf p A.Name (advance p) else A.none in
  let params, ret, body = signature p ~kw in
  list p A.Fn ~main:kw ~first:kw [ name; tparams; params; ret; body ]

(* `f :: (params) { body }` *)
and fn_colon p =
  let name = leaf p A.Name (advance p) in
  let kw = advance p in
  let params, ret, body = signature p ~kw in
  list p A.Fn_colon ~main:kw ~first:name [ name; A.none; params; ret; body ]

and if_ p =
  let kw = advance p in
  let c = paren_expr p in
  let b = block p in
  let rec thens acc =
    if peek_past_nl p = T.Kw_then then begin
      skip_nl p;
      ignore (advance p);
      let c = paren_expr p in
      let b = block p in
      thens (b :: c :: acc)
    end
    else List.rev acc
  in
  let middle = thens [] in
  let els =
    if peek_past_nl p = T.Kw_else then (skip_nl p; ignore (advance p); block p) else A.none
  in
  list p A.If ~main:kw ~first:kw ((c :: b :: middle) @ [ els ])

and for_ p =
  let kw = advance p in
  ignore (expect p T.LParen "`(`");
  let var, iter =
    if peek p = T.Kw_ever then (ignore (advance p); (A.none, A.none))
    else begin
      let v = leaf p A.Name (expect p T.Ident "a loop variable or `ever`") in
      ignore (expect p T.Kw_of "`of`");
      (v, expr p ~min:0 ~no_gt:false)
    end
  in
  ignore (expect p T.RParen "`)`");
  let state =
    if peek_past_nl p = T.Kw_with then begin
      skip_nl p;
      let w = advance p in
      ignore (expect p T.LParen "`(`");
      let vars = comma_list p ~is_close:(( = ) T.RParen) (param A.State_var) in
      list p A.State ~main:w ~first:w vars
    end
    else A.none
  in
  let body = block p in
  list p A.For ~main:kw ~first:kw [ var; iter; state; body ]

and katsa p =
  let kw = advance p in
  let scrutinee = paren_expr p in
  skip_nl p;
  ignore (expect p T.LBrace "`{`");
  let arm p =
    let pat = pattern p in
    let arrow = expect p T.FatArrow "`=>`" in
    let body = expr p ~min:0 ~no_gt:false in
    node p A.Arm ~main:arrow ~first:(first_of p pat) ~lhs:pat ~rhs:body
  in
  let arms = comma_list ~trailing:true p ~is_close:(( = ) T.RBrace) arm in
  if arms = [] then error p kw "`katsa` needs at least one arm";
  list p A.Katsa ~main:kw ~first:kw (scrutinee :: arms)

and pattern p =
  let tok = p.pos in
  match peek p with
  | T.Ident when peek_at p 1 = T.LParen ->
    ignore (advance p);
    ignore (advance p);
    list p A.Pat_con ~main:tok ~first:tok (comma_list p ~is_close:(( = ) T.RParen) pattern)
  | T.Ident -> leaf p A.Pat_ident (advance p)
  | T.Placeholder when text p tok = "_" -> leaf p A.Pat_wild (advance p)
  | T.Kw_nil | T.Kw_true | T.Kw_false | T.Int | T.Float | T.String -> prefix p ~no_gt:false
  | T.Minus when (match peek_at p 1 with T.Int | T.Float -> true | _ -> false) ->
    prefix p ~no_gt:false  (* a negative number *)
  | _ -> error p tok "expected a pattern"

(* ---------- statements ---------- *)

(* Tries to read `T name: e` (grammar doc, part 4, rule 1). If that fails, rewinds and
   returns None. *)
and try_decl p =
  let save = p.pos and save_last = p.last and mark = A.mark p.b in
  let was = p.speculating in
  p.speculating <- true;
  let ty =
    try
      let ty = if peek p = T.Kw_atalo then leaf p A.Atalo (advance p) else typ p in
      if peek p = T.Ident && peek_at p 1 = T.Colon then Some ty else None
    with Fail -> None
  in
  p.speculating <- was;
  (* rule 2: `x? y: z` is a declaration only if `x` is a type keyword or declared type *)
  let ternary_lookalike ty =
    A.b_tag p.b ty = A.T_opt
    &&
    let inner = A.b_lhs p.b ty in
    let tok = A.b_main p.b inner in
    A.b_tag p.b inner = A.T_named && Lexer.tag p.toks tok = T.Ident
    && not (Hashtbl.mem p.type_names (text p tok))
  in
  match ty with
  | Some ty when not (ternary_lookalike ty) ->
    let name = advance p in
    ignore (advance p);
    let v = expr p ~min:0 ~no_gt:false in
    Some (node p A.Decl ~main:name ~first:(first_of p ty) ~lhs:ty ~rhs:v)
  | _ ->
    p.pos <- save;
    p.last <- save_last;
    A.reset p.b mark;
    None

and statement p =
  let tok = p.pos in
  match peek p with
  | T.Kw_type -> type_def p
  | T.Kw_export ->
    ignore (advance p);
    let def =
      match peek p with
      | T.Kw_type -> type_def p
      | T.Kw_otala when peek_at p 1 = T.Ident || is_open_angle (peek_at p 1) -> fn p
      | T.Ident when peek_at p 1 = T.ColonColon -> fn_colon p
      | _ -> error p p.pos "expected a function or type definition"
    in
    node p A.Export ~main:tok ~first:tok ~lhs:def ~rhs:A.none
  | T.Kw_use ->
    ignore (advance p);
    ignore (expect p T.Ident "a module name");
    ignore (expect p T.Dot "`.`");
    ignore (expect p T.Ident "a name");
    let alias = if peek p = T.Kw_as then (ignore (advance p); expect p T.Ident "a name") else A.none in
    node p A.Use ~main:tok ~first:tok ~lhs:alias ~rhs:A.none
  | T.Ident when peek_at p 1 = T.Colon || peek_at p 1 = T.Eq ->
    ignore (advance p);
    let tag = if peek p = T.Colon then A.Bind else A.Assign in
    ignore (advance p);
    let v = expr p ~min:0 ~no_gt:false in
    node p tag ~main:tok ~first:tok ~lhs:v ~rhs:A.none
  | T.Placeholder when peek_at p 1 = T.Colon || peek_at p 1 = T.Eq -> error p tok "`_` isn't a name"
  | t when can_start_type t || t = T.Kw_atalo -> (
    match try_decl p with Some d -> d | None -> expr p ~min:0 ~no_gt:false)
  | _ -> expr p ~min:0 ~no_gt:false

(* `type name<params> = ...`: an alias if the right side is clearly a type, else an ADT
   (grammar doc, part 4, rule 5). *)
and type_def p =
  let kw = advance p in
  let name = expect p T.Ident "a type name" in
  let tparams = if is_open_angle (peek p) then type_params p else A.none in
  ignore (expect p T.Eq "`=`");
  let alias =
    match peek p with
    | t when is_type_keyword t || is_open_angle t -> true
    | T.LBracket | T.LParen -> true
    | T.Ident -> (
      match peek_at p 1 with
      | t when is_open_angle t -> true
      | T.Question -> true
      | T.LParen | T.Pipe -> false
      | _ -> Hashtbl.mem p.type_names (text p p.pos))
    | _ -> false
  in
  if alias then
    let t = typ p in
    node p A.Type_alias ~main:name ~first:kw ~lhs:tparams ~rhs:t
  else begin
    let variant p =
      let c = expect p T.Ident "a constructor name" in
      let fields =
        if peek p = T.LParen then (ignore (advance p); comma_list p ~is_close:(( = ) T.RParen) typ)
        else []
      in
      list p A.Variant ~main:c ~first:c fields
    in
    let rec variants acc =
      let v = variant p in
      if peek p = T.Pipe then (ignore (advance p); variants (v :: acc)) else List.rev (v :: acc)
    in
    let vs = variants [] in
    list p A.Type_adt ~main:name ~first:kw (tparams :: vs)
  end

(* Statements up to `}` (in a block) or the end of the file. *)
and statements p ~in_block =
  let rec loop acc =
    while peek p = T.Newline || peek p = T.Semi do ignore (advance p) done;
    match peek p with
    | T.Eof -> List.rev acc
    | T.RBrace when in_block -> List.rev acc
    | _ ->
      p.stmt_start <- p.pos;
      let s =
        try
          let s = statement p in
          (match peek p with
           | T.Newline | T.Semi | T.Eof -> ()
           | T.RBrace when in_block -> ()
           | _ -> error p p.pos "expected the end of the statement");
          s
        with Fail ->
          let tok = p.pos in
          recover p ~in_block;
          leaf p A.Error tok
      in
      loop (s :: acc)
  in
  loop []

(* Skips to the next `;` or newline outside braces, or to a `}` that closes the block. *)
and recover p ~in_block =
  let depth = ref 0 in
  let stop = ref false in
  while not !stop do
    match peek p with
    | T.Eof -> stop := true
    | T.Newline | T.Semi when !depth = 0 -> stop := true
    | T.RBrace when !depth = 0 -> if in_block then stop := true else ignore (advance p)
    | T.LBrace -> incr depth; ignore (advance p)
    | T.RBrace -> decr depth; ignore (advance p)
    | _ -> ignore (advance p)
  done

(* ---------- entry ---------- *)

(* Pre-pass: collect every name declared with `type <id>`, so `x? y: z` can be read as a
   declaration when `x` is a type. *)
let type_names toks =
  let h = Hashtbl.create 16 in
  for i = 0 to Lexer.count toks - 2 do
    if Lexer.tag toks i = T.Kw_type && Lexer.tag toks (i + 1) = T.Ident then
      Hashtbl.replace h (Lexer.text toks (i + 1)) ()
  done;
  h

let parse (toks : Lexer.tokens) (diag : Diag.bag) : A.t =
  let p =
    { toks; diag; b = A.builder (); type_names = type_names toks; pos = 0; last = 0;
      stmt_start = 0; speculating = false; quote_depth = 0 }
  in
  let stmts = statements p ~in_block:false in
  let root = list p A.Program ~main:0 ~first:0 stmts in
  A.finish p.b ~root
