(* Placeholder are rewritten so that expressions containing `_` become a function

   1. Each placeholder walks up the parent column to its boundary: a call argument, 
      array element, object field value, value of a binding, or a whole statement.
      It never crosses a block or a parameter list. A bare `_` that is a call argument
      widens to the call itself: `print(_)` is `otala(a) { print(a) }`.
   2. The placeholders under one boundary are numbered in De Bruijn Level fashion.
      so node order is source order, `_k` by k.  
   3. A new store is built with an [Fn_placeholder] node around each boundary, and each
      [Placeholder] leaf holds its parameter index in lhs.

   Exception: a `_` inside a `katsa` arm body is the value being matched. 
              so it becomes a [Matched] leaf instead. *)

module A = Ast

let rewrite (t : A.t) (toks : Lexer.tokens) (diag : Diag.bag) : A.t =
  let n = A.count t in
  let all_holes = List.filter (fun i -> A.tag t i = A.Placeholder) (List.init n Fun.id) in
  if all_holes = [] then t
  else begin
    let parent = Parent.compute t in
    let report ?(warn = false) node msg =
      let start = Lexer.start toks (A.first t node) and stop = Lexer.stop toks (A.last t node) in
      (if warn then Diag.warning else Diag.error) diag toks.src ~start ~stop msg
    in
    (* is [h] in a katsa arm body, with no `otala` in between? *)
    let rec in_arm_body c =
      let q = parent.(c) in
      if q = A.none then false
      else
        match A.tag t q with
        | A.Arm -> c = A.rhs t q || in_arm_body q
        | A.Fn | A.Fn_colon | A.Quote -> false
        | _ -> in_arm_body q
    in
    let matched = Array.make n false in
    List.iter
      (fun h ->
        if in_arm_body h then begin
          matched.(h) <- true;
          if Lexer.text toks (A.main t h) <> "_" then report h "use `_` for the matched value"
        end)
      all_holes;
    let holes = List.filter (fun h -> not matched.(h)) all_holes in
    (* is [c] in a boundary position under its parent [q]? *)
    let boundary_slot q c =
      match A.tag t q with
      | A.Call -> c <> A.kid t q 0
      | A.Decl -> c = A.rhs t q
      | A.Array | A.Field_init | A.Bind | A.Assign | A.Program | A.Block | A.Param | A.State_var -> true
      | _ -> false
    in
    let boundary h =
      let rec up c =
        let q = parent.(c) in
        if q = A.none || boundary_slot q c then c else up q
      in
      let b = up h in
      if b = h && parent.(h) <> A.none && A.tag t parent.(h) = A.Call then parent.(h) else b
    in
    (* group placeholders by boundary, keeping source order *)
    let groups = Hashtbl.create 8 in
    List.iter
      (fun h ->
        let b = boundary h in
        Hashtbl.replace groups b (h :: Option.value (Hashtbl.find_opt groups b) ~default:[]))
      (List.rev holes);
    let index = Array.make n A.none and arity = Array.make n A.none in
    (* boundaries in node order, which is source order, so diagnostics come out in order *)
    let boundaries = List.sort compare (List.of_seq (Hashtbl.to_seq_keys groups)) in
    List.iter
      (fun b ->
        let hs = Hashtbl.find groups b in
        let text h = Lexer.text toks (A.main t h) in
        let plain h = text h = "_" in
        (match List.find_opt (fun h -> plain h <> plain (List.hd hs)) hs with
         | Some h -> report h "use either `_` or `_0`, `_1`, ... in one function"
         | None -> ());
        if plain (List.hd hs) then begin
          List.iteri (fun k h -> index.(h) <- k) hs;
          arity.(b) <- List.length hs
        end
        else begin
          let num h =
            if plain h then 0
            else Option.value (int_of_string_opt (String.sub (text h) 1 (String.length (text h) - 1))) ~default:max_int
          in
          List.iter (fun h -> index.(h) <- num h) hs;
          (* the parameters used must be exactly 0 .. max *)
          let used = List.sort_uniq compare (List.map num hs) in
          let rec first_gap k = function x :: rest when x = k -> first_gap (k + 1) rest | _ -> k in
          let gap = first_gap 0 used in
          if gap < List.length used then report b (Printf.sprintf "`_%d` is never used" gap);
          arity.(b) <- List.length used
        end;
        (* a statement that is only a `_` function throws it away (a block's last
           statement is its value, so that one is fine) *)
        let q = parent.(b) in
        let thrown_away =
          match A.tag t q with
          | A.Program -> true
          | A.Block -> List.nth (A.kids t q) (List.length (A.kids t q) - 1) <> b
          | _ -> false
        in
        if thrown_away then report ~warn:true b "unused function value")
      boundaries;
    (* copy the tree, wrapping each boundary *)
    let bld = A.builder () in
    let rec copy i =
      let tag = A.tag t i in
      let main = A.main t i and first = A.first t i and last = A.last t i in
      let re c = if c = A.none then A.none else copy c in
      let j =
        match A.shape tag with
        | A.Leaf ->
          let tag = if matched.(i) then A.Matched else tag in
          let lhs = if tag = A.Placeholder then index.(i) else A.lhs t i in
          A.add bld tag ~main ~lhs ~rhs:(A.rhs t i) ~first ~last
        | A.Un ->
          let l = re (A.lhs t i) in
          A.add bld tag ~main ~lhs:l ~rhs:(A.rhs t i) ~first ~last
        | A.Bin ->
          let l = re (A.lhs t i) in
          let r = re (A.rhs t i) in
          A.add bld tag ~main ~lhs:l ~rhs:r ~first ~last
        | A.List -> A.add_list bld tag ~main ~first ~last (List.map re (A.kids t i))
      in
      if arity.(i) = A.none then j
      else A.add bld A.Fn_placeholder ~main ~lhs:j ~rhs:arity.(i) ~first ~last
    in
    A.finish bld ~root:(copy A.root)
  end
