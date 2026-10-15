(* Surface AST: a node store in SoA form

   A node is an index into parallel columns: [tag], [main] (its main token), [lhs], [rhs],
   and [first]/[last] (the token span). Every tag has one of four shapes, which says how
   [lhs]/[rhs] are read:

     Leaf  no children; lhs/rhs hold raw data (e.g. a literal's sign)
     Un    one child in lhs; rhs holds raw data
     Bin   two children, lhs and rhs
     List  children in [extra], from index lhs, rhs of them

   Optional children are [none]. Stores are built bottom-up by a [builder] and then
   renumbered into pre-order by [finish], so the root is node 0 and node order is source
   order. A finished store is never modified; passes add columns or build a new store. *)

let none = -1 (* a'no:wall uses 0xFFFF_FFFF for u32 columns but OCaml ints use -1 *)

type tag =
  (* leaves *)
  | Int | Float  (* lhs = 1 if a leading `-` was folded in *)
  | Str | True | False | Nil | Ident
  | Placeholder  (* lhs = parameter index, set by the placeholder rewrite *)
  | Matched  (* `_` in a katsa arm body: the value being matched (set by the rewrite) *)
  | Name  (* a name being bound: function name, loop variable *)
  | Atalo  (* `atalo` in a type position *)
  | T_param  (* type parameter `a` in `<a, b>` *)
  | Pat_ident | Pat_wild
  | Use  (* tokens: `use` m . f; lhs = alias token or none *)
  | Error  (* standin for a statement that failed to parse *)
  (* one child *)
  | Neg | Not | Bnot   
  | Field  (* e.name; main = name token *)
  | Field_init  (* name: e inside an object literal; main = name *)
  | Break | Return | Quote | Unquote | Export 
  | Bind | Assign  (* x: e, x = e; main = name *)
  | T_array | T_opt
  | T_field  (* name: T inside an object type; main = name *)
  | Fn_placeholder  (* function made from `_`s; rhs = arity *)
  (* two children *)
  | Binop  (* main = operator token *)
  | Chain  (* lhs is a comparison, rhs the next operand: `a < b <= c` *)
  | Index
  | Decl  (* T x: e; lhs = type (or Atalo), rhs = value; main = name *)
  | Param | State_var  (* lhs = type or none, rhs = initial value or none; main = name *)
  | Arm  (* pattern => body *)
  | Cast  (* type, expr *)
  | Type_alias  (* lhs = type params or none, rhs = type; main = name *)
  (* list *)
  | Program | Block | Array | Object | Call (* callee :: args *)
  | Ternary  (* cond, then, else *)
  | Range  (* lo, hi, step or none; main = `..` or `..=` *)
  | If  (* cond1, block1, cond2, block2, ..., else block or none *)
  | Katsa  (* scrutinee :: arms *)
  | For  (* var or none, iterable or none (ever), State or none, body *)
  | State  (* `with (...)` state variables *)
  | Fn | Fn_colon  (* name or none, type params or none, Params, return type or none, body *)
  | Params | Type_params
  | Pat_con  (* main = constructor; children = sub-patterns *)
  | Type_adt  (* type params or none :: variants; main = name *)
  | Variant  (* main = constructor; children = field types *)
  | T_named  (* main = name or type keyword; children = type arguments *)
  | T_fn  (* parameter types, then the result type last *)
  | T_obj

type shape = Leaf | Un | Bin | List

let shape = function
  | Int | Float | Str | True | False | Nil | Ident | Placeholder | Matched | Name | Atalo | T_param
  | Pat_ident | Pat_wild | Use | Error -> Leaf
  | Neg | Not | Bnot | Field | Field_init | Break | Return | Quote | Unquote | Export | Bind
  | Assign | T_array | T_opt | T_field | Fn_placeholder -> Un
  | Binop | Chain | Index | Decl | Param | State_var | Arm | Cast | Type_alias -> Bin
  | Program | Block | Array | Object | Call | Ternary | Range | If | Katsa | For | State | Fn
  | Fn_colon | Params | Type_params | Pat_con | Type_adt | Variant | T_named | T_fn | T_obj ->
    List

(* Tags are stored one byte each; [all] lists them in declaration order *)
let all =
  [| Int; Float; Str; True; False; Nil; Ident; Placeholder; Matched; Name; Atalo; T_param; Pat_ident;
     Pat_wild; Use; Error; Neg; Not; Bnot; Field; Field_init; Break; Return; Quote; Unquote;
     Export; Bind; Assign; T_array; T_opt; T_field; Fn_placeholder; Binop; Chain; Index; Decl;
     Param; State_var; Arm; Cast; Type_alias; Program; Block; Array; Object; Call; Ternary;
     Range; If; Katsa; For; State; Fn; Fn_colon; Params; Type_params; Pat_con; Type_adt;
     Variant; T_named; T_fn; T_obj |]

let to_int (t : tag) : int = Obj.magic t
let () = Array.iteri (fun i t -> assert (to_int t = i)) all

(* ---------- growable int arrays ---------- *)

module Vec = struct
  type t = { mutable a : int array; mutable n : int }

  let create () = { a = Array.make 64 0; n = 0 }

  let push v x =
    if v.n = Array.length v.a then begin
      let b = Array.make (2 * v.n) 0 in
      Array.blit v.a 0 b 0 v.n;
      v.a <- b
    end;
    v.a.(v.n) <- x;
    v.n <- v.n + 1
end

(* ---------- builder ---------- *)

type builder = {
  b_tag : Vec.t; b_main : Vec.t; b_lhs : Vec.t; b_rhs : Vec.t;
  b_first : Vec.t; b_last : Vec.t; b_extra : Vec.t;
}

let builder () =
  { b_tag = Vec.create (); b_main = Vec.create (); b_lhs = Vec.create (); b_rhs = Vec.create ();
    b_first = Vec.create (); b_last = Vec.create (); b_extra = Vec.create () }

let add b tag ~main ~lhs ~rhs ~first ~last =
  let i = b.b_tag.n in
  Vec.push b.b_tag (to_int tag);
  Vec.push b.b_main main;
  Vec.push b.b_lhs lhs;
  Vec.push b.b_rhs rhs;
  Vec.push b.b_first first;
  Vec.push b.b_last last;
  i

let add_list b tag ~main ~first ~last kids =
  let start = b.b_extra.n in
  List.iter (Vec.push b.b_extra) kids;
  add b tag ~main ~lhs:start ~rhs:(List.length kids) ~first ~last

(* Reading and patching nodes that are still being built. *)
let b_tag b i = all.(b.b_tag.a.(i))
let b_main b i = b.b_main.a.(i)
let b_lhs b i = b.b_lhs.a.(i)
let b_rhs b i = b.b_rhs.a.(i)
let b_first b i = b.b_first.a.(i)
let b_last b i = b.b_last.a.(i)
let set_lhs b i v = b.b_lhs.a.(i) <- v
let set_first b i v = b.b_first.a.(i) <- v

(* Speculative parsing rolls the builder back to a mark. *)
let mark b = (b.b_tag.n, b.b_extra.n)

let reset b (n, ne) =
  List.iter (fun v -> v.Vec.n <- n) [ b.b_tag; b.b_main; b.b_lhs; b.b_rhs; b.b_first; b.b_last ];
  b.b_extra.n <- ne

(* ---------- finished store ---------- *)

type t = {
  id : int;
  tag : Bytes.t;
  main : int array;
  lhs : int array;
  rhs : int array;
  first : int array;
  last : int array;
  extra : int array;
}

let count t = Bytes.length t.tag
let root = 0
let tag t i = all.(Char.code (Bytes.get t.tag i))
let main t i = t.main.(i)
let lhs t i = t.lhs.(i)
let rhs t i = t.rhs.(i)
let first t i = t.first.(i)
let last t i = t.last.(i)

(* Child slots in order, [none] included, from raw columns. *)
let slots tag ~lhs ~rhs ~extra =
  match shape tag with
  | Leaf -> []
  | Un -> [ lhs ]
  | Bin -> [ lhs; rhs ]
  | List -> List.init rhs (fun k -> extra (lhs + k))

let kids t i = slots (tag t i) ~lhs:t.lhs.(i) ~rhs:t.rhs.(i) ~extra:(Array.get t.extra)
let kid t i k = List.nth (kids t i) k
let children t i = List.filter (fun c -> c <> none) (kids t i)

let next_id = ref 0

(* Copies the nodes reachable from [root] into a new store, in pre-order. *)
let finish b ~root =
  let n = b.b_tag.n in
  let b_slots i =
    slots (b_tag b i) ~lhs:b.b_lhs.a.(i) ~rhs:b.b_rhs.a.(i) ~extra:(Array.get b.b_extra.a)
  in
  let order = Vec.create () and renum = Array.make n none in
  let rec visit i =
    renum.(i) <- order.n;
    Vec.push order i;
    List.iter (fun c -> if c <> none then visit c) (b_slots i)
  in
  visit root;
  let m = order.n in
  let col v = Array.init m (fun k -> v.Vec.a.(order.a.(k))) in
  let lhs = col b.b_lhs and rhs = col b.b_rhs in
  let extra = Vec.create () in
  let re c = if c = none then none else renum.(c) in
  for k = 0 to m - 1 do
    let i = order.a.(k) in
    match shape (b_tag b i) with
    | Leaf -> ()
    | Un -> lhs.(k) <- re lhs.(k)
    | Bin -> lhs.(k) <- re lhs.(k); rhs.(k) <- re rhs.(k)
    | List ->
      lhs.(k) <- extra.n;
      List.iter (fun c -> Vec.push extra (re c)) (b_slots i)
  done;
  incr next_id;
  { id = !next_id;
    tag = Bytes.init m (fun k -> Char.chr b.b_tag.a.(order.a.(k)));
    main = col b.b_main; lhs; rhs; first = col b.b_first; last = col b.b_last;
    extra = Array.sub extra.a 0 extra.n }
