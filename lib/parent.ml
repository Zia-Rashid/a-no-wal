(* Parent links: one pass, one new column. parent.(root) = Ast.none. *)

let compute (t : Ast.t) =
  let parent = Array.make (Ast.count t) Ast.none in
  for i = 0 to Ast.count t - 1 do
    List.iter (fun c -> parent.(c) <- i) (Ast.children t i)
  done;
  parent
