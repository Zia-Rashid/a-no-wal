(* Diagnostics: errors and warnings collected by every compile stage.

   Format:
     file.t:line:col
     message
*)

type severity = Error | Warning

type t = {
  severity : severity;
  src : Source.t;
  start : int;  (* byte offsets into src.text *)
  stop : int;
  msg : string;
}

type bag = { mutable rev_items : t list; mutable errors : int }

let create () = { rev_items = []; errors = 0 }

let add bag d =
  bag.rev_items <- d :: bag.rev_items;
  if d.severity = Error then bag.errors <- bag.errors + 1

let error bag src ~start ~stop msg = add bag { severity = Error; src; start; stop; msg }
let warning bag src ~start ~stop msg = add bag { severity = Warning; src; start; stop; msg }
let has_errors bag = bag.errors > 0
let error_count bag = bag.errors

(* In the order they were reported. *)
let to_list bag = List.rev bag.rev_items

let format d =
  let line, col = Source.line_col d.src d.start in
  let prefix = match d.severity with Error -> "" | Warning -> "warning: " in
  Printf.sprintf "%s:%d:%d\n%s%s" d.src.Source.name line col prefix d.msg
