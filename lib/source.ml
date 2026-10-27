(* A source file: its name, its bytes, and offset -> line:col mapping. *)

type t = {
  name : string;
  text : string;
  mutable line_starts : int array option;  (* computed on first use *)
}

let of_string ~name text = { name; text; line_starts = None }

let load path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Ok (of_string ~name:path text)
  | exception Sys_error msg -> Error msg

let line_starts src =
  match src.line_starts with
  | Some a -> a
  | None ->
    let starts = ref [ 0 ] in
    String.iteri (fun i c -> if c = '\n' then starts := (i + 1) :: !starts) src.text;
    let a = Array.of_list (List.rev !starts) in
    src.line_starts <- Some a;
    a

(* 1-based line and column. The column counts code points, not bytes, so Oneida text
   inside a string literal doesn't push later columns out of place. *)
let line_col src offset =
  let starts = line_starts src in
  (* last line start <= offset *)
  let lo = ref 0 and hi = ref (Array.length starts - 1) in
  while !lo < !hi do
    let mid = (!lo + !hi + 1) / 2 in
    if starts.(mid) <= offset then lo := mid else hi := mid - 1
  done;
  let line_start = starts.(!lo) in
  let col = ref 1 in
  for i = line_start to min offset (String.length src.text) - 1 do
    (* count lead bytes only: continuation bytes are 0b10xxxxxx *)
    if Char.code src.text.[i] land 0xC0 <> 0x80 then incr col
  done;
  (!lo + 1, !col)
