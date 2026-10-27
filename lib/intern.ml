(* String pool: each distinct string gets a small int id

   single threaded(rn) since parsing is main user. 
   Sharding by thread like how Zig's InternPool packs the thread id 
   into the high bits comes with the parallel passes
*)

type t = {
  ids : (string, int) Hashtbl.t;
  mutable strings : string array;
  mutable count : int;
}

let create () = { ids = Hashtbl.create 256; strings = Array.make 256 ""; count = 0 }

let intern pool s =
  match Hashtbl.find_opt pool.ids s with
  | Some id -> id
  | None ->
    let id = pool.count in
    if id = Array.length pool.strings then begin
      let bigger = Array.make (2 * id) "" in
      Array.blit pool.strings 0 bigger 0 id;
      pool.strings <- bigger
    end;
    pool.strings.(id) <- s;
    pool.count <- id + 1;
    Hashtbl.add pool.ids s id;
    id

let get pool id =
  if id < 0 || id >= pool.count then invalid_arg "Intern.get";
  pool.strings.(id)

let count pool = pool.count
