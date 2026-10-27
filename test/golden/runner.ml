(* Golden test runner (testing plan, part 2).

     runner.exe ANWL FILE.t

   Runs `ANWL <args> FILE.t`, where <args> comes from a first line `%% args: ...` in the
   file (default: `run`). Prints the program's stdout, then its stderr, then `[exit N]` if
   N isn't 0. dune diffs that against FILE.expected. *)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let args_of file =
  let first = match String.index_opt (read_file file) '\n' with
    | Some i -> String.sub (read_file file) 0 i
    | None -> read_file file
  in
  let prefix = "%% args:" in
  if String.starts_with ~prefix first then
    String.sub first (String.length prefix) (String.length first - String.length prefix)
    |> String.split_on_char ' '
    |> List.filter (fun a -> a <> "")
  else [ "run" ]

let () =
  match Sys.argv with
  | [| _; anwl; file |] ->
    let out = Filename.temp_file "anwl" ".out" and err = Filename.temp_file "anwl" ".err" in
    let cmd = Filename.quote_command anwl (args_of file @ [ file ]) ~stdout:out ~stderr:err in
    let code = Sys.command cmd in
    print_string (read_file out);
    print_string (read_file err);
    if code <> 0 then Printf.printf "[exit %d]\n" code;
    Sys.remove out;
    Sys.remove err
  | _ ->
    prerr_endline "usage: runner.exe ANWL FILE.t";
    exit 2
