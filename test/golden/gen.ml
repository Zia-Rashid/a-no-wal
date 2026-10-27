(* Writes the dune rules for a golden test directory: one rule per `x.t` that runs it
   into `x.out`, and one that diffs `x.out` against `x.expected`.

     gen.exe ROOT

   ROOT is the path from the test directory to the project root. The output is checked
   in as `dune.inc`; adding a `.t` file makes `dune test` fail until `dune promote`
   regenerates it. *)

let () =
  let root = match Sys.argv with [| _; r |] -> r | _ -> failwith "usage: gen.exe ROOT" in
  let files =
    Sys.readdir "." |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".t")
    |> List.sort compare
  in
  List.iter
    (fun f ->
      let base = Filename.chop_suffix f ".t" in
      Printf.printf
        "(rule\n\
        \ (deps %s)\n\
        \ (action\n\
        \  (with-stdout-to %s.out\n\
        \   (run %%{exe:%s/test/golden/runner.exe} %%{exe:%s/bin/main.exe} %s))))\n\n\
         (rule\n\
        \ (alias runtest)\n\
        \ (action (diff %s.expected %s.out)))\n\n"
        f base root root f base base)
    files
