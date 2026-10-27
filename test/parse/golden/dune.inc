(rule
 (deps ast_program.t)
 (action
  (with-stdout-to ast_program.out
   (run %{exe:../../../test/golden/runner.exe} %{exe:../../../bin/main.exe} ast_program.t))))

(rule
 (alias runtest)
 (action (diff ast_program.expected ast_program.out)))

(rule
 (deps syntax_errors.t)
 (action
  (with-stdout-to syntax_errors.out
   (run %{exe:../../../test/golden/runner.exe} %{exe:../../../bin/main.exe} syntax_errors.t))))

(rule
 (alias runtest)
 (action (diff syntax_errors.expected syntax_errors.out)))

