(rule
 (deps clean_check_not_implemented.t)
 (action
  (with-stdout-to clean_check_not_implemented.out
   (run %{exe:../../../test/golden/runner.exe} %{exe:../../../bin/main.exe} clean_check_not_implemented.t))))

(rule
 (alias runtest)
 (action (diff clean_check_not_implemented.expected clean_check_not_implemented.out)))

(rule
 (deps lex_errors.t)
 (action
  (with-stdout-to lex_errors.out
   (run %{exe:../../../test/golden/runner.exe} %{exe:../../../bin/main.exe} lex_errors.t))))

(rule
 (alias runtest)
 (action (diff lex_errors.expected lex_errors.out)))

(rule
 (deps tokens_program.t)
 (action
  (with-stdout-to tokens_program.out
   (run %{exe:../../../test/golden/runner.exe} %{exe:../../../bin/main.exe} tokens_program.t))))

(rule
 (alias runtest)
 (action (diff tokens_program.expected tokens_program.out)))

