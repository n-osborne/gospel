(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to any_not_bool.mli.output
   (run %{checker} %{dep:any_not_bool.mli}))))

(rule
 (alias runtest)
 (action
  (diff any_not_bool.mli any_not_bool.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:any_not_bool.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to axiom.mli.output
   (run %{checker} %{dep:axiom.mli}))))

(rule
 (alias runtest)
 (action
  (diff axiom.mli axiom.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:axiom.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constructor_arity5.mli.output
   (run %{checker} %{dep:constructor_arity5.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_arity5.mli constructor_arity5.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_arity5.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to duplicate_declaration.mli.output
   (run %{checker} %{dep:duplicate_declaration.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_declaration.mli duplicate_declaration.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:duplicate_declaration.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to exception_no_pattern.mli.output
   (run %{checker} %{dep:exception_no_pattern.mli}))))

(rule
 (alias runtest)
 (action
  (diff exception_no_pattern.mli exception_no_pattern.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:exception_no_pattern.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to exn_arity.mli.output
   (run %{checker} %{dep:exn_arity.mli}))))

(rule
 (alias runtest)
 (action
  (diff exn_arity.mli exn_arity.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:exn_arity.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to faulty_header1.mli.output
   (run %{checker} %{dep:faulty_header1.mli}))))

(rule
 (alias runtest)
 (action
  (diff faulty_header1.mli faulty_header1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:faulty_header1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to faulty_header2.mli.output
   (run %{checker} %{dep:faulty_header2.mli}))))

(rule
 (alias runtest)
 (action
  (diff faulty_header2.mli faulty_header2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:faulty_header2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to faulty_header3.mli.output
   (run %{checker} %{dep:faulty_header3.mli}))))

(rule
 (alias runtest)
 (action
  (diff faulty_header3.mli faulty_header3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:faulty_header3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to float_not_int.mli.output
   (run %{checker} %{dep:float_not_int.mli}))))

(rule
 (alias runtest)
 (action
  (diff float_not_int.mli float_not_int.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:float_not_int.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to float_not_integer.mli.output
   (run %{checker} %{dep:float_not_integer.mli}))))

(rule
 (alias runtest)
 (action
  (diff float_not_integer.mli float_not_integer.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:float_not_integer.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to int_not_bool.mli.output
   (run %{checker} %{dep:int_not_bool.mli}))))

(rule
 (alias runtest)
 (action
  (diff int_not_bool.mli int_not_bool.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:int_not_bool.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to integer_not_bool.mli.output
   (run %{checker} %{dep:integer_not_bool.mli}))))

(rule
 (alias runtest)
 (action
  (diff integer_not_bool.mli integer_not_bool.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:integer_not_bool.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to integer_not_int.mli.output
   (run %{checker} %{dep:integer_not_int.mli}))))

(rule
 (alias runtest)
 (action
  (diff integer_not_int.mli integer_not_int.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:integer_not_int.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to invariant4.mli.output
   (run %{checker} %{dep:invariant4.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant4.mli invariant4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant4.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to labeled_arg1.mli.output
   (run %{checker} %{dep:labeled_arg1.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg1.mli labeled_arg1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to labeled_arg2.mli.output
   (run %{checker} %{dep:labeled_arg2.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg2.mli labeled_arg2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to labeled_arg3.mli.output
   (run %{checker} %{dep:labeled_arg3.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg3.mli labeled_arg3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to labeled_arg4.mli.output
   (run %{checker} %{dep:labeled_arg4.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg4.mli labeled_arg4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg4.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to labeled_arg5.mli.output
   (run %{checker} %{dep:labeled_arg5.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg5.mli labeled_arg5.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg5.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to labeled_arg6.mli.output
   (run %{checker} %{dep:labeled_arg6.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg6.mli labeled_arg6.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg6.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to labeled_arg7.mli.output
   (run %{checker} %{dep:labeled_arg7.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg7.mli labeled_arg7.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg7.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to labeled_arg8.mli.output
   (run %{checker} %{dep:labeled_arg8.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg8.mli labeled_arg8.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg8.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to open.mli.output
   (run %{checker} %{dep:open.mli}))))

(rule
 (alias runtest)
 (action
  (diff open.mli open.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:open.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to partial_application.mli.output
   (run %{checker} %{dep:partial_application.mli}))))

(rule
 (alias runtest)
 (action
  (diff partial_application.mli partial_application.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:partial_application.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t1_not_t2.mli.output
   (run %{checker} %{dep:t1_not_t2.mli}))))

(rule
 (alias runtest)
 (action
  (diff t1_not_t2.mli t1_not_t2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t1_not_t2.mli}))))

