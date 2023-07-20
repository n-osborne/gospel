(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constructor_args.mli.output
   (run %{checker} %{dep:constructor_args.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_args.mli constructor_args.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_args.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constructor_arity1.mli.output
   (run %{checker} %{dep:constructor_arity1.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_arity1.mli constructor_arity1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_arity1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constructor_arity2.mli.output
   (run %{checker} %{dep:constructor_arity2.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_arity2.mli constructor_arity2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_arity2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constructor_arity3.mli.output
   (run %{checker} %{dep:constructor_arity3.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_arity3.mli constructor_arity3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_arity3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constructor_arity4.mli.output
   (run %{checker} %{dep:constructor_arity4.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_arity4.mli constructor_arity4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_arity4.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to empty_match.mli.output
   (run %{checker} %{dep:empty_match.mli}))))

(rule
 (alias runtest)
 (action
  (diff empty_match.mli empty_match.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:empty_match.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to field_application.mli.output
   (run %{checker} %{dep:field_application.mli}))))

(rule
 (alias runtest)
 (action
  (diff field_application.mli field_application.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:field_application.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to invariant5.mli.output
   (run %{checker} %{dep:invariant5.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant5.mli invariant5.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant5.mli}))))

