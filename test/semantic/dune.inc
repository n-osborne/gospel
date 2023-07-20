(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to invariant1.mli.output
   (run %{checker} %{dep:invariant1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant1.mli invariant1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to invariant2.mli.output
   (run %{checker} %{dep:invariant2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant2.mli invariant2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to invariant3.mli.output
   (run %{checker} %{dep:invariant3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant3.mli invariant3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant3.mli}))))

