(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constants1.mli.output
   (run %{checker} %{dep:constants1.mli}))))

(rule
 (alias runtest)
 (action
  (diff constants1.mli constants1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constants1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t27.mli.output
   (run %{checker} %{dep:t27.mli}))))

(rule
 (alias runtest)
 (action
  (diff t27.mli t27.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t27.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t28.mli.output
   (run %{checker} %{dep:t28.mli}))))

(rule
 (alias runtest)
 (action
  (diff t28.mli t28.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t28.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t29.mli.output
   (run %{checker} %{dep:t29.mli}))))

(rule
 (alias runtest)
 (action
  (diff t29.mli t29.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t29.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t3.mli.output
   (run %{checker} %{dep:t3.mli}))))

(rule
 (alias runtest)
 (action
  (diff t3.mli t3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t30.mli.output
   (run %{checker} %{dep:t30.mli}))))

(rule
 (alias runtest)
 (action
  (diff t30.mli t30.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t30.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t31.mli.output
   (run %{checker} %{dep:t31.mli}))))

(rule
 (alias runtest)
 (action
  (diff t31.mli t31.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t31.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t32.mli.output
   (run %{checker} %{dep:t32.mli}))))

(rule
 (alias runtest)
 (action
  (diff t32.mli t32.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t32.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t33.mli.output
   (run %{checker} %{dep:t33.mli}))))

(rule
 (alias runtest)
 (action
  (diff t33.mli t33.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t33.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t34.mli.output
   (run %{checker} %{dep:t34.mli}))))

(rule
 (alias runtest)
 (action
  (diff t34.mli t34.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t34.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t35.mli.output
   (run %{checker} %{dep:t35.mli}))))

(rule
 (alias runtest)
 (action
  (diff t35.mli t35.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t35.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t36.mli.output
   (run %{checker} %{dep:t36.mli}))))

(rule
 (alias runtest)
 (action
  (diff t36.mli t36.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t36.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t37.mli.output
   (run %{checker} %{dep:t37.mli}))))

(rule
 (alias runtest)
 (action
  (diff t37.mli t37.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t37.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t38.mli.output
   (run %{checker} %{dep:t38.mli}))))

(rule
 (alias runtest)
 (action
  (diff t38.mli t38.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t38.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t39.mli.output
   (run %{checker} %{dep:t39.mli}))))

(rule
 (alias runtest)
 (action
  (diff t39.mli t39.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (with-accepted-exit-codes 2
    (run ocamlc -c %{dep:t39.mli})))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t4.mli.output
   (run %{checker} %{dep:t4.mli}))))

(rule
 (alias runtest)
 (action
  (diff t4.mli t4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t4.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t5.mli.output
   (run %{checker} %{dep:t5.mli}))))

(rule
 (alias runtest)
 (action
  (diff t5.mli t5.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t5.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t6.mli.output
   (run %{checker} %{dep:t6.mli}))))

(rule
 (alias runtest)
 (action
  (diff t6.mli t6.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t6.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t7.mli.output
   (run %{checker} %{dep:t7.mli}))))

(rule
 (alias runtest)
 (action
  (diff t7.mli t7.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t7.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t8.mli.output
   (run %{checker} %{dep:t8.mli}))))

(rule
 (alias runtest)
 (action
  (diff t8.mli t8.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t8.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to type_arity1.mli.output
   (run %{checker} %{dep:type_arity1.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity1.mli type_arity1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:type_arity1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to type_arity2.mli.output
   (run %{checker} %{dep:type_arity2.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity2.mli type_arity2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (with-accepted-exit-codes 2
    (run ocamlc -c %{dep:type_arity2.mli})))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to type_arity3.mli.output
   (run %{checker} %{dep:type_arity3.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity3.mli type_arity3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (with-accepted-exit-codes 2
    (run ocamlc -c %{dep:type_arity3.mli})))))

