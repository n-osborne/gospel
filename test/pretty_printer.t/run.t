Running `gospel check --dsource` to test pretty printing

  $ gospel check --dsource lib.mli > tmp.mli
  $ gospel check tmp.mli
  File "tmp.mli", line 12, characters 3-54:
  12 | (*@ axiom mixfix: exists xs. xsGospelstdlib.[42] = 42 *)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Syntax error.
  [125]
  $ gospel check --dsource lib.mli
  (*@ open Stdlib *)
  
  (*@ open Gospelstdlib *)
  
  (*@ axiom prefix: exists x. x = (-) 42 *)
  
  (*@ axiom infix: exists x_1 y. x_1 - y = 0 *)
  
  (*@ axiom infix_partial_application: exists x_2 y_1. let f = (-) x_2 in 
  f y_1 = 0 *)
  
  (*@ axiom mixfix: exists xs. xsGospelstdlib.[42] = 42 *)
  
  (*@ axiom mixfix_partial_application: exists xs_1. let f_1 = (Gospelstdlib.[_.._]) 
  xs_1 42 in Gospelstdlib.Sequence.mem
  f_1 73 42 *)
  
  type t
       (*@ ephemeral
           mutable model m : integer *)
  
  val p : t -> t
  (*@ y_2 = p x_3
      pure*)
  
  val f_2 : t -> t
  (*@ y_3 = f_2 x_4
      ensures y_3.m = (p x_4).m*)

