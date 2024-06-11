Running `gospel check --dsource` to test pretty printing

  $ gospel check --dsource lib.mli > tmp.mli
  $ gospel check tmp.mli
  File "tmp.mli", line 13, characters 3-88:
  13 | ... axiom mixfix: exists xs. ((xsGospelstdlib.[42:integer]):integer = 42:
  14 | integer):prop ..
  Error: Syntax error.
  [125]
  $ gospel check --dsource lib.mli
  (*@ open Stdlib *)
  
  (*@ open Gospelstdlib *)
  
  (*@ axiom prefix: exists x. (x = ((-) 42:integer):integer -> integer):prop *)
  
  (*@ axiom infix: exists x_1 y. ((x_1 - y):integer = 0:integer):prop *)
  
  (*@ axiom infix_partial_application: exists x_2 y_1. let f = ((-) x_2):
  integer -> integer in ((apply 
  f y_1):integer = 0:integer):prop *)
  
  (*@ axiom mixfix: exists xs. ((xsGospelstdlib.[42:integer]):integer = 42:
  integer):prop *)
  
  (*@ axiom mixfix_partial_application: exists xs_1. let f_1 = ((Gospelstdlib.[_.._]) 
  xs_1 42:integer):integer -> integer sequence in (mem 
  (apply  f_1 73:integer):integer sequence 42:integer):prop *)
