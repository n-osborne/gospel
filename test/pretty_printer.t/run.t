Running `gospel check --dsource` to test pretty printing

  $ gospel check --dsource lib.mli > tmp.mli
  $ gospel check tmp.mli
  File "tmp.mli", line 5, characters 3-114:
  5 | ... axiom prefix: exists x:integer -> integer. (x:integer -> integer = ((-) 42:
  6 | integer):integer -> integer):prop ..
  Error: Syntax error.
  [125]
  $ gospel check --dsource lib.mli
  (*@ open Stdlib *)
  
  (*@ open Gospelstdlib *)
  
  (*@ axiom prefix: exists x:integer -> integer. (x:integer -> integer = ((-) 42:
  integer):integer -> integer):prop *)
  
  (*@ axiom infix: exists x_1:integer y:integer. ((x_1:integer - y:integer):
  integer = 0:integer):prop *)
  
  (*@ axiom infix_partial_application: exists x_2:integer y_1:integer. let 
  f:integer -> integer = ((-) x_2:integer):integer -> integer in ((apply 
  f:integer -> integer y_1:integer):integer = 0:integer):prop *)
  
  (*@ axiom mixfix: exists xs:integer sequence. ((xs:integer sequenceGospelstdlib.[
                                                  42:integer]):integer = 42:
  integer):prop *)
  
  (*@ axiom mixfix_partial_application: exists xs_1:integer sequence. let 
  f_1:integer -> integer sequence = ((Gospelstdlib.[_.._]) xs_1:integer 
                                                           sequence 42:
                                                           integer):integer ->
                                                                    integer 
                                                                    sequence in (mem 
  (apply  f_1:integer -> integer sequence 73:integer):integer sequence
  42:integer):prop *)
