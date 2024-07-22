(*@ type t = { a : integer; b : integer; c : bool } *)

(*@ axiom x : let a = { a = 42 } in true *)
(* {gospel_expected|
   [125] File "incomplete_record.mli", line 3, characters 22-32:
         3 | (*@ axiom x : let a = { a = 42 } in true *)
                                   ^^^^^^^^^^
         Error: Some record fields are undefined: c b.
   |gospel_expected} *)
