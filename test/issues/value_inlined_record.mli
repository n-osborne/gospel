(*@ type t = A of { b : bool } *)

(*@ function f (b : bool) : t = A { b } *)
(* {gospel_expected|
   [125] File "value_inlined_record.mli", line 3, characters 36-37:
         3 | (*@ function f (b : bool) : t = A { b } *)
                                                 ^
         Error: Symbol b not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
