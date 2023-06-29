type t = C of int * int

val f : int -> t -> unit
(*@ f n t
    requires let x = (n, n) in C x = C x *)

(* {gospel_expected|
   [125] File "constructor_arity1.mli", line 5, characters 31-32:
         5 |     requires let x = (n, n) in C x = C x *)
                                            ^
         Error: The constructor `C' expects `2' argument(s)
                but is applied to 1 argument(s) here.
   |gospel_expected} *)
