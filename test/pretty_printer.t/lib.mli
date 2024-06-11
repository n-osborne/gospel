(*@ axiom prefix : exists x. x = - 42 *)

(*@ axiom infix : exists x y. x - y = 0 *)

(*@ axiom infix_partial_application :
      exists x y. let f = (-) x in  f y = 0 *)

(*@ axiom mixfix : exists xs. xs[42] = 42 *)

(*@ axiom  mixfix_partial_application:
      exists xs. let f = ([_.._]) xs 42 in Sequence.mem (f 73) 42 *)

type t
(*@ mutable model m : integer *)

val p : t -> t
(*@ y = p x
    pure *)

(* Access model of a complex value *)
val f : t -> t
(*@ y = f x
    ensures y.m = (p x).m *)
