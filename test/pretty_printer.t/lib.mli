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

(* Use pattern matching in specifications *)
val g : t -> t list -> t
(*@ r = g x xs
    ensures r = match xs with
                | [] -> x
                | y :: _ -> y *)

(* Use nested pattern matching in specifications *)
val f8 : t -> t list -> t
(*@ r = f8 x xs
    ensures r = match xs with
                | y :: _ -> (match y.m with
                             | 42 -> x
                             | _ -> y)
                | [] -> x *)

(* Ghost type declaration *)
(*@ type g = A of integer | B of (bool * integer) *)
(*@ type r = { a : integer; b : bool } *)

(* Relative paths *)
type 'a ty
(*@ mutable model m : 'a Sequence.t *)

val empty : unit -> 'a ty
(*@ t = empty ()
    ensures t.m = Sequence.empty *)

