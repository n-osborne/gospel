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

(* invariant *)
type my_list
(*@ model contents : integer Sequence.t
    with l invariant Sequence.mem l.contents 42
           invariant l.contents <> Sequence.empty *)

(* let binding *)
val f9 : 'a ty -> 'a ty
(*@ r = f9 x
    ensures let m = old x.m in x.m = m *)

(* lambda binding *)
val f10 : int ty list -> int ty list
(*@ r = f10 x
    ensures r = List.map (fun y -> y) x *)

(* more parenthesis *)
(*@ axiom a : forall x y z. (x < y \/ y < z) /\ (x = 42 -> y = 73) *)
(*@ axiom b : forall x y z. (x < y || y < z) && (x = 42 -> y = 73) *)
(*@ axiom c : forall x y z. (x + 3 < y || y < z) && (x + y = 42 -> y = 73) *)

(*@ open Gospelstdlib *)
(*@ open Stdlib *)
