Running `gospel check --dsource` to test pretty printing

  $ gospel check --dsource lib.mli > tmp.mli
  $ gospel check tmp.mli

  $ gospel check --dsource lib.mli
  (*@ open Stdlib *)
  
  (*@ open Gospelstdlib *)
  
  (*@ axiom prefix: exists x. x = (-) 42 *)
  
  (*@ axiom infix: exists x_1 y. x_1 - y = 0 *)
  
  (*@ axiom infix_partial_application: exists x_2 y_1. let f = (-) x_2 in 
  f y_1 = 0 *)
  
  (*@ axiom mixfix: exists xs. xs[42] = 42 *)
  
  (*@ axiom mixfix_partial_application: exists xs_1. let f_1 = ([_.._]) 
  xs_1 42 in Sequence.mem
  (f_1 73) 42 *)
  
  type t
       (*@ ephemeral
           mutable model m : integer *)
  
  val p : t -> t
  (*@ y_2 = p x_3
      pure*)
  
  val f_2 : t -> t
  (*@ y_3 = f_2 x_4
      ensures y_3.m = (p x_4).m*)
  
  val g : t -> t list -> t
  (*@ r = g x_5 xs_2
      ensures r = match xs_2 with
              | [] -> x_5
              | y_4 :: _ -> y_4*)
  
  val f8 : t -> t list -> t
  (*@ r_1 = f8 x_6 xs_3
      ensures r_1 = match xs_3 with
              | y_5 :: _ -> (match y_5.m with
                             | 42 -> x_6
                             | _ -> y_5)
              | [] -> x_6*)
  
  (*@ type g_1 = A of integer
                 | B of bool * integer
            *)
  
  (*@ type r_2 = { a_1 : integer; b : bool }
            *)
  
  type 'a ty
       (*@ ephemeral
           mutable model m : 'a sequence *)
  
  val empty : unit -> 'a ty
  (*@ t_1 = empty ()
      ensures t_1.m = Sequence.empty*)
  
  type my_list
       (*@ model contents : integer sequence
           with l
           invariant Sequence.mem l.contents 42
           invariant not l.contents = Sequence.empty *)
  
  val f9 : 'a ty -> 'a ty
  (*@ r_3 = f9 x_7
      ensures let m_2 = old x_7.m in x_7.m = m_2*)
  
  val f10 : int ty list -> int ty list
  (*@ r_4 = f10 x_8
      ensures r_4 = List.map (fun y_6 -> y_6) x_8*)
  $ gospel check --dsource lib.mli
  (*@ open Stdlib *)
  
  (*@ open Gospelstdlib *)
  
  (*@ axiom prefix: exists x. x = (-) 42 *)
  
  (*@ axiom infix: exists x_1 y. x_1 - y = 0 *)
  
  (*@ axiom infix_partial_application: exists x_2 y_1. let f = (-) x_2 in 
  f y_1 = 0 *)
  
  (*@ axiom mixfix: exists xs. xs[42] = 42 *)
  
  (*@ axiom mixfix_partial_application: exists xs_1. let f_1 = ([_.._]) 
  xs_1 42 in Sequence.mem
  (f_1 73) 42 *)
  
  type t
       (*@ ephemeral
           mutable model m : integer *)
  
  val p : t -> t
  (*@ y_2 = p x_3
      pure*)
  
  val f_2 : t -> t
  (*@ y_3 = f_2 x_4
      ensures y_3.m = (p x_4).m*)
  
  val g : t -> t list -> t
  (*@ r = g x_5 xs_2
      ensures r = match xs_2 with
              | [] -> x_5
              | y_4 :: _ -> y_4*)
  
  val f8 : t -> t list -> t
  (*@ r_1 = f8 x_6 xs_3
      ensures r_1 = match xs_3 with
              | y_5 :: _ -> (match y_5.m with
                             | 42 -> x_6
                             | _ -> y_5)
              | [] -> x_6*)
  
  (*@ type g_1 = A of integer
                 | B of bool * integer
            *)
  
  (*@ type r_2 = { a_1 : integer; b : bool }
            *)
  
  type 'a ty
       (*@ ephemeral
           mutable model m : 'a sequence *)
  
  val empty : unit -> 'a ty
  (*@ t_1 = empty ()
      ensures t_1.m = Sequence.empty*)
  
  type my_list
       (*@ model contents : integer sequence
           with l
           invariant Sequence.mem l.contents 42
           invariant not l.contents = Sequence.empty *)
  
  val f9 : 'a ty -> 'a ty
  (*@ r_3 = f9 x_7
      ensures let m_2 = old x_7.m in x_7.m = m_2*)
  
  val f10 : int ty list -> int ty list
  (*@ r_4 = f10 x_8
      ensures r_4 = List.map (fun y_6 -> y_6) x_8*)
  
  (*@ axiom a_2: forall x_9 y_7 z. (x_9 < y_7 \/ y_7 < z) /\ (x_9 = 42 -> 
                                                              y_7 = 73) *)
  
  (*@ axiom b_1: forall x_10 y_8 z_1. (x_10 < y_8 || y_8 < z_1) && (x_10 = 42 -> 
                                                                    y_8 = 73) *)
  
  (*@ axiom c: forall x_11 y_9 z_2. ((x_11 + 3) < y_9 || y_9 < z_2) && 
  (x_11 + y_9 = 42 -> y_9 = 73) *)






