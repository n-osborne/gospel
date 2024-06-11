module Make : functor
  (S : sig
     val annot : bool
   end)
  -> sig
  val print_signature : Tast.signature Fmt.t
end
