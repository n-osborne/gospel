Require Import List.
Open Scope list_scope.
Open Scope type_scope.

Variable combine3   : forall {A B C : Set} ,list A -> list B -> list C -> list (A * B * C).
Variable pat        : Set.
Variable identifier : Set.
Variable constant   : Set.
Variable binop      : Set.
Variable quantifier : Set.

Inductive gospel_ty : Set :=
| Var    : identifier -> gospel_ty
| Bool   : gospel_ty
| Nat    : gospel_ty
| Arrow  : list gospel_ty -> gospel_ty -> gospel_ty
| Tuple  : list gospel_ty -> gospel_ty
| NTuple : list (identifier * gospel_ty) -> gospel_ty
| Schema : identifier -> gospel_ty -> gospel_ty
.

Variable coercions : gospel_ty -> gospel_ty -> Prop.

Variable get_const_ty : constant -> gospel_ty.
Variable get_pat_vars : pat -> gospel_ty -> list (identifier * gospel_ty).

Inductive binder : Set :=
| Raw   : identifier -> binder
| Typed : identifier -> gospel_ty -> binder
.

Inductive term : Set :=
| Ttrue   : term
| Tfalse  : term
| Tconst  : constant -> term
| Tpreid  : identifier -> term
| Tidapp  : identifier -> list term -> term
| Tfield  : term -> identifier -> term
| Tapply  : term -> term -> term
| Tinfix  : term -> identifier -> term -> term
| Tbinop  : term -> binop -> term -> term
| Tnot    : term -> term
| Tif     : term -> term -> term -> term
| Tquant  : quantifier -> list binder -> term -> term
| Tlet    : identifier -> term -> term -> term
| Tcase   : term -> list (pat * term) -> term
| Tcast   : term -> gospel_ty -> term
| Ttuple  : list term -> term
| Trecord : list (identifier * term) -> term
| Tupdate : term -> list (identifier * term) -> term
| Tscope  : identifier -> term -> term
| Told    : term -> term
.

Definition env : Set := list (identifier * gospel_ty).
Variable gamma : env.
Variable ty tau sigma : gospel_ty.
Variable tys : list gospel_ty.
Variable id id0 id1 id2 vty : identifier.
Variable ids : list identifier.
Variable t t0 t1 t2 f a : term.
Variable terms : list term.
Variable binders : list binder.

Inductive Lookup : env -> identifier -> gospel_ty -> Prop :=
| lookup_here  : Lookup ((id, ty) :: gamma) id ty
| lookup_there : Lookup gamma id0 tau -> Lookup ((id1, sigma) :: gamma) id0 tau
.

Inductive Free : env -> identifier -> Prop :=
| free_nil  : Free nil id
| free_cons : Free gamma id0 -> ~ (id0 = id1) -> Free ((id1, ty) :: gamma) id0
.

Variable NotUsed : identifier -> gospel_ty -> Prop.

Inductive FreeVarTy : env -> identifier -> Prop :=
| freevarty_nil  : FreeVarTy nil vty
| freevarty_cons :
    FreeVarTy gamma vty
    -> NotUsed vty ty
    -> FreeVarTy ((id, ty) :: gamma) vty
.

Inductive BindersTy : env -> list binder -> list gospel_ty -> Prop :=
| bindersty_nil   : BindersTy gamma nil nil
| bindersty_raw   :
    BindersTy gamma binders tys
    -> FreeVarTy gamma vty
    -> BindersTy gamma (Raw id :: binders) (Var vty :: tys)
| bindersty_types :
    BindersTy gamma binders tys
    -> BindersTy gamma (Typed id ty :: binders) (ty :: tys)
.

Variable BindersId : list binder -> list identifier -> Prop.

(* TODO for let polymorphism! *)
Variable gen : gospel_ty -> gospel_ty.
Variable check_pattern : gospel_ty -> pat -> Prop.

Inductive typing : env -> term -> gospel_ty -> Prop :=

| R_Ttrue  :
    (* ------------------- *)
    typing gamma Ttrue Bool

| R_Tfalse :
    (* ------------------- *)
    typing gamma Tfalse Bool

(* lookup the constant value in order to infer the type of a constant term *)
| R_Tconst : forall (const : constant),
    (* ------------------------------------------- *)
    typing gamma (Tconst const) (get_const_ty const)

(* lookup in the gammaironment in order to infer the type af a variable *)
| R_Tpreid :
    Lookup gamma id ty
    (* -------------------------------------- *)
    -> typing gamma (Tpreid id) ty

| R_Tidapp :
    Lookup gamma id (Arrow tys ty)
    -> bar gamma terms tys
    (* -------------------------------------- *)
    -> typing gamma (Tidapp id terms) ty

| R_Tapply :
    typing gamma f (Arrow (tau :: nil) sigma)
    -> typing gamma a tau
    (* -------------------------------------- *)
    -> typing gamma (Tapply f a) sigma

| R_Tinfix :
    Lookup gamma id (Arrow (ty :: tau :: nil) sigma)
    -> typing gamma t0 ty
    -> typing gamma t1 tau
    (* -------------------------------------- *)
    -> typing gamma (Tinfix t0 id t0) sigma

| R_Tbinop : forall (op : binop),
     typing gamma t0 Bool
    -> typing gamma t1 Bool
    (* -------------------------------------- *)
    -> typing gamma (Tbinop t0 op t1) Bool

| R_Tnot :
    typing gamma t ty
    (* -------------------------------------- *)
    -> typing gamma (Told t) ty

| R_Tif :
    typing gamma t0 Bool
    -> typing gamma t1 ty
    -> typing gamma t2 ty
    (* -------------------------------------- *)
    -> typing gamma (Tif t0 t1 t2) ty

| R_Tquant : forall (quant : quantifier),
    BindersTy gamma binders tys
    -> BindersId binders ids
    -> typing ((List.combine ids tys) ++ gamma) t ty
    (* ----------------------------------------------------- *)
    -> typing gamma (Tquant quant binders t) (Arrow tys ty)

| R_Tlet :
  typing gamma t0 tau
  -> typing ((id, gen tau) :: gamma) t1 sigma
(* ----------------------------------------------------- *)
  -> typing gamma (Tlet id t0 t1) sigma

| R_Tcase : forall (patterns : list pat),
    typing gamma t tau
    -> Forall (check_pattern tau) patterns
    -> Forall (fun x => typing ((get_pat_vars (fst x) tau) ++ gamma) (snd x) sigma) (combine patterns terms)
    (* -------------------------------------------------------------------------------------------------------- *)
    -> typing gamma (Tcase t (combine patterns terms)) sigma

| R_Tcast :
  typing gamma t tau
  -> coercions tau sigma (* just check that it is a valid coercions *)
  (* ---------------------- *)
  -> typing gamma t sigma

| R_Ttuple :
  bar gamma terms tys (* bar implies that both lists have the same length *)
  (* -------------------------------------- *)
  -> typing gamma (Ttuple terms) (Tuple tys)

| R_Trecord :
  bar gamma terms tys (* bar implies that both lists have the same length *)
  -> NoDup ids
  (* ------------------------------------------------------------------- *)
  -> typing gamma (Trecord (combine ids terms)) (NTuple (combine ids tys))

| R_Tupdate : forall (fieldsid0 fieldsid1 : list identifier) (fieldsval0 fieldsval1 : list term) (fieldsty0 fieldsty1 : list gospel_ty),
    (* check that fields*0 is a subset of fields*1 *)
    Forall (fun id => In id fieldsid1) fieldsid0
    -> Forall (fun val => In val fieldsval1) fieldsval0
    -> Forall (fun ty => In ty fieldsty1) fieldsty0
    (* a bit verbose... *)
    -> NoDup fieldsid0
    -> NoDup fieldsid1
    -> NoDup fieldsval0
    -> NoDup fieldsval1
    -> NoDup fieldsty0
    -> NoDup fieldsty1
    -> typing gamma (Trecord (combine fieldsid1 fieldsval1)) (NTuple (combine fieldsid1 fieldsty1))
    (* really really hugly *)
    -> Forall
         (fun x =>
            match x with
            | (id, t, ty) =>
              (* the type is associated with the field in the record type *)
              In (id, ty) (combine fieldsid1 fieldsty1)
              (* the term has the right type *)
              /\ typing gamma t ty (* non strictly positive occurence of typing... *)
            end)
         (combine3 fieldsid0 fieldsval0 fieldsty0)
    (* ------------------------------------------------------------------- *)
    -> typing gamma (Tupdate (Trecord (combine fieldsid1 fieldsval1)) (combine fieldsid0 fieldsval0)) (NTuple (combine fieldsid1 fieldsty1))

(* don't really know what it is...
  | Tscope  : identifier -> term -> term
*)

| R_Told :
  typing gamma t ty
  -> typing gamma (Told t) ty

with bar : env -> list term -> list gospel_ty -> Prop :=
| bar_nil  : bar gamma nil nil
| bar_cons : typing gamma t ty -> bar gamma terms tys -> bar gamma (t :: terms) (ty :: tys)
.
