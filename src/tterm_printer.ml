(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Tterm
open Symbols
open Ttypes
open Utils
open Fmt

module Make (S : sig
  val annot : bool
end) =
struct
  let print_vs fmt { vs_name; vs_ty } =
    if S.annot then
      pp fmt "@[(%a:%a)@]" Ident.pp_last_with_tag vs_name print_ty vs_ty
    else pp fmt "@[%a@]" Ident.pp_last_with_tag vs_name

  let print_ls_decl fmt { ls_name; ls_args; ls_value; _ } =
    let is_func = Option.is_some ls_value in
    let print_unnamed_arg fmt ty = pp fmt "(_:%a)" print_ty ty in
    pp fmt "%s %a %a%s%a"
      (if is_func then "function" else "predicate")
      Ident.pp_last_with_tag ls_name
      (list ~sep:sp print_unnamed_arg)
      ls_args
      (if is_func then " : " else "")
      (option print_ty) ls_value

  let print_ls_nm fmt { ls_name; _ } = pp fmt "%a" Ident.pp_relative ls_name
  let protect_on x s = if x then "(" ^^ s ^^ ")" else s

  let rec print_pat_node pri fmt p =
    match p.p_node with
    | Pwild -> pp fmt "_"
    | Pvar v -> print_vs fmt v
    | Pas (p, v) ->
        pp fmt (protect_on (pri > 1) "%a as %a") (print_pat_node 1) p print_vs v
    | Por (p, q) ->
        pp fmt
          (protect_on (pri > 0) "%a | %a")
          (print_pat_node 0) p (print_pat_node 0) q
    | Papp (cs, pl) when is_fs_tuple cs ->
        pp fmt
          (protect_on (pri > 0) "%a")
          (list ~sep:comma (print_pat_node 1))
          pl
    | Papp (cs, []) -> print_ls_nm fmt cs
    | Papp (cs, [ pl ]) -> pp fmt "%a@ %a" print_ls_nm cs (print_pat_node 2) pl
    (* There is only one constructor with non normal fixity to pattern match on:
       the cons one *)
    | Papp (cs, [ left; right ]) when cs.ls_name.id_fixity = Infix ->
        pp fmt "%a %a %a" (print_pat_node 2) left print_ls_nm cs
          (print_pat_node 2) right
    | Papp (cs, pl) ->
        pp fmt "%a@ (%a)" print_ls_nm cs (list ~sep:comma (print_pat_node 2)) pl
    | Pconst c -> Opprintast.constant fmt c
    | Pinterval (c1, c2) -> pp fmt "%C..%C" c1 c2

  let print_pattern = print_pat_node 0

  let print_binop fmt = function
    | Tand -> pp fmt "/\\"
    | Tor -> pp fmt "\\/"
    | Timplies -> pp fmt "->"
    | Tiff -> pp fmt "<->"
    | Tand_asym -> pp fmt "&&"
    | Tor_asym -> pp fmt "||"

  let print_quantifier fmt = function
    | Tforall -> pp fmt "forall"
    | Texists -> pp fmt "exists"

  (* TODO use pretty printer from why3 *)
  let rec print_term par fmt { t_node; t_ty; t_attrs; _ } =
    let paren = if par then parens else Fun.id in
    let print_ty fmt ty =
      match ty with
      | None -> pp fmt ":prop"
      | Some ty -> pp fmt ":%a" print_ty ty
    in
    let annotated p fmt x =
      if S.annot then pp fmt "(%a%a)" p x print_ty t_ty else pp fmt "%a" p x
    in
    let print_t_node fmt t_node =
      match t_node with
      | Tconst c -> pp fmt "%a" (annotated Opprintast.constant) c
      | Ttrue -> pp fmt "%a" (annotated string) "true"
      | Tfalse -> pp fmt "%a" (annotated string) "false"
      | Tvar vs ->
          pp fmt "%a" print_vs vs;
          assert (vs.vs_ty = Option.get t_ty) (* TODO remove this *)
      | Tapp (ls, tl) -> (
          match ls.ls_name.id_fixity with
          | Identifier.Prefix -> (
              match tl with
              (* partial application: zero argument *)
              | [] ->
                  pp fmt "(%a)" (annotated Ident.pp_full_with_tag) ls.ls_name
              (* complete application: one or many arguments *)
              | _ ->
                  let aux fmt (id, args) =
                    pp fmt "%a %a" Ident.pp_full_with_tag id
                      (list (print_term true))
                      args
                  in
                  pp fmt "%a" (paren (annotated aux)) (ls.ls_name, tl))
          | Identifier.Infix -> (
              match tl with
              (* partial applications: zero or one argument *)
              | [] ->
                  pp fmt "(%a)" (annotated Ident.pp_last_with_tag) ls.ls_name
              | [ x ] ->
                  let aux fmt (id, arg) =
                    pp fmt "(%a) %a" Ident.pp_last_with_tag id (print_term true)
                      arg
                  in
                  pp fmt "%a" (paren (annotated aux)) (ls.ls_name, x)
              (* total application *)
              | [ x0; x1 ] ->
                  let par = not Symbols.(ls_equal ls ps_equ) in
                  let aux fmt (left, id, right) =
                    pp fmt "%a %a %a" (print_term par) left
                      Ident.pp_last_with_tag id (print_term par) right
                  in
                  pp fmt "%a" (paren (annotated aux)) (x0, ls.ls_name, x1)
              | _ -> failwith "three-arguments infix symbols shouldn't happen")
          | Identifier.Mixfix -> (
              (* Mixfix symbols are only builtin so they neither begin nor ends
                 with an underscore character. They also assume that the first
                 argument appear before the symbol. *)
              let exploded =
                String.split_on_char '_'
                  (str "%a" Ident.pp_last_with_tag ls.ls_name)
              in
              match Int.compare (List.length tl) (List.length exploded) with
              (* complete application: one argument before the symbol and then
                 one for each underscore *)
              | 0 ->
                  let xs = List.combine tl exploded in
                  (* TODO: in theory no need to pernethsize all the argument,
                     only the first one *)
                  let aux fmt (a, s) = pp fmt "%a%s" (print_term true) a s in
                  pp fmt "%a" (paren (annotated (list aux))) xs
              (* partial application *)
              | i when i < 0 ->
                  let aux fmt (id, args) =
                    pp fmt "(%a) %a" Ident.pp_full_with_tag id
                      (list ~sep:sp (print_term true))
                      args
                  in
                  pp fmt "%a" (paren (annotated aux)) (ls.ls_name, tl)
              | _ ->
                  failwith
                    "No mixfix symbols are defined with an arity greater than \
                     the number of underscore + 1 ")
          | Identifier.Normal when ls_equal ls Symbols.fs_apply ->
              pp fmt "%a"
                (paren (annotated (list ~sep:sp (print_term true))))
                tl
          | Identifier.Normal ->
              let aux fmt (id, args) =
                pp fmt "%a%a" Ident.pp_relative id
                  (list ~first:sp ~sep:sp (print_term true))
                  args
              in
              pp fmt "%a" (paren (annotated aux)) (ls.ls_name, tl))
      | Tfield (t, ls) ->
          pp fmt "%a.%a" (print_term true) t Ident.pp_relative ls.ls_name
      | Tnot t ->
          let par = match t.t_node with Tbinop _ -> true | _ -> false in
          let aux fmt t = pp fmt "not %a" (print_term par) t in
          pp fmt "%a" (paren aux) t
      | Tif (t1, t2, t3) ->
          let aux fmt (t1, t2, t3) =
            pp fmt "if %a then %a else %a" (print_term false) t1
              (print_term false) t2 (print_term false) t3
          in
          pp fmt "%a" (paren aux) (t1, t2, t3)
      | Tlet (vs, t1, t2) ->
          let aux fmt (vs, t1, t2) =
            pp fmt "let %a = %a in %a" print_vs vs (print_term false) t1
              (print_term false) t2
          in
          pp fmt "%a" (paren aux) (vs, t1, t2)
      | Tbinop (op, t1, t2) ->
          let par1 = match t1.t_node with Tbinop _ -> true | _ -> false
          and par2 = match t2.t_node with Tbinop _ -> true | _ -> false in
          let aux fmt (t1, op, t2) =
            pp fmt "%a %a %a" (print_term par1) t1 print_binop op
              (print_term par2) t2
          in
          pp fmt "%a" (paren aux) (t1, op, t2)
      | Tquant (q, vsl, t) ->
          let aux fmt (q, vsl, t) =
            pp fmt "%a %a. %a" print_quantifier q (list ~sep:sp print_vs) vsl
              (print_term false) t
          in
          pp fmt "%a" (paren aux) (q, vsl, t)
      | Tlambda (pl, t) ->
          let aux fmt (pl, t) =
            pp fmt "fun %a -> %a"
              (list ~sep:sp print_pattern)
              pl (print_term false) t
          in
          pp fmt "%a" (paren aux) (pl, t)
      | Tcase (t, ptl) ->
          let print_branch fmt (p, g, t) =
            let f = match t.t_node with Tcase _ -> parens | _ -> Fun.id in
            match g with
            | None ->
                pp fmt "| @[%a@] -> @[%a@]" print_pattern p
                  (f (print_term false))
                  t
            | Some g ->
                pp fmt "| @[%a@] when @[%a@] -> @[%a@]" print_pattern p
                  (print_term false) g
                  (f (print_term false))
                  t
          in
          let aux fmt (case, branches) =
            pp fmt "match %a with@\n%a" (print_term false) case
              (list ~sep:newline print_branch)
              branches
          in
          pp fmt "%a" (paren (annotated aux)) (t, ptl)
      | Told t ->
          let aux fmt t = pp fmt "old %a" (print_term true) t in
          pp fmt "%a" (paren aux) t
    in
    let print_attrs fmt = List.iter (pp fmt "[%@ %s]") in
    pp fmt "%a%a" print_attrs t_attrs print_t_node t_node

  let print_term = print_term false
end
