(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)
open Gospel

let run dumpast verbose files =
  let _, files = typecheck ~verbose files in
  let tast = (List.hd files).fdefs in
  if dumpast then
    Fmt.pf Fmt.stdout "%a@\n" Gospel_typecheckerlibs.Tast.pp_s_signature tast;
  ()
