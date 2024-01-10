(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module W = Warnings
open Ppxlib

let stdlib = "Stdlib"
let gospelstdlib = "Gospelstdlib"
let gospelstdlib_file = "gospelstdlib.mli"

let with_loadpath load_path file =
  let exception Break of string in
  let try_open d =
    try
      let f = Filename.concat d file in
      if Sys.file_exists f then raise (Break f)
    with Sys_error _ -> ()
  in
  if file = gospelstdlib_file then file
  else if Filename.is_relative file then
    try
      List.iter try_open load_path;
      raise Not_found
    with Break c -> c
  else if Sys.file_exists file then file
  else raise Not_found

let process_attribute attr =
  match attr.attr_payload with
  | PStr
      [
        ({
           pstr_desc =
             Pstr_eval
               ( ({
                    pexp_desc = Pexp_constant (Pconst_string (spec, loc, opt));
                    _;
                  } as exp),
                 attrs );
           _;
         } as pstr);
      ]
    when String.starts_with ~prefix:"@gospel" spec ->
      let spec = String.sub spec 7 (String.length spec - 7) in
      let pexp_desc = Pexp_constant (Pconst_string (spec, loc, opt)) in
      let pstr_desc = Pstr_eval ({ exp with pexp_desc }, attrs) in
      let attr_payload = PStr [ { pstr with pstr_desc } ] in
      let attr_name = { attr.attr_name with txt = "gospel" } in
      { attr with attr_payload; attr_name }
  | _ -> attr

let rec signature = function
  | [] -> []
  | ({ psig_desc = Psig_attribute attr; _ } as x) :: xs ->
      let attr = process_attribute attr in
      let psig_desc = Psig_attribute attr in
      { x with psig_desc } :: signature xs
  | x :: xs -> x :: signature xs

let rec attributes = function
  | [] -> []
  | x :: xs -> process_attribute x :: attributes xs

let merge =
  object
    inherit Ast_traverse.map as super
    method! signature s = super#signature s |> signature
    method! attributes attrs = super#attributes attrs |> attributes
  end

let parse_ocaml_lb lb =
  let parsetree =
    try Oparse.interface lb
    with _ ->
      let loc_start, loc_end = (lb.lex_start_p, lb.lex_curr_p) in
      let loc = Location.{ loc_start; loc_end; loc_ghost = false } in
      W.error ~loc W.Syntax_error
  in
  merge#signature parsetree

let parse_ocaml file =
  let lb =
    if String.equal file gospelstdlib_file then
      Lexing.from_string Gospellib.contents
    else open_in file |> Lexing.from_channel
  in
  Location.init lb file;
  parse_ocaml_lb lb

module B = Ast_builder.Make (struct
  let loc = Location.none
end)

let parse_gospel ~filename signature name =
  let open_gospelstdlib =
    let payload = PStr [ B.(pstr_eval (estring "open Gospelstdlib")) [] ] in
    let name = { txt = "gospel"; loc = Location.none } in
    B.attribute ~name ~payload |> B.psig_attribute
  in
  let open_stdlib =
    let payload = PStr [ B.(pstr_eval (estring "open Stdlib")) [] ] in
    let name = { txt = "gospel"; loc = Location.none } in
    B.attribute ~name ~payload |> B.psig_attribute
  in
  let s =
    if
      String.equal name gospelstdlib
      || String.equal name stdlib
      || String.equal name "CamlinternalFormatBasics"
    then signature
    else open_stdlib :: open_gospelstdlib :: signature
  in
  Uattr2spec.signature ~filename s

let path2module p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

let parse_ocaml_gospel path =
  let module_name = path2module path in
  let ocaml = parse_ocaml path in
  parse_gospel ~filename:path ocaml module_name
