open Oparser
open Uattr2spec

exception Ocaml_syntax_error of Location.t

let () = Location.register_error_of_exn (function
             | Ocaml_syntax_error loc ->
                Some (Location.errorf ~loc "OCaml syntax error")
             | _ -> None )

exception FileNotFound of string

let open_file load_path file =
  let exception Break of in_channel in
  let try_open d = try
      let f = Filename.concat d file in
      let c = open_in f in raise (Break c)
    with Sys_error _ -> () in
  if not (Filename.is_relative file) then open_in file
  else try List.iter try_open load_path;
           raise (FileNotFound file)
       with Break c -> c

(** Parse the given *.mli file -- it must be an interface.
 Raises FileNotFound if file does not exist. *)
let parse_ocaml load_path file =
  let ch = open_file load_path file in
  let lb = Lexing.from_channel ch in
  Location.init lb file;
  try interface Olexer.token lb with
    Error -> begin
      let spos,fpos = lb.lex_start_p, lb.lex_curr_p in
      let loc = Location.{loc_start=spos; loc_end=fpos;loc_ghost=false}  in
      raise (Ocaml_syntax_error loc) end

(** Parse the attributes as GOSPEL specification. Raises FileNotFound
   if file does not exist. *)
let parse_gospel sign = signature sign

(** Raises FileNotFound if file does not exist. *)
let parse_ocaml_gospel load_path file =
  parse_gospel (parse_ocaml load_path file)
