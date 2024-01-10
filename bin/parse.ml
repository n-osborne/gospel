let fmt = Format.std_formatter
let pp = Format.fprintf

let run_file file =
  let ocaml = Gospel.Parser_frontend.parse_ocaml file in
  pp fmt "@[# 1 \"%s\"\n%a@]@." file Gospel.Opprintast.signature ocaml

let run = List.iter run_file
