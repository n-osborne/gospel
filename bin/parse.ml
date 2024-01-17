let fmt = Format.std_formatter
let pp = Format.fprintf

let run_file file =
  Gospel.Parser_frontend.parse_ocaml file

(*   let cmi_info = Ocaml_common.Cmi_format.read_cmi file in *)
(*   let cmi_info = { cmi_info with cmi_sign = ocaml } in *)
(*   Cmi_format.output_cmi file stdout cmi_info |> ignore *)

let run files = run_file (List.hd files)
