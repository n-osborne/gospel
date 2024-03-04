module Interpreter = Uparser.MenhirInterpreter

let rec parse (lexbuf : Lexing.lexbuf)
    (last_token : Uparser.token * Lexing.position * Lexing.position)
    (last_env : _ Interpreter.env) (checkpoint : 'a Interpreter.checkpoint) :
    ('a, Warnings.error) result =
  match checkpoint with
  | InputNeeded _ -> consume lexbuf checkpoint
  | (Shifting (_, _, _) | AboutToReduce (_, _)) as cp ->
      parse lexbuf last_token last_env (Interpreter.resume cp)
  | Accepted x -> Ok x
  | Rejected -> assert false
  | HandlingError _ -> handle_error last_token last_env

and handle_error _last_token _last_env =
  Result.Error (Ppxlib.Location.none, Warnings.Syntax_error)
(* match Parse_errors.error_message last_env Lexing.dummy_pos last_token with *)
(* | None -> Result.Error "Syntax error (no handler)" *)
(* | Some err -> Result.Error err *)

and consume lexbuf = function
  | Interpreter.InputNeeded env as checkpoint -> (
      match Ulexer.token lexbuf with
      | raw_token ->
          let token = (raw_token, lexbuf.lex_start_p, lexbuf.lex_curr_p) in
          parse lexbuf token env (Interpreter.offer checkpoint token)
      | exception Warnings.Error msg -> Result.Error msg)
  | _ -> assert false

let parse lb inc_par =
  match consume lb (inc_par lb.lex_start_p) with
  | Result.Ok value -> value
  | Result.Error (loc, exn) ->
    let loc = if loc = Ppxlib.Location.none then
      Ppxlib.{ loc_start = lb.lex_start_p; loc_end = lb.lex_curr_p; loc_ghost = false }
    else loc
    in
    Warnings.(error ~loc exn)
