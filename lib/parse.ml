open Format
open Lk
module I = Parser.MenhirInterpreter

let parse lexbuf =
  let supplier = Sedlexing.with_tokenizer Lexer.read lexbuf in
  let success = Result.ok in
  let fail = function
    | I.HandlingError env ->
      let state =
        match I.top env with
        | Some (Element (s, _, _, _)) -> I.number s
        | None -> 0
      in
      let msg = Parser_messages.message state in
      let offset = Sedlexing.lexeme_start lexbuf in
      Error (sprintf "Syntax error at offset %d: %s" offset msg)
    | _ -> assert false
  in
  let init =
    Parser.Incremental.sequent (fst @@ Sedlexing.lexing_positions lexbuf)
  in
  try I.loop_handle success fail supplier init with
  | Lexer.Error msg -> Error msg
;;

let from_channel ch =
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  parse lexbuf
;;

let from_string str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  parse lexbuf
;;
