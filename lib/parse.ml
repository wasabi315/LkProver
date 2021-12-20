let parse lexbuf =
  let lexer = Sedlexing.with_tokenizer Lexer.read lexbuf in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.sequent
  in
  parser lexer

let from_channel ch =
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  parse lexbuf

let from_string str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  parse lexbuf
