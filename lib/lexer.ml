open Format
open Parser

exception Error of string

let space = [%sedlex.regexp? Plus white_space]
let alpha = [%sedlex.regexp? 'A' .. 'Z' | 'a' .. 'z']
let digit = [%sedlex.regexp? '0' .. '9']
let sym = [%sedlex.regexp? alpha, Star (alpha | digit | '_')]

let rec read lexbuf =
  let open Sedlexing in
  match%sedlex lexbuf with
  | space -> read lexbuf
  | ',' -> COMMA
  | '(' -> LPAREN
  | ')' -> RPAREN
  | 0x22a5 -> BOTTOM (* ⊥ *)
  | "_|_" -> BOTTOM
  | 0xac -> NOT (* ¬ *)
  | '~' -> NOT
  | '!' -> NOT
  | 0x2227 -> AND (* ∧ *)
  | "/\\" -> AND
  | '&' -> AND
  | '^' -> AND
  | 0x2228 -> OR (* ∨ *)
  | "\\/" -> OR
  | '|' -> OR
  | 0x2192 -> IMP (* → *)
  | "->" -> IMP
  | 0x21d2 -> PROVES (* ⇒ *)
  | 0x22a2 -> PROVES (* ⊢ *)
  | "=>" -> PROVES
  | "|-" -> PROVES
  | sym -> SYM (Utf8.lexeme lexbuf)
  | eof -> EOF
  | any ->
    let offset = lexeme_start lexbuf in
    let tok = Utf8.lexeme lexbuf in
    raise @@ Error (sprintf "Unexpected character %S at offset %d" tok offset)
  | _ -> failwith "impossible"
;;
