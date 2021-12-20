open Parser

exception UnknownToken of string

let space = [%sedlex.regexp? Plus white_space]
let alpha = [%sedlex.regexp? 'A' .. 'Z' | 'a' .. 'z']
let greek = [%sedlex.regexp? 0x391 .. 0x3a9 | 0x3b1 .. 0x3c9]
let digit = [%sedlex.regexp? '0' .. '9']
let sym = [%sedlex.regexp? (alpha | greek), Star (alpha | greek | digit | '_')]

let rec read lexbuf =
  match%sedlex lexbuf with
  | space -> read lexbuf
  | ',' -> COMMA
  | '(' -> LPAREN
  | ')' -> RPAREN
  | 0xac -> NOT (* ¬ *)
  | '~' -> NOT
  | '!' -> NOT
  | 0x2227 -> AND (* ∧ *)
  | '&' -> AND
  | '^' -> AND
  | 0x2228 -> OR (* ∨ *)
  | '|' -> OR
  | 'v' -> OR
  | 0x2192 -> IMP (* → *)
  | "->" -> IMP
  | 0x21d2 -> PROVES (* ⇒ *)
  | 0x22a2 -> PROVES (* ⊢ *)
  | "|-" -> PROVES
  | sym ->
      let s = Sedlexing.Utf8.lexeme lexbuf in
      SYM s
  | eof -> EOF
  | _ ->
      let s = Sedlexing.Utf8.lexeme lexbuf in
      raise (UnknownToken s)
