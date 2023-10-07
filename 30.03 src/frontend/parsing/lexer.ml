let digit = [%sedlex.regexp? '0' .. '9']
let int = [%sedlex.regexp? Plus digit]

let alpha = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let id = [%sedlex.regexp? (alpha, alpha | digit | Star '_') ]

open Parser

let rec read_token buf =
  match%sedlex lexbuf with
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "{" -> LBRACE
  | "}" -> RBRACE
