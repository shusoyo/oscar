open Oscar

(** Json value *)
type json =
  | Null
  | Bool of bool
  | Number of int  (** Note: not support for float unmber *)
  | String of string
  | Array of json list
  | Object of (string * json) list
[@@deriving show, eq]

module P = struct
  let is_digit = Char.Ascii.is_digit
  let is_letter = Char.Ascii.is_letter
  let not_quot = ( <> ) '"'

  let is_space = function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false
end

let spaces = skip_while P.is_space

(* Lexer *)
let lex p = spaces *> p
let digits = lex (take_while1 P.is_digit)
let lbrace, rbrace = (lex (char '{'), lex (char '}'))
let lbrack, rbrack = (lex (char '['), lex (char ']'))
let comma, colon = (lex (char ','), lex (char ':'))
let true_ = lex (string "true")
let false_ = lex (string "false")
let null_ = lex (string "null")
let string_literal = lex (char '"' *> take_while P.not_quot <* char '"')

(* Parser *)
let json_parser : json parser =
  fix (fun json ->
    let json_null = (fun _ -> Null) <$> null_ in
    let json_bool = (fun x -> Bool (bool_of_string x)) <$> (true_ <|> false_) in
    let json_string = (fun x -> String x) <$> string_literal in
    let json_number = (fun x -> Number (int_of_string x)) <$> digits in

    let json_pair =
      (fun x _ y -> (x, y)) <$> string_literal <*> colon <*> json
    in

    let json_object =
      (fun x -> Object x) <$> lbrace *> sep_by comma json_pair <* rbrace
    in

    let json_array =
      (fun x -> Array x) <$> lbrack *> sep_by comma json <* rbrack
    in

    spaces *> json_null <|> json_bool <|> json_number <|> json_string
    <|> json_array <|> json_object <?> "json")
