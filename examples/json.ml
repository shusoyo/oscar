open Oscar

(** Json value *)
type json_value =
  | Null
  | Bool of bool
  | Number of int  (** Note: not support for float unmber *)
  | String of string
  | Array of json_value list
  | Object of (string * json_value) list
[@@deriving show, eq]

module P = struct
  let is_digit = Char.Ascii.is_digit
  let is_letter = Char.Ascii.is_letter
  let not_quot = ( <> ) '"'
  let is_space = function ' ' | '\t' -> true | _ -> false
end

let spaces = skip_while P.is_space

(* Lexer *)
let lex p = p <* spaces
let digits = lex (take_while1 P.is_digit)
let lbrace = lex (char '{')
let rbrace = lex (char '}')
let comma = lex (char ',')
let colon = lex (char ':')
let lbrack = lex (char '[')
let rbrack = lex (char ']')

let string_literal : string parser =
  lex (char '"' *> take_while P.not_quot <* char '"')

(* Parser *)
let json_parser : json_value parser =
  (fun json_value ->
    let json_null : json_value parser =
      (fun _ -> Null) <$> lex (string "null")
    in

    let json_bool : json_value parser =
      (fun x -> Bool (bool_of_string x))
      <$> (lex (string "true") <|> lex (string "false"))
    in

    let json_number : json_value parser =
      (fun x -> Number (int_of_string x)) <$> digits
    in

    let json_string : json_value parser =
      (fun x -> String x) <$> string_literal
    in

    let json_array : json_value parser =
      let elements = sep_by comma json_value in
      (fun x -> Array x) <$> (lbrack *> elements <* rbrack)
    in

    let json_pair : (string * json_value) parser =
      (fun key _ value -> (key, value))
      <$> string_literal <*> colon <*> json_value
    in

    let json_object : json_value parser =
      (fun x -> Object x) <$> lbrace *> sep_by comma json_pair <* rbrace
    in

    json_null <|> json_bool <|> json_number <|> json_string <|> json_array
    <|> json_object)
  |> fix
