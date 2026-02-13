include Parser
include Combinators

let parse (p : 'a parser) (text : string) : ('a, error) result =
  match p.run (make_input text) with Ok (_, x) -> Ok x | Error e -> Error e

(* Char Primitive *)
let any_char : char parser =
  (fun input ->
    let input_lenght = String.length input.text in
    if input_lenght = 0 then Error { desc = "empty input"; pos = input.pos }
    else Ok (input_sub input 1 (input_lenght - 1), input.text.[0]))
  |> gen_parser

let satisfy (p : char -> bool) : char parser =
  (fun input ->
    let* rest, token = any_char.run input in
    if p token then Ok (rest, token)
    else
      Error { desc = "Expected char: " ^ String.make 1 token; pos = rest.pos })
  |> gen_parser

let char (ch : char) : char parser = satisfy (( = ) ch)

let advance (n : int) =
  (fun input ->
    let input_lenght = String.length input.text in
    if input_lenght < n then
      Error { desc = "[advance]: input lenght not enough"; pos = input.pos }
    else Ok (input_sub input n (input_lenght - n), ()))
  |> gen_parser

(* String match *)
let string (s : string) : string parser =
  (fun input ->
    let s_len = String.length s in
    let input_len = String.length input.text in
    try
      if String.sub input.text 0 s_len = s then
        let rest = input_sub input s_len (input_len - s_len) in
        Ok (rest, s)
      else Error { desc = "Expected string: '" ^ s ^ "'"; pos = input.pos }
    with Invalid_argument _ ->
      Error { desc = "Expected string: '" ^ s ^ "'"; pos = input.pos })
  |> gen_parser

let take_while (p : char -> bool) : string parser =
  (fun input ->
    let input_len = String.length input.text in
    let i = ref 0 in
    while !i < input_len && p input.text.[!i] do
      incr i
    done;
    Ok (input_sub input !i (input_len - !i), String.sub input.text 0 !i))
  |> gen_parser

let skip_while (p : char -> bool) : unit parser =
  (fun input ->
    let input_len = String.length input.text in
    let i = ref 0 in
    while !i < input_len && p input.text.[!i] do
      incr i
    done;
    Ok (input_sub input !i (input_len - !i), ()))
  |> gen_parser

let not_null p =
  (fun input ->
    let* rest, token = p.run input in
    if token = "" then
      Error { desc = "excepted not null, but take null"; pos = rest.pos }
    else Ok (rest, token))
  |> gen_parser

let take_while1 p = not_null (take_while p)
let skip_while1 p = satisfy p *> skip_while p

let end_of_file : unit parser =
  (fun input ->
    if String.length input.text = 0 then Ok (input, ())
    else
      Error
        {
          desc = "Expected end of file, but has: " ^ input.text;
          pos = input.pos;
        })
  |> gen_parser

let skip_many p = fix (fun m -> p *> m <|> return ())

let peek_char : char option parser =
  (fun input ->
    if String.length input.text = 0 then Ok (input, None)
    else Ok (input, Some input.text.[0]))
  |> gen_parser
