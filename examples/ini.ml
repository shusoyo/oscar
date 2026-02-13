open Oscar

type pair = { key : string; value : string }
type section = { name : string; pairs : pair list }
type sections = section list

(* example :
[section 1]
hello = value *)

module P = struct
  let is_token = function '[' | ']' | '=' -> false | _ -> true
  let is_spaces = function ' ' | '\t' -> true | _ -> false
end

let single_line_comment =
  string ";;" *> skip_while (( <> ) '\n') <* optional (char '\n')

let ws = take_while P.is_spaces
let lex (ps : 'a parser) : 'a parser = ps <* ws

let a =
  gen_parser (fun input ->
    (* hello *)
    failwith "hello")

let read_file (path : string) : string =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
