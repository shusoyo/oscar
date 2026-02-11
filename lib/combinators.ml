(* Monadic Parser Combinator in OCaml *)

open Parser

let ( let* ) = Result.bind
let fail e = (fun _ -> Error e) |> gen_parser
let todo = fail { desc = "Todo"; pos = 0 }

(* 2.3 Parser combinators *)
(* Sequence Applicative *)
let pure (x : 'a) : 'a parser = (fun input -> Ok (input, x)) |> gen_parser
let return (x : 'a) : 'a parser = (fun input -> Ok (input, x)) |> gen_parser

let ( <*> ) (pf : ('a -> 'b) parser) (pa : 'a parser) : 'b parser =
  (fun input ->
    let* rest, f = pf.run input in
    let* rest', a = pa.run rest in
    Ok (rest', f a))
  |> gen_parser

let ( *> ) (pa : 'a parser) (pb : 'b parser) : 'b parser =
  (fun input ->
    let* rest, _ = pa.run input in
    pb.run rest)
  |> gen_parser

let ( <* ) (pa : 'a parser) (pb : 'b parser) : 'a parser =
  (fun input ->
    let* rest, token = pa.run input in
    let* rest', _ = pb.run rest in
    Ok (rest', token))
  |> gen_parser

(* Functor *)
let fmap (f : 'a -> 'b) (ps : 'a parser) : 'b parser =
  (fun input ->
    let* rest, token = ps.run input in
    Ok (rest, f token))
  |> gen_parser

let ( <$> ) = fmap

(* Alterantive *)
let empty : 'a parser = fail { desc = "Alternative empty"; pos = 0 }

let ( <|> ) (pa : 'a parser) (pb : 'a parser) : 'a parser =
  (fun input ->
    match pa.run input with
    | Ok _ as v -> v
    | Error { desc = l_desc; pos = l_pos } ->
        pb.run input
        |> Result.map_error (fun { desc = r_desc } ->
            { desc = l_desc ^ " or " ^ r_desc; pos = l_pos }))
  |> gen_parser

let optional (pa : 'a parser) : 'a option parser =
  (fun input ->
    match pa.run input with
    | Ok (rest, token) -> Ok (rest, Some token)
    | Error e -> Ok (input, None))
  |> gen_parser

(* Repeation *)
let rec many (pa : 'a parser) : 'a list parser =
  (fun input ->
    match pa.run input with
    | Ok (rest, token) ->
        let* rest', tokens = (many pa).run rest in
        Ok (rest', token :: tokens)
    | Error err -> Ok (input, []))
  |> gen_parser

let rec many1 = todo
let rec count = todo

let sep_by (sep : 'a parser) (element : 'b parser) : 'b list parser =
  List.cons <$> element <*> many (sep *> element) <|> pure []

let sep_by1 = todo
let end_by = todo

(* finxpoint *)
let notset = { run = (fun input -> failwith "not set") }

let fix f =
  let rec p = ref notset and r = { run = (fun input -> !p.run input) } in
  p := f r;
  r
