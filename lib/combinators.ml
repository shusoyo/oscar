(* Monadic Parser Combinator in OCaml *)

open Parser

let ( let* ) = Result.bind
let fail e = gen_parser (fun _ -> Error e)
let todo = fail { desc = "Todo"; pos = 0 }

(* 2.3 Parser combinators *)
(* Sequence Applicative *)
let return (x : 'a) : 'a parser = gen_parser (fun input -> Ok (input, x))

let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  gen_parser (fun input ->
    let* rest, token = p.run input in
    (f token).run rest)

let ( >>= ) = bind

let ( <*> ) (pf : ('a -> 'b) parser) (pa : 'a parser) : 'b parser =
  gen_parser (fun input ->
    let* rest, f = pf.run input in
    let* rest', a = pa.run rest in
    Ok (rest', f a))

let ( *> ) (pa : 'a parser) (pb : 'b parser) : 'b parser =
  gen_parser (fun input ->
    let* rest, _ = pa.run input in
    pb.run rest)

let ( <* ) (pa : 'a parser) (pb : 'b parser) : 'a parser =
  gen_parser (fun input ->
    let* rest, token = pa.run input in
    let* rest', _ = pb.run rest in
    Ok (rest', token))

(* Functor *)
let fmap (f : 'a -> 'b) (ps : 'a parser) : 'b parser =
  gen_parser (fun input ->
    let* rest, token = ps.run input in
    Ok (rest, f token))

let ( <$> ) = fmap
let void (a : 'a parser) : unit parser = (fun _ -> ()) <$> a

(* Alterantive *)
let empty : 'a parser = fail { desc = "Alternative empty"; pos = 0 }

let ( <|> ) (pa : 'a parser) (pb : 'a parser) : 'a parser =
  gen_parser (fun input ->
    match pa.run input with
    | Ok _ as v -> v
    | Error { desc = l_desc; pos = l_pos } ->
        pb.run input
        |> Result.map_error (fun { desc = r_desc } ->
          { desc = l_desc ^ " or " ^ r_desc; pos = l_pos }))

let optional (pa : 'a parser) : 'a option parser =
  gen_parser (fun input ->
    match pa.run input with
    | Ok (rest, token) -> Ok (rest, Some token)
    | Error e -> Ok (input, None))

let ( <?> ) (pa : 'a parser) (msg : string) : 'a parser =
  gen_parser (fun input ->
    let rewrite_error { desc; pos } = { desc = "Expected: " ^ msg; pos } in
    pa.run input |> Result.map_error rewrite_error)

(* Repeation *)
let rec many (pa : 'a parser) : 'a list parser =
  gen_parser (fun input ->
    match pa.run input with
    | Ok (rest, token) ->
        let* rest', tokens = (many pa).run rest in
        Ok (rest', token :: tokens)
    | Error err -> Ok (input, []))

let rec many1 (pa : 'a parser) : 'a list parser = List.cons <$> pa <*> many pa
let count = todo

let sep_by (sep : 'a parser) (element : 'b parser) : 'b list parser =
  List.cons <$> element <*> many (sep *> element) <|> return []

let sep_by1 (sep : 'a parser) (element : 'b parser) : 'b list parser =
  List.cons <$> element <*> sep_by sep element

let end_by = todo

(* finxpoint *)
let notset = { run = (fun input -> failwith "not set") }

let fix f =
  let rec p = ref notset and r = { run = (fun input -> !p.run input) } in
  p := f r;
  r
