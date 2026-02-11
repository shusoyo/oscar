type input = { text : string; pos : int }
type error = { desc : string; pos : int }
type 'a parser = { run : input -> (input * 'a, error) result }

let make_input (text : string) : input = { text; pos = 0 }

let input_sub (inp : input) (start : int) (len : int) : input =
  { text = String.sub inp.text start len; pos = inp.pos + start }

let gen_parser (run : input -> (input * 'a, error) result) : 'a parser = { run }
