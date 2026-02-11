open Oscar

type pair = { key : string; value : string }
type section = { name : string; pairs : pair list }
type sections = section list

let ini : sections parser = fail { desc = "todo"; pos = 0 }

let read_file (path : string) : string =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
