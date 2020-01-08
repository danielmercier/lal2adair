type t = int

let of_int i = i

let of_string s = int_of_string s

let to_int i = i

let pp = Format.pp_print_int
