open Printf

let print_array f vs =
  print_char '[';
  Array.iteri (fun i v ->
      f v;
      if i <> Array.length vs - 1
      then print_string ", ") vs;
  print_char ']';
  print_newline ()

let print_float_array = print_array (printf "%.5f")
and print_int_array = print_array (printf "%i")

let random_array ?(a=0.) ?(b=100.) n =
  let vs = Array.make n 0. in
  for i = 0 to n - 1 do
    vs.(i) <- a +. Random.float b
  done; vs
