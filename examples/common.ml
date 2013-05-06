let print_array v =
  let open Printf in
  print_char '[';
  Array.iteri (fun i ->
    if i <> Array.length v - 1
    then printf "%.5f, "
    else printf "%.5f") v;
  print_char ']';
  print_newline ()

let random_array ?(a=0.) ?(b=100.) n =
  let vs = Array.make n 0. in
  for i = 0 to n - 1 do
    vs.(i) <- a +. Random.float b
  done; vs
