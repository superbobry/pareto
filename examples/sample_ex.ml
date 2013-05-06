open Printf
open Statistics

let print_array v =
  print_char '[';
  Array.iteri (fun i ->
    if i <> Array.length v - 1
    then printf "%f, "
    else printf "%f") v;
  print_char ']';
  print_newline ()

let random_array ?(a=0.) ?(b=100.) n =
  let vs = Array.make n 0. in
  for i = 0 to n - 1 do
    vs.(i) <- a +. Random.float b
  done; vs

let sample_quantiles () =
  let vs = random_array 10 in begin
    print_array vs;
    for i = 0 to 4 do
      printf "%4d%%: %9.5f\n"
        (25 * i)
        (Sample.quantile ~p:(0.25 *. float_of_int i) vs)
    done;
    print_newline ()
  end

let sample_iqr () =
  let vs = random_array 10 in begin
    print_array vs;
    printf "  IQR: %9.5f\n" (Sample.iqr vs);
    print_newline ()
  end


let () = begin
  sample_quantiles ();
  sample_iqr ();
end
