open Printf
open Statistics

open Common

let sample_shuffle () =
  let vs  = Array.init 10 (fun i -> float_of_int i) in
  let svs = Sample.shuffle vs in
  let svs_with_replacement = Sample.sample ~replace:true ~size:5 vs
  and svs_without_replacement = Sample.sample ~size:5 vs in begin
    print_string "Initial sample : ";
    print_array vs;
    print_string "Shuffled sample: ";
    print_array svs;
    print_endline "5-sample *with* replacement from the shuffled array:";
    print_array svs_with_replacement;
    print_endline "5-sample *without* replacement from the shuffled array:";
    print_array svs_without_replacement;
    print_newline ();
  end

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
  sample_shuffle ();
  sample_quantiles ();
  sample_iqr ();
end
