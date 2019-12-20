open! Pareto

open Common


let sample_shuffle () =
  let vs  = Array.init 10 (fun i -> float_of_int i) in
  let svs = Base.shuffle vs in
  let svs_with_replacement = Base.sample ~replace:true ~size:5 vs
  and svs_without_replacement = Base.sample ~size:5 vs in begin
    print_string "Initial sample : ";
    print_float_array vs;
    print_string "Shuffled sample: ";
    print_float_array svs;
    print_endline "5-sample *with* replacement from the shuffled array:";
    print_float_array svs_with_replacement;
    print_endline "5-sample *without* replacement from the shuffled array:";
    print_float_array svs_without_replacement;
    print_newline ()
  end


let () = begin
  sample_shuffle ()
end
