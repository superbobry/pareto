open Printf

open Pareto

open Common

let bootstrap_mean () =
  let open Distributions.Normal in
  let vs = sample ~size:100 standard in
  let open Resampling.Bootstrap in
  let { point; upper_bound; lower_bound; confidence_level } =
    bca ~estimator:Sample.mean ~n:10000 vs
  in begin
    print_endline "Sample:";
    print_float_array vs;
    print_endline "BCA bootstrapped estimate of sample mean:";
    printf "%.5f    %i%% CI  %.5f %.5f\n"
      point (int_of_float (confidence_level *. 100.)) lower_bound upper_bound;
    print_newline ()
  end


let () = begin
  bootstrap_mean ()
end
