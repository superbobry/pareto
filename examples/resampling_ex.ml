open Printf
open Statistics

open Common

let jackknife_mean () =
  let open Distributions.Normal in
  let vs = sample ~size:100 standard in
  let resample = Resampling.jackknife ~estimator:Sample.mean vs in begin
    print_endline "Sample:";
    print_array vs;
    print_endline "Jackknife estimate of sample mean:";
    printf "%.5f\n" (Sample.mean resample);
    print_newline ()
  end

let resample_mean () =
  let open Distributions.Normal in
  let vs = sample ~size:100 standard in
  let resample =
    Resampling.resample ~estimator:Sample.mean ~n:10000 vs
  in begin
    print_endline "Sample:";
    print_array vs;
    print_endline "Bootstrapped estimate of sample mean:";
    printf "%.5f\n" (Sample.mean resample);
    print_newline ()
  end


let () = begin
  jackknife_mean ();
  resample_mean ()
end
