open Printf
open Pareto

open Common

let print_histogram (points, counts) =
  Array.iteri (fun i count ->
      printf "%9.5f " points.(i);
      for _i = 0 to int_of_float count do
        print_char '*';
      done;

      print_newline ();
    ) counts

and print_density (points, pdf) =
  Array.iteri (fun i d ->
      let count = int_of_float (d *. 20.) in
      printf "%9.5f " points.(i);
      for i = 0 to count do
        print_char (if i = count then '.' else ' ');
      done;

      print_newline ();
    ) pdf

let sample_shuffle () =
  let vs  = Array.init 10 (fun i -> float_of_int i) in
  let svs = Sample.shuffle vs in
  let svs_with_replacement = Sample.sample ~replace:true ~size:5 vs
  and svs_without_replacement = Sample.sample ~size:5 vs in begin
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

let sample_histogram () =
  let open Distributions.Normal in
  let vs = sample ~size:100 standard in
  let (points, counts) = Sample.histogram ~bins:10 vs in begin
    print_endline "Normal sample histogram";
    print_float_array counts;
    print_histogram (points, counts);
    print_newline ()
  end

let sample_kde () =
  let open Distributions.Normal in
  let vs = sample ~size:100 standard in
  let (points, pdf) =
    let open Sample.KDE in
    estimate_pdf ~kernel:Gaussian ~bandwidth:Silverman ~points:10 vs
  in begin
    print_endline "Normal sample (Gaussian) KDE";
    print_float_array pdf;
    print_density (points, pdf);
    print_newline ()
  end

let sample_quantiles () =
  let vs = random_array 10 in begin
    print_float_array vs;
    for i = 0 to 4 do
      printf "%4d%%: %9.5f\n"
        (25 * i)
        (Sample.quantile ~p:(0.25 *. float_of_int i) vs)
    done;
    print_newline ()
  end

let sample_iqr () =
  let vs = random_array 10 in begin
    print_float_array vs;
    printf "  IQR: %9.5f\n" (Sample.iqr vs);
    print_newline ()
  end

let sample_ranks () =
  let vs = random_array 10 in begin
    print_float_array vs;
    print_int_array (Sample.rank vs)
  end

let () = begin
  sample_shuffle ();
  sample_histogram ();
  sample_kde ();
  sample_quantiles ();
  sample_iqr ();
  sample_ranks ()
end
