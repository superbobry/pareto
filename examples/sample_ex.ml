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


let sample_histogram () =
  let open Distributions.Normal in
  let vs = sample ~size:100 standard in
  let (points, counts) = Sample.histogram ~n_bins:10 vs in begin
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
    estimate_pdf ~kernel:Gaussian ~bandwidth:Silverman ~n_points:10 vs
  in begin
    print_endline "Normal sample (Gaussian) KDE";
    print_float_array pdf;
    print_density (points, pdf);
    print_newline ()
  end

let sample_quantiles () =
  let vs = random_array 10
  and ps = [|0.; 0.25; 0.75; 1.|] in begin
    print_float_array vs;
    print_float_array ps;
    print_float_array (Sample.quantile ~ps vs);
    print_newline ()
  end

let sample_iqr () =
  let vs = random_array 10 in begin
    print_float_array vs;
    printf "  IQR: %9.5f\n" (Sample.iqr vs);
    print_newline ()
  end

let sample_ranks () =
  let vs = random_array 10 in
  let (_t, ranks) = Sample.rank vs in begin
    print_endline "Sample ranks";
    print_float_array vs;
    print_float_array ranks;
    print_newline ()
  end

let sample_correlation () =
  let vs1 = random_array 10
  and vs2 = random_array 10 in begin
    print_float_array vs1;
    print_float_array vs2;
    printf "Pearson product-momentum correlation: %f\n"
      (Sample.Correlation.pearson vs1 vs2);
    printf "Spearman rank-correlation: %f\n"
      (Sample.Correlation.spearman vs1 vs2);
    print_newline ()
  end

let sample_autocorrelation () =
  let vs = random_array 10 in begin
    print_float_array vs;
    print_endline "Pearson product-momentum autocorrelation:";
    print_float_array (Sample.Correlation.Auto.pearson vs);
    print_newline ()
  end

let sample_summary () =
  let vs = random_array 10 in
  let open Sample in
  let s  = Summary.(Array.fold_left add empty vs) in
  begin
    print_float_array vs;
    printf "        kurtosis = %f, skewness = %f\n" (kurtosis vs) (skewness vs);
    printf "Summary kurtosis = %f, skewness = %f\n"
      (Summary.kurtosis s) (Summary.skewness s);
    print_newline ()
  end


let () = begin
  sample_histogram ();
  sample_kde ();
  sample_quantiles ();
  sample_iqr ();
  sample_ranks ();
  sample_correlation ();
  sample_autocorrelation ();
  sample_summary ()
end
