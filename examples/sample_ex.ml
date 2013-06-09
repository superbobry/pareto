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
  let vs = random_array 10 in
  let (_t, ranks) = Sample.rank vs in begin
    print_endline "Sample ranks";
    print_float_array vs;
    print_float_array ranks;
    print_newline ()
  end


let sample_autocovariance () =
  let vs = random_array 10 in begin
    print_endline "Autocovariance / autocorrelation";
    print_float_array vs;
    print_float_array (Sample.autocovariance vs);
    print_float_array (Sample.autocorrelation vs);
    print_newline ()
  end


let () = begin
  sample_histogram ();
  sample_kde ();
  sample_quantiles ();
  sample_iqr ();
  sample_ranks ();
  sample_autocovariance ()
end
