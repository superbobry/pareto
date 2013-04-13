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

let t_test_one_sample () =
  let open Distributions.Gaussian in
  let v = sample ~size:10 standard in
  let (t, pvalue) =
    Tests.T.one_sample v ~mean:0. ~alternative:Tests.TwoSided ()
  in begin
    printf "One-sample T-test for true mean = 0.0\n";
    print_array v;
    printf "t = %f, P-value: %f\n" t pvalue;
    print_newline ()
  end

let t_test_two_sample_independent () =
  let open Distributions.Gaussian in
  let v1 = sample ~size:10 standard in
  let v2 = sample ~size:10 standard in
  let (t, pvalue) = Tests.T.two_sample_independent v1 v2
      ~mean:0.1 ~equal_variance:false ~alternative:Tests.TwoSided ()
  in begin
    printf "Two-sample T-test for mean difference not equal to 0.1\n";
    print_array v1;
    print_array v2;
    printf "t = %f, P-value: %f\n" t pvalue;
    print_newline ()
  end

let t_test_two_sample_related () =
  let open Distributions.Gaussian in
  let v1 = sample ~size:10 standard in
  let v2 = Array.map (fun x -> x +. generate standard) v1 in
  let (t, pvalue) = Tests.T.two_sample_related v1 v2
      ~mean:0.1 ~alternative:Tests.TwoSided ()
  in begin
    printf "Paired two-sample T-test for mean difference not equal to 0.1\n";
    print_array v1;
    print_array v2;
    printf "t = %f, P-value: %f\n" t pvalue;
    print_newline ()
  end


let () = begin
  t_test_one_sample ();
  t_test_two_sample_independent ();
  t_test_two_sample_related ()
end
