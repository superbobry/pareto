open Printf
open Pareto

open Common

let t_test_one_sample () =
  let open Distributions.Normal in
  let v = sample ~size:10 standard in
  let (t, pvalue) =
    Tests.T.one_sample v ~mean:0. ~alternative:Tests.TwoSided ()
  in begin
    printf "One-sample T-test for true mean = 0.0\n";
    print_float_array v;
    printf "t = %f, P-value: %f\n" t pvalue;
    print_newline ()
  end

let t_test_two_sample_independent () =
  let open Distributions.Normal in
  let v1 = sample ~size:10 standard in
  let v2 = sample ~size:10 standard in
  let (t, pvalue) = Tests.T.two_sample_independent v1 v2
      ~mean:0.1 ~equal_variance:false ~alternative:Tests.TwoSided ()
  in begin
    printf "Two-sample T-test for mean difference not equal to 0.1\n";
    print_float_array v1;
    print_float_array v2;
    printf "t = %f, P-value: %f\n" t pvalue;
    print_newline ()
  end

let t_test_two_sample_related () =
  let open Distributions.Normal in
  let v1 = sample ~size:10 standard in
  let v2 = Array.map (fun x -> x +. generate standard) v1 in
  let (t, pvalue) = Tests.T.two_sample_related v1 v2
      ~mean:0.1 ~alternative:Tests.TwoSided ()
  in begin
    printf "Paired two-sample T-test for mean difference not equal to 0.1\n";
    print_float_array v1;
    print_float_array v2;
    printf "t = %f, P-value: %f\n" t pvalue;
    print_newline ()
  end

let chisq_test_gof () =
  let open Distributions.Uniform in
  let v = sample ~size:10 (create ~lower:0. ~upper:1.) in
  let (chisq, pvalue) = Tests.ChiSquared.goodness_of_fit v () in
  begin
    print_endline "X^2 test for goodness of fit";
    print_float_array v;
    printf "X^2 = %f, P-value: %f\n" chisq pvalue;
    print_newline ()
  end

let chisq_test_independence () =
  let open Distributions.Uniform in
  let d  = create ~lower:0. ~upper:1. in
  let v1 = sample ~size:10 d in
  let v2 = sample ~size:10 d in
  let (chisq, pvalue) =
    Tests.ChiSquared.independence [|v1; v2|] ~correction:true ()
  in begin
    print_endline "X^2 test for independence with Yates' continuity correction\n";
    print_float_array v1;
    print_float_array v2;
    printf "X^2 = %f, P-value: %f\n" chisq pvalue;
    print_newline ()
  end

let mann_whitney_wilcoxon () =
  let v1 = [|11; 1; -1; 2; 0|] in
  let v2 = [|-5; 9; 5; 8; 4|] in
  let (u, pvalue) = Tests.Wilcoxon.two_sample_independent v1 v2
      ~correction:true ~alternative:Tests.TwoSided ()
  in begin
    printf "Two-sample Mann-Whitney U test\n";
    print_int_array v1;
    print_int_array v2;
    printf "U = %f, P-value: %f\n" u pvalue;
    print_newline ()
  end



let () = begin
  t_test_one_sample ();
  t_test_two_sample_independent ();
  t_test_two_sample_related ();
  chisq_test_gof ();
  chisq_test_independence ();
  mann_whitney_wilcoxon ()
end
