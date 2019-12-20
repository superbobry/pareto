open Printf

open! Pareto
open Pareto.Tests

open Common

let t_test_one_sample () =
  let open Distributions.Normal in
  let v = sample ~size:10 standard in
  let { test_statistic = t; test_pvalue } =
    T.one_sample v ~mean:0. ~alternative:TwoSided ()
  in begin
    printf "One-sample T-test for true mean = 0.0\n";
    print_float_array v;
    printf "t = %f, P-value: %f\n" t test_pvalue;
    print_newline ()
  end

let t_test_two_sample_independent () =
  let open Distributions.Normal in
  let v1 = sample ~size:10 standard in
  let v2 = sample ~size:10 standard in
  let { test_statistic = t; test_pvalue } =
    T.two_sample_independent v1 v2
      ~mean:0.1 ~equal_variance:false ~alternative:TwoSided ()
  in begin
    printf "Two-sample T-test for mean difference not equal to 0.1\n";
    print_float_array v1;
    print_float_array v2;
    printf "t = %f, P-value: %f\n" t test_pvalue;
    print_newline ()
  end

let t_test_two_sample_paired () =
  let open Distributions.Normal in
  let v1 = sample ~size:10 standard in
  let v2 = Array.mapi (fun i x -> x +. v1.(i)) (sample ~size:10 standard) in
  let { test_statistic = t; test_pvalue } = T.two_sample_paired v1 v2
      ~mean:0.1 ~alternative:TwoSided ()
  in begin
    printf "Paired two-sample T-test for mean difference not equal to 0.1\n";
    print_float_array v1;
    print_float_array v2;
    printf "t = %f, P-value: %f\n" t test_pvalue;
    print_newline ()
  end

let chisq_test_gof () =
  let open Distributions.Uniform in
  let v = sample ~size:10 (create ~lower:0. ~upper:1.) in
  let { test_statistic = chisq; test_pvalue } =
    ChiSquared.goodness_of_fit v ()
  in begin
    print_endline "X^2 test for goodness of fit";
    print_float_array v;
    printf "X^2 = %f, P-value: %f\n" chisq test_pvalue;
    print_newline ()
  end

let chisq_test_independence () =
  let open Distributions.Uniform in
  let d  = create ~lower:0. ~upper:1. in
  let v1 = sample ~size:10 d in
  let v2 = sample ~size:10 d in
  let { test_statistic = chisq; test_pvalue } =
    ChiSquared.independence [|v1; v2|] ~correction:true ()
  in begin
    print_endline "X^2 test for independence with Yates' continuity correction\n";
    print_float_array v1;
    print_float_array v2;
    printf "X^2 = %f, P-value: %f\n" chisq test_pvalue;
    print_newline ()
  end

let mann_whitney_wilcoxon () =
  let v1 = [|11; 1; -1; 2; 0|] in
  let v2 = [|-5; 9; 5; 8; 4|] in
  let { test_statistic = u; test_pvalue } =
    MannWhitneyU.two_sample_independent v1 v2
      ~correction:true ~alternative:TwoSided ()
  in begin
    printf "Two-sample Mann-Whitney U test\n";
    print_int_array v1;
    print_int_array v2;
    printf "U = %f, P-value: %f\n" u test_pvalue;
    print_newline ()
  end

let wilcoxon_signed_rank_one_sample () =
  let vs = [|11.; 1.; -1.; 2.; 0.|] in
  let { test_statistic = w; test_pvalue } =
    WilcoxonT.one_sample vs
      ~shift:1. ~correction:true ~alternative:Greater ()
  in begin
    printf "Wilcoxon signed rank test with continuity correction\n";
    print_float_array vs;
    printf "W = %f, P-value: %f\n" w test_pvalue;
    print_newline ()
  end

let wilcoxon_signed_rank_paired () =
  let v1 = [|11.; 1.; -1.; 2.; 0.|] in
  let v2 = [|-5.; 9.; 5.; 8.; 4.|] in
  let { test_statistic = w; test_pvalue } =
    WilcoxonT.two_sample_paired v1 v2
      ~correction:true ~alternative:Less ()
  in begin
    print_endline ("Two-sample paired Wilcoxon signed rank test with " ^
                     "continuity correction");
    print_float_array v1;
    print_float_array v2;
    printf "W = %f, P-value: %f\n" w test_pvalue;
    print_newline ()
  end

let sign_one_sample () =
  let vs = [|11.; 1.; -1.; 2.; 0.|] in
  let { test_statistic = pi_plus; test_pvalue } =
    Sign.one_sample vs ~shift:1. ~alternative:TwoSided ()
  in begin
    printf "One-sample Sign test\n";
    print_float_array vs;
    printf "π+ = %f, P-value: %f\n" pi_plus test_pvalue;
    print_newline ()
  end

let sign_paired () =
  let v1 = [|11.; 1.; -1.; 2.; 0.|] in
  let v2 = [|-5.; 9.; 5.; 8.; 4.|] in
  let { test_statistic = pi_plus; test_pvalue } =
    Sign.two_sample_paired v1 v2 ~alternative:TwoSided ()
  in begin
    printf "Two-sample Sign test\n";
    print_float_array v1;
    print_float_array v2;
    printf "π+ = %f, P-value: %f\n" pi_plus test_pvalue;
    print_newline ()
  end

let ks_gof () =
  let open Distributions.Normal in
  let v = sample ~size:10 standard in
  let { test_statistic = d; test_pvalue } =
    KolmogorovSmirnov.goodness_of_fit v
      ~cumulative_probability:(fun x -> cumulative_probability standard ~x)
      ~alternative:TwoSided ()
  in begin
    print_endline "One-sample Kolmogorov-Smirnov test for goodness of fit";
    print_float_array v;
    printf "D = %f, P-value: %f\n" d test_pvalue;
    print_newline ()
  end

let ks_two_sample () =
  let open Distributions.Normal in
  let v1 = sample ~size:10 standard in
  let v2 = sample ~size:10 standard in
  let { test_statistic = d; test_pvalue } =
    KolmogorovSmirnov.two_sample v1 v2 ~alternative:TwoSided ()
  in begin
    print_endline "Two-sample Kolmogorov-Smirnov test";
    print_float_array v1;
    print_float_array v2;
    printf "D = %f, P-value: %f\n" d test_pvalue;
    print_newline ()
  end


let adjust_bh () =
  let open Distributions.Beta in
  let pvalues = sample ~size:10 (create ~alpha:0.5 ~beta:0.5) in
  let adjusted_pvalues = Multiple.(adjust pvalues BenjaminiHochberg) in
  begin
    printf "Benjamini-Hochberg P-value adjustment\n";
    print_float_array pvalues;
    print_float_array adjusted_pvalues;
    print_newline ()
  end

let adjust_hb () =
  let open Distributions.Beta in
  let pvalues = sample ~size:10 (create ~alpha:0.5 ~beta:0.5) in
  let adjusted_pvalues = Multiple.(adjust pvalues HolmBonferroni) in
  begin
    printf "Holm-Bonferroni P-value adjustment\n";
    print_float_array pvalues;
    print_float_array adjusted_pvalues;
    print_newline ()
  end


let () = begin
  t_test_one_sample ();
  t_test_two_sample_independent ();
  t_test_two_sample_paired ();
  chisq_test_gof ();
  chisq_test_independence ();
  mann_whitney_wilcoxon ();
  wilcoxon_signed_rank_one_sample ();
  wilcoxon_signed_rank_paired ();
  sign_one_sample ();
  sign_paired ();
  ks_gof ();
  ks_two_sample ();

  adjust_bh ();
  adjust_hb ()
end
