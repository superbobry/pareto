open OUnit

open Pareto.Tests

open Common


let assert_equal_test_result ~msg tr1 tr2 =
  assert_almost_equal ~msg tr1.test_statistic tr2.test_statistic;
  assert_almost_equal ~msg tr1.test_pvalue tr2.test_pvalue

let assert_equal_test_results ?(msg="")
    (f : ?alternative:test_alternative -> unit -> test_result)
    expected =
  List.iter2 (fun tr1 alternative ->
      let tr2       = f ~alternative () in
      let direction = match alternative with
        | TwoSided -> "two-sided"
        | Less     -> "less"
        | Greater  -> "greater"
      in assert_equal_test_result
        ~msg:(if msg = ""
              then direction
              else Printf.sprintf "%s, %s" msg direction)
        tr1 tr2
    ) expected [TwoSided; Less; Greater]


let t_test_one_sample () =
  let vs = [|0.88456; 0.43590; 0.95778; -1.05039; -0.38589;
             -0.06342; -0.18712; 1.58856; 0.86964; 1.22192|]
  in begin
    assert_equal_test_results (T.one_sample vs ~mean:0.) [
      { test_statistic = 1.6368118; test_pvalue = 0.1360967 };
      { test_statistic = 1.6368118; test_pvalue = 0.931951 };
      { test_statistic = 1.6368118; test_pvalue = 0.0680483 }
    ];

    (* Zero division issues. *)
    let tr =
      T.one_sample [|0.; 0.; 0.; 0.|] ~mean:0. ~alternative:TwoSided ()
    in begin
      assert_bool "t-statistic or t-test P-value is not nan"
        (tr.test_statistic <> tr.test_statistic &&
         tr.test_pvalue <> tr.test_pvalue)
    end
  end

and t_test_two_sample_independent () =
  let v1 = [|-0.86349; 0.36688; -0.48266; 0.53237; -0.87635;
             -1.28357; -1.46325; 0.21937; -0.38159; -0.22752|]
  and v2 = [|-0.20951; 1.27388; 0.27331; 1.85599; -1.09702;
             -0.20033; -0.45065; 0.06710; -0.18932; 1.60007|]
  in begin
    assert_equal_test_results ~msg:"unequal variance"
      (T.two_sample_independent v1 v2 ~mean:0.42 ~equal_variance:false)
      [{ test_statistic = -3.097208; test_pvalue = 0.00683211 };
       { test_statistic = -3.097208; test_pvalue = 0.003416056 };
       { test_statistic = -3.097208; test_pvalue = 0.996583 }];
    assert_equal_test_results ~msg:"equal variance"
      (T.two_sample_independent v1 v2 ~mean:0.24 ~equal_variance:true)
      [{ test_statistic = -2.615915; test_pvalue = 0.01750332 };
       { test_statistic = -2.615915; test_pvalue = 0.00875166 };
       { test_statistic = -2.615915; test_pvalue = 0.991248 }]
  end

and t_test_two_sample_paired () =
  let v1 = [|-0.86349; 0.36688; -0.48266; 0.53237; -0.87635;
             -1.28357; -1.46325; 0.21937; -0.38159; -0.22752|]
  and v2 = [|-0.20951; 1.27388; 0.27331; 1.85599; -1.09702;
             -0.20033; -0.45065; 0.06710; -0.18932; 1.60007|]
  in begin
    assert_equal_test_results
      (T.two_sample_paired v1 v2 ~mean:0.)
      [{ test_statistic = -3.607401; test_pvalue = 0.0056823 };
       { test_statistic = -3.607401; test_pvalue = 0.00284115 };
       { test_statistic = -3.607401; test_pvalue = 0.997158 }];
  end

and chisq_test_gof () =
  let observed = [|42.; 24.; 10.; 10.|]
  and expected = [|21.; 22.; 17.; 26.|]
  in begin
    assert_equal_test_result ~msg:"with uniform probabilities"
      (ChiSquared.goodness_of_fit observed ())
      { test_statistic = 32.139534; test_pvalue = 4.8908077607843233e-07 };

    (* Note(superbobry): R doesn't have a version of the test with
       frequences, thus we use 'scipy.stats.chisquare' as a reference. *)
    assert_equal_test_result ~msg:"with given probabilities"
      (ChiSquared.goodness_of_fit observed ~expected ())
      { test_statistic = 33.910324; test_pvalue = 2.06945476533e-07 }
  end

and chisq_test_independence () =
  let observed = [|
    [|4.; 3.; 5.; 3.; 5.; 3.; 2.; 5.; 4.; 4.; 4.; 3.|];
    [|2.; 2.; 1.; 2.; 3.; 1.; 2.; 3.; 2.; 1.; 1.; 3.|];
    [|2.; 4.; 3.; 3.; 4.; 3.; 3.; 4.; 4.; 1.; 2.; 1.|];
    [|3.; 5.; 4.; 3.; 4.; 4.; 3.; 3.; 3.; 4.; 4.; 4.|]
  |] in begin
    assert_equal_test_result ~msg:"with continuity correction"
      (ChiSquared.independence observed ~correction:true ())
      { test_statistic = 9.153073; test_pvalue = 0.999987 };
  end

and ks_test_gof () =
  let vs = [|-1.52455; 0.79745; 0.76526; -2.32246; 0.15411;
             -1.36430; 0.62041; 1.17614; 1.09825; -0.17400|]
  in begin
    let open Pareto.Distributions.Normal in
    assert_equal_test_results ~msg:"standard normal"
      (KolmogorovSmirnov.goodness_of_fit vs
         ~cumulative_probability:(fun x -> cumulative_probability standard ~x))
      [{ test_statistic = 0.232506; test_pvalue = 0.575175 };
       { test_statistic = 0.232506; test_pvalue = 0.293906 };
       { test_statistic = 0.2137634; test_pvalue = 0.3516124 }]
  end

and ks_test_two_sample () =
  let v1 = [|0.60074; 1.93516; 0.62419; -0.40251; -0.14719;
             -0.05324; -0.95052; 1.84247; -0.58041; -0.75201|]
  and v2 = [|-0.33866; -0.59032; -0.12525; -0.81013; -0.60733;
             0.18550; 1.01396; 0.17067; -0.74872; 1.03694|]
  in begin
    assert_equal_test_results
      (KolmogorovSmirnov.two_sample v1 v2)
      [{ test_statistic = 0.2; test_pvalue = 0.994457 };
       { test_statistic = 0.2; test_pvalue = 0.670320 };
       { test_statistic = 0.1; test_pvalue = 0.904837 }]
  end

and mann_whitney_test_two_sample () =
  (* Source: 'scipy/stats/tests/test_stats.py'. *)
  let v1 = [|1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 2.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 2.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 2.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 2.; 1.; 1.; 1.; 1.; 2.; 1.; 1.; 2.; 1.; 1.;
             2.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 2.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             2.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 2.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 3.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.|]
  and v2 = [|1.; 1.; 1.; 1.; 1.; 1.; 1.; 2.; 1.; 2.; 1.; 1.; 1.;
             1.; 2.; 1.; 1.; 1.; 2.; 1.; 1.; 1.; 1.; 1.; 2.; 1.; 1.; 3.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 2.; 1.; 2.; 1.; 1.; 1.; 1.;
             1.; 1.; 2.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 2.; 1.; 1.; 1.; 1.; 1.; 2.; 2.; 1.; 1.; 2.; 1.; 1.; 2.;
             1.; 2.; 1.; 1.; 1.; 1.; 2.; 2.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 2.; 1.; 1.; 1.; 1.; 1.; 2.; 2.; 2.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 2.; 1.; 1.; 2.; 1.; 1.; 1.; 1.; 2.; 1.; 1.; 1.; 1.; 1.; 1.;
             1.; 1.; 1.; 1.; 1.; 1.; 2.; 1.; 1.; 1.; 2.; 1.; 1.; 1.; 1.; 1.;
             1.|]
  in begin
    assert_equal_test_results ~msg:"with continuity correction and ties"
      (MannWhitneyU.two_sample_independent v1 v2 ~correction:true)
      [{ test_statistic = 16980.5; test_pvalue = 0.0000564287 };
       { test_statistic = 16980.5; test_pvalue = 0.0000282143 };
       { test_statistic = 16980.5; test_pvalue = 0.999972 }];

    assert_equal_test_results ~msg:"with continuity correction and no ties"
      (MannWhitneyU.two_sample_independent
         [|11; 4; 2; 5|] [|12; 3; 9; 7|] ~correction:true)
      [{ test_statistic = 5.; test_pvalue = 0.4857143 };
       { test_statistic = 5.; test_pvalue = 0.2428571 };
       { test_statistic = 5.; test_pvalue = 0.8285714 }]
  end

and wilcoxon_signed_rank_test_one_sample () =
  begin
    (* Source: Sheskin, Example 6.1.

       Note(superbobry): we don't cross-check statistic value with R
       here, because R computes W = W-, while our implementation
       follows Sheskin and defines W = min(W-, W+). I guess that's
       why one-tailed P-values are flipped. *)
    assert_equal_test_results ~msg:"with continuity correction and ties"
      (WilcoxonT.one_sample [|9.; 10.; 8.; 4.; 8.; 3.; 0.; 10.; 15.; 9.|]
         ~shift:5. ~correction:true)
      [{ test_statistic = 11.; test_pvalue = 0.101575647 };
       { test_statistic = 11.; test_pvalue = 0.0507878 };
       { test_statistic = 11.; test_pvalue = 0.959035 }];

    assert_equal_test_results ~msg:"with continuity correction and no ties"
      (WilcoxonT.one_sample [|9.; 10.; 8.; 4.; 3.; 15.|]
         ~shift:5. ~correction:true)
      [{ test_statistic = 3.; test_pvalue = 0.15625 };
       { test_statistic = 3.; test_pvalue = 0.078125 };
       { test_statistic = 3.; test_pvalue = 0.953125 }]
  end

and wilcoxon_signed_rank_test_two_samples () =
  let v1 = [|78.; 24.; 64.; 45.; 64.; 52.; 30.; 50.; 64.; 50.;
             78.; 22.; 84.; 40.; 90.; 72.|]
  and v2 = [|78.; 24.; 62.; 48.; 68.; 56.; 25.; 44.; 56.; 40.;
             68.; 36.; 68.; 20.; 58.; 32.|]
  in begin
    assert_equal_test_results ~msg:"with continuity correction and ties"
      (WilcoxonT.two_sample_paired v1 v2 ~correction:true)
      [{ test_statistic = 19.; test_pvalue = 0.0382053143 };
       { test_statistic = 19.; test_pvalue = 0.01910266 };
       { test_statistic = 19.; test_pvalue = 0.983638 }]
  end

and sign_test_one_sample () =
  (* Source: Sheskin, Example 9.7. *)
  let vs =
    [|230.; 167.; 250.; 345.; 442.; 190.; 200.; 248.; 289.; 262.; 301.|]
  in begin
    assert_equal_test_results
      (Sign.one_sample vs ~shift:200.)
      [{ test_statistic = 8.; test_pvalue = 0.109375 };
       { test_statistic = 8.; test_pvalue = 0.9892578 };
       { test_statistic = 8.; test_pvalue = 0.0546875 }]
  end

and sign_test_two_sample () =
  (* Source: Sheskin, Example 19.1. *)
  let v1 = [|9.; 2.; 1.; 4.; 6.; 4.; 7.; 8.; 5.; 1.|]
  and v2 = [|8.; 2.; 3.; 2.; 3.; 0.; 4.; 5.; 4.; 0.|]
  in begin
    assert_equal_test_results
      (Sign.two_sample_paired v1 v2)
      [{ test_statistic = 8.; test_pvalue = 0.0390625 };
       { test_statistic = 8.; test_pvalue = 0.9980468 };
       { test_statistic = 8.; test_pvalue = 0.01953125 }]
  end


let test_hb_adjust () =
  let pvalues =
    [|0.000962882346117542; 0.00189844480724466; 0.0183097438104205;
      0.0315318359604176; 0.0481693657349631; 0.105687877464594;
      0.543211136961355; 0.565056666152251; 0.603476808731503;
      0.955690764788587|]
  and adjusted_pvalues =
    [|0.00962882346117542; 0.0170860032652019; 0.146477950483364;
      0.220722851722923; 0.289016194409779; 0.528439387322972;
      1.; 1.; 1.; 1.|]
  in begin
    assert_equal
      ~cmp:(cmp_array ~cmp:(cmp_float ~epsilon:1e-6))
      ~printer:(printer_array ~printer:(Printf.sprintf "%.6f"))
      adjusted_pvalues
      Multiple.(adjust pvalues HolmBonferroni)
  end

and test_bh_adjust () =
  let pvalues =
    [|0.000962882346117542; 0.00189844480724466; 0.0183097438104205;
      0.0315318359604176; 0.0481693657349631; 0.105687877464594;
      0.543211136961355; 0.565056666152251; 0.603476808731503;
      0.955690764788587|]
  and adjusted_pvalues =
    [|0.00949222403622329; 0.00949222403622329; 0.0610324793680683;
      0.078829589901044; 0.0963387314699263; 0.176146462440991;
      0.670529787479448; 0.670529787479448; 0.670529787479448;
      0.955690764788587|]
  in begin
    assert_equal
      ~cmp:(cmp_array ~cmp:(cmp_float ~epsilon:1e-6))
      ~printer:(printer_array ~printer:(Printf.sprintf "%.6f"))
      adjusted_pvalues
      Multiple.(adjust pvalues BenjaminiHochberg)
  end


let test = "Tests" >::: [
    "one-sample t-test" >:: t_test_one_sample;
    "two-sample t-test for independent samples" >::
      t_test_two_sample_independent;
    "two-sample t-test for paired samples" >:: t_test_two_sample_paired;
    "X^2 test for goodness of fit" >:: chisq_test_gof;
    "X^2 test for independence" >:: chisq_test_independence;
    "one-sample Kolmogorov-Smirnov test for goodness of fit" >:: ks_test_gof;
    "two-sample Kolmogorov-Smirnov test" >:: ks_test_two_sample;
    "two-sample Mann-Whitney test for independent samples" >::
      mann_whitney_test_two_sample;
    "one-sample Wilcoxon signed-rank test" >::
      wilcoxon_signed_rank_test_one_sample;
    "two-sample Wilcoxon signed-rank test for paired samples" >::
      wilcoxon_signed_rank_test_two_samples;
    "one-sample sign test" >:: sign_test_one_sample;
    "two-sample sign test" >:: sign_test_two_sample;

    "Holm-Bonferroni P-value adjustment" >:: test_hb_adjust;
    "Benjamini-Hochberg P-value adjustment" >:: test_bh_adjust
  ]
