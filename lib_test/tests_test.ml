open OUnit

open Pareto.Tests


let assert_almost_equal ?(epsilon=1e-4) =
  assert_equal ~cmp:(cmp_float ~epsilon) ~printer:string_of_float

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
      { test_statistic = 1.636803; test_pvalue = 0.136096 };
      { test_statistic = 1.636803; test_pvalue = 0.931951 };
      { test_statistic = 1.636803; test_pvalue = 0.068048 }
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
      [{ test_statistic = -3.0972; test_pvalue = 0.006832 };
       { test_statistic = -3.0972; test_pvalue = 0.003416 };
       { test_statistic = -3.0972; test_pvalue = 0.996583 }];
    assert_equal_test_results ~msg:"equal variance"
      (T.two_sample_independent v1 v2 ~mean:0.24 ~equal_variance:true)
      [{ test_statistic = -2.6159; test_pvalue = 0.017503 };
       { test_statistic = -2.6159; test_pvalue = 0.008751 };
       { test_statistic = -2.6159; test_pvalue = 0.991248 }]
  end

and t_test_two_sample_paired () =
  let v1 = [|-0.86349; 0.36688; -0.48266; 0.53237; -0.87635;
             -1.28357; -1.46325; 0.21937; -0.38159; -0.22752|]
  and v2 = [|-0.20951; 1.27388; 0.27331; 1.85599; -1.09702;
             -0.20033; -0.45065; 0.06710; -0.18932; 1.60007|]
  in begin
    assert_equal_test_results
      (T.two_sample_paired v1 v2 ~mean:0.)
      [{ test_statistic = -3.607401; test_pvalue = 0.005682 };
       { test_statistic = -3.607401; test_pvalue = 0.002841 };
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
      { test_statistic = 9.153073; test_pvalue = 1. };
  end


let test = "Tests" >::: [
    "one-sample t-test" >:: t_test_one_sample;
    "two-sample t-test for independent samples" >:: t_test_two_sample_independent;
    "two-sample t-test for paired samples" >:: t_test_two_sample_paired;
    "X^2 test for goodness of fit" >:: chisq_test_gof;
    "X^2 test for independence" >:: chisq_test_independence;
  ]
