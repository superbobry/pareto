open OUnit

open Pareto.Tests


let assert_almost_equal ?(epsilon=1e-6) =
  assert_equal ~cmp:(cmp_float ~epsilon) ~printer:(Printf.sprintf "%.10f")

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
       { test_statistic = 0.2137634; test_pvalue = 0.3516124 }];
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
       { test_statistic = 0.1; test_pvalue = 0.904837 }];
  end


let test = "Tests" >::: [
    "one-sample t-test" >:: t_test_one_sample;
    "two-sample t-test for independent samples" >:: t_test_two_sample_independent;
    "two-sample t-test for paired samples" >:: t_test_two_sample_paired;
    "X^2 test for goodness of fit" >:: chisq_test_gof;
    "X^2 test for independence" >:: chisq_test_independence;
    "one-sample Kolmogorov-Smirnov test for goodness of fit" >:: ks_test_gof;
    "two-sample Kolmogorov-Smirnov test" >:: ks_test_two_sample;
  ]
