open OUnit

open Pareto.Sample

open Common


let test_summary ~size () =
  let vs =
    let open Pareto.Distributions.Uniform in
    sample ~size (create ~lower:(-42.) ~upper:42.)
  in

  let s = Array.fold_left Summary.add Summary.empty vs in begin
    assert_almost_equal ~msg:"min" (Summary.min s) (min vs);
    assert_almost_equal ~msg:"max" (Summary.max s) (max vs);
    assert_equal ~msg:"size" (Summary.size s) (Array.length vs);
    assert_almost_equal ~msg:"mean" (Summary.mean s) (mean vs);
    assert_almost_equal ~msg:"variance" (Summary.variance s) (variance vs);
    assert_almost_equal ~msg:"sd" (Summary.sd s) (sd vs)
  end

and test_combined_summary ~size () =
  let vs =
    let open Pareto.Distributions.Uniform in
    sample ~size (create ~lower:(-42.) ~upper:42.)
  in

  let mid = size / 2 in
  let v1  = Array.sub vs 0 mid
  and v2  = Array.sub vs mid (size - mid) in
  let s1  = Array.fold_left Summary.add Summary.empty v1
  and s2  = Array.fold_left Summary.add Summary.empty v2 in
  let s12 = Summary.Monoid.mappend s1 s2
  and s   = Array.fold_left Summary.add Summary.empty vs in begin
    assert_almost_equal ~msg:"min" (Summary.min s) (Summary.min s12);
    assert_almost_equal ~msg:"max" (Summary.max s) (Summary.max s12);
    assert_equal ~msg:"size" (Summary.size s) (Summary.size s12);
    assert_almost_equal ~msg:"mean" (Summary.mean s) (Summary.mean s12);
    assert_almost_equal ~msg:"variance"
      (Summary.variance s) (Summary.variance s12);
    assert_almost_equal ~msg:"sd" (Summary.sd s) (Summary.sd s12);
    assert_almost_equal ~msg:"skewness"
      (Summary.skewness s) (Summary.skewness s12);
    assert_almost_equal ~msg:"kurtosis"
      (Summary.kurtosis s) (Summary.kurtosis s12);
  end

and test_quantile () =
  let rec vs =
    [|0.952363286988083; 0.829666168783014; 0.563616484350936;
      0.386334933107061; 0.0833841367636058; 0.99997428768617;
      0.374802467851785; 0.349201461890657; 0.89384498325876;
      0.431750792813907|]
  and go ~param ~msg expected =
    assert_equal
      ~msg ~cmp:(cmp_array ~cmp:cmp_float)
      ~printer:(printer_array ~printer:(Printf.sprintf "%.6f"))
      (Quantile.continuous_by ~param ~ps:[|0.; 0.25; 0.5; 0.75; 1.|] vs)
      expected
  in begin
    go ~param:Quantile.CADPW ~msg:"type 4" [|
      0.0833841367636058; 0.362001964871221; 0.431750792813907;
      0.861755576020887; 0.99997428768617
    |];
    go ~param:Quantile.Hazen ~msg:"type 5" [|
      0.0833841367636058; 0.374802467851785; 0.497683638582421;
      0.89384498325876; 0.99997428768617
    |];
    go ~param:Quantile.SPSS ~msg:"type 6" [|
      0.0833841367636058; 0.368402216361503; 0.497683638582421;
      0.908474559191091; 0.99997428768617
    |];
    go ~param:Quantile.S ~msg:"type 7" [|
      0.0833841367636058; 0.377685584165604; 0.497683638582421;
      0.877800279639823; 0.99997428768617
    |];
    go ~param:Quantile.MedianUnbiased ~msg:"type 8" [|
      0.0833841367636058; 0.372669050688358; 0.497683638582421;
      0.898721508569537; 0.99997428768617
    |];
    go ~param:Quantile.NormalUnbiased ~msg:"type 9" [|
      0.0833841367636058; 0.373202404979215; 0.497683638582421;
      0.897502377241843; 0.99997428768617
    |]
  end

and test_rank () =
  let vs = [|4.; 19.; 11.; 18.; 2.; 12.; 13.; 16.; 0.; 2.;
             2.; 7.; 1.; 17.; 16.; 19.; 11.; 12.; 19.; 4.|]
  in begin
    assert_equal
      ~msg:"average" ~cmp:(cmp_array ~cmp:cmp_float)
      ~printer:(printer_array ~printer:(Printf.sprintf "%.6f"))
      (snd (rank ~ties_strategy:`Average vs))
      [|6.5; 19.; 9.5; 17.; 4.; 11.5; 13.; 14.5; 1.; 4.; 4.;
        8.; 2.; 16.; 14.5; 19.; 9.5; 11.5; 19.; 6.5|];
    assert_equal
      ~msg:"min" ~cmp:(cmp_array ~cmp:cmp_float)
      ~printer:(printer_array ~printer:(Printf.sprintf "%.6f"))
      (snd (rank ~ties_strategy:`Min vs))
      [|6.; 18.; 9.; 17.; 3.; 11.; 13.; 14.; 1.; 3.; 3.; 8.;
        2.; 16.; 14.; 18.; 9.; 11.; 18.; 6.|];
    assert_equal
      ~msg:"max" ~cmp:(cmp_array ~cmp:cmp_float)
      ~printer:(printer_array ~printer:(Printf.sprintf "%.6f"))
      (snd (rank ~ties_strategy:`Max vs))
      [|7.; 20.; 10.; 17.; 5.; 12.; 13.; 15.; 1.; 5.; 5.; 8.;
        2.; 16.; 15.; 20.; 10.; 12.; 20.; 7.|]
  end

and test_correlation () =
  let v1 = [|0.0172824052698839; -0.454504077892448; 1.25946495815052;
             1.34046889912174; 0.609915320636686; -0.217651830293509;
             0.991287334206914; -0.186392670591343; 0.0266357683474309;
             -1.45310338401619|]
  and v2 = [|0.505340435184033; -1.42575172974959; -0.521196733941402;
             -0.0185022933706756; 0.17230654109602; 1.67872553102743;
             0.480586104798118; 0.910431368258919; 0.373583673677502;
             -0.3494655448979|]
  in begin
    assert_almost_equal ~msg:"Pearson product-moment correlation"
      (Correlation.pearson v1 v2) 0.02894452;
    assert_almost_equal ~msg:"Spearman rank correlation"
      (Correlation.spearman v1 v2) (-0.07878788);

    assert_equal
      ~msg:"Pearson product-moment self-correlation"
      ~cmp:(cmp_array ~cmp:cmp_float)
      ~printer:(printer_array ~printer:(Printf.sprintf "%.6f"))
      (Correlation.Auto.pearson v1)
      [|1.; 0.0975335049156232; 0.00331984882534066; -0.20666268433441;
        0.179634622194576; -0.255208927944468; -0.289850963695003;
        -0.233617717921346; 0.162004926236094; 0.0428473917235931|]
  end

and test_moments () =
  let vs =
    let open Pareto.Distributions.Uniform in
    sample ~size:1024 (create ~lower:(-42.) ~upper:42.)
  in

  let k  = 8 in
  let ms = Pareto.Sample.moments k vs in
  for p = 1 to k do
    assert_almost_equal ~msg:(string_of_int p)
      (Array.fold_left (+.) 0.
         (Array.map (fun v -> v ** float_of_int p) vs) /. 1024.)
      ms.(p - 1)
  done


let test = "Sample" >::: [
    "summary statistics, n = 100" >:: test_summary ~size:100;
    "summary statistics, n = 1000" >:: test_summary ~size:1000;
    "summary statistics, n = 10000" >:: test_summary ~size:10000;
    "combined summary, n = 100" >:: test_combined_summary ~size:100;
    "combined summary, n = 1000" >:: test_combined_summary ~size:1000;
    "combined summary, n = 10000" >:: test_combined_summary ~size:10000;
    "quantile" >:: test_quantile;
    "rank" >:: test_rank;
    "correlation" >:: test_correlation;
    "moments" >:: test_moments
  ]
