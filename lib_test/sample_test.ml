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

and test_quantile () =
  let rec vs =
    [|0.952363286988083; 0.829666168783014; 0.563616484350936;
      0.386334933107061; 0.0833841367636058; 0.99997428768617;
      0.374802467851785; 0.349201461890657; 0.89384498325876;
      0.431750792813907|]
  and go ~param ~msg expected =
    Array.iteri (fun i p ->
        assert_almost_equal
          ~epsilon:1e-14 ~msg:(msg ^ Printf.sprintf " %.2f" p)
          (Quantile.continuous_by ~param ~p vs) expected.(i)
      )
      [|0.; 0.25; 0.5; 0.75; 1.|]
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

let test = "Sample" >::: [
    "summary statistics, n = 100" >:: test_summary ~size:100;
    "summary statistics, n = 1000" >:: test_summary ~size:1000;
    "summary statistics, n = 10000" >:: test_summary ~size:10000;
    "quantile" >:: test_quantile;
    "rank" >:: test_rank
  ]
