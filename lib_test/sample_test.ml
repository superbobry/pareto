open OUnit

open Pareto.Sample


let assert_almost_equal ?(epsilon=1e-10) =
  assert_equal ~cmp:(cmp_float ~epsilon) ~printer:(Printf.sprintf "%.10f")


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


let test = "Sample" >::: [
    "sample summary statistics, n = 100" >:: test_summary ~size:100;
    "sample summary statistics, n = 1000" >:: test_summary ~size:1000;
    "sample summary statistics, n = 10000" >:: test_summary ~size:10000
  ]
