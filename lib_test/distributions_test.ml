open OUnit

open Pareto.Distributions

open Common


let test_categorical () =
  let module Strings = Categorical.Make(String) in
  let d =
    Strings.create [|("C", 0.3); ("A", 0.1); ("G", 0.5); ("T", 0.1)|]
  in begin
    assert_almost_equal ~msg:"Pr(X = A)" 0.1 (Strings.probability d "A");
    assert_almost_equal ~msg:"Pr(X = C)" 0.3 (Strings.probability d "C");
    assert_almost_equal ~msg:"Pr(X = G)" 0.5 (Strings.probability d "G");
    assert_almost_equal ~msg:"Pr(X = T)" 0.1 (Strings.probability d "T");

    assert_almost_equal ~msg:"Pr(X <= A)"
      (Strings.cumulative_probability d "A") 0.1;
    assert_almost_equal ~msg:"Pr(X <= T)"
      (Strings.cumulative_probability d "T") 1.0;

    assert_almost_equal ~msg:"Pr(X <= $)"
      (Strings.cumulative_probability d "$") 0.
  end


let test = "Distributions" >::: [
    "categorical" >:: test_categorical
  ]
