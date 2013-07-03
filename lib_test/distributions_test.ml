open OUnit

open Pareto.Distributions

open Common


let test_categorical () =
  let module Strings = Categorical.Make(String) in
  let d =
    Strings.create [|("A", 0.1); ("C", 0.3); ("G", 0.5); ("T", 0.1)|]
  in begin
    assert_almost_equal ~msg:"Pr(X = A)" (Strings.probability d "A") 0.1;
    assert_almost_equal ~msg:"Pr(X = C)" (Strings.probability d "C") 0.3;
    assert_almost_equal ~msg:"Pr(X = G)" (Strings.probability d "G") 0.5;
    assert_almost_equal ~msg:"Pr(X = T)" (Strings.probability d "T") 0.1;

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
