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


let test_gamma_mle () =
  let shape = Uniform.(random (create ~lower:0. ~upper:42.))
  and scale = Uniform.(random (create ~lower:0. ~upper:42.)) in
  let open Gamma in
  let vs = sample ~size:(1 lsl 16) (create ~shape ~scale) in
  let { gamma_shape; gamma_scale } = mle ~n_iter:100 ~epsilon:1e-6 vs in
  begin
    assert_almost_equal ~msg:"shape" ~epsilon:0.01 shape gamma_shape;
    assert_almost_equal ~msg:"scale" ~epsilon:0.01 scale gamma_scale
  end

and test_nb_mle () =
  let r = Uniform.(random (create ~lower:0. ~upper:42.))
  and p = Uniform.(random (create ~lower:0. ~upper:1.)) in
  let open NegativeBinomial in
  let vs = sample ~size:(1 lsl 16) (create ~failures:r ~p) in
  let { nbinomial_failures; nbinomial_p } =
    mle ~n_iter:100 ~epsilon:1e-6 vs
  in begin
    (* TODO(superbobry): this is a VERY rough test. I'm not sure if we can
       do significantly better, because 'r' is challenging to estimate
       accurately over a small sample. *)
    assert_almost_equal
      ~msg:"number of failures" ~epsilon:1. r nbinomial_failures;
    assert_almost_equal
      ~msg:"probability of success" ~epsilon:0.01 p nbinomial_p
  end


let test = "Distributions" >::: [
    "categorical" >:: test_categorical;

    "Gamma MLE" >:: test_gamma_mle;
    "Negative-Binomial MLE" >:: test_nb_mle
  ]
