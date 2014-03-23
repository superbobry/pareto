open OUnit

open Pareto.Distributions

open Common


let test_log_density () =
  let go ~msg
      (type t)
      (module D : ContinuousDistribution with type t = t and type elt = float)
      (d : t) =
    for i = 0 to 100 do
      let x = Uniform.(random (create ~lower:0. ~upper:42.)) in
      assert_almost_equal ~msg (log (D.density d ~x)) (D.log_density d ~x)
    done
  in begin
    go "Normal" (module Normal) (normal ~mean:4. ~sd:2.);
    go "LogNormal" (module LogNormal) (log_normal ~mean:4. ~sd:2.);
    go "Uniform" (module Uniform) (uniform ~lower:2. ~upper:4.);
    go "Exponential" (module Exponential) (exponential ~scale:42.);
    go "ChiSquared" (module ChiSquared) (chi_squared ~df:42);
    go "F" (module F) (f ~df1:4 ~df2:2);
    go "T" (module T) (t ~df:42.);
    go "Gamma" (module Gamma) (gamma ~shape:4. ~scale:2.);
    go "Cauchy" (module Cauchy) (cauchy ~location:4. ~scale:2.);
    go "Beta" (module Beta) (beta ~alpha:4. ~beta:2.);
    go "Logistic" (module Logistic) (logistic ~location:4. ~scale:2.)
  end

and test_log_probability () =
  let go ~msg
      (type t)
      (module D : DiscreteDistribution with type t = t and type elt = int)
      (d : t) =
    for i = 0 to 100 do
      let n = int_of_float (Uniform.(random (create ~lower:0. ~upper:42.))) in
      assert_almost_equal ~msg:msg
        (log (D.probability d ~n)) (D.log_probability d ~n)
    done
  in begin
    go "Poisson" (module Poisson) (poisson ~rate:42.);
    go "Bernoulli" (module Bernoulli) (bernoulli ~p:0.42);
    go "Binomial" (module Binomial) (binomial ~trials:42 ~p:0.42);
    go "Geometric" (module Geometric) (geometric ~p:0.42);
    go "NegativeBinomial" (module NegativeBinomial)
      (negative_binomial ~failures:42. ~p:0.42);
  end


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
    "density vs. log_density" >:: test_log_density;
    "probability vs. log_probability" >:: test_log_probability;

    "Gamma MLE" >:: test_gamma_mle;
    "Negative-Binomial MLE" >:: test_nb_mle
  ]
