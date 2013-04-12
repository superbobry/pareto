open Internal
open Distributions

type test_type = OneTailed | TwoTailed
type test_result = Significant | NonSignigicant

let run_test ?(pvalue=0.05) f =
  let (_statistic, actual_pvalue) = f () in
  if actual_pvalue < pvalue
  then NonSignigicant
  else Significant

let chi_squared f_expected f_observed ?(df=0) () =
  let k = Vector.length f_expected in
  if Vector.length f_observed <> k then invalid_arg "chi_squared";

  let delta = Vector.copy f_expected in
  let chisq = begin
    Vector.sub delta f_observed;
    Vector.mul delta delta;
    Vector.div delta f_expected;
    Vector.sum delta
  end in

  let prob =
    ChiSquared.(cumulative_probability ~x:chisq (create ~df:(k - 1 - df)))
  in (chisq, 1. -. prob)
