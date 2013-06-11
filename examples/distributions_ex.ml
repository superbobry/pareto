open Pareto.Distributions

open Common


let distribution_mean (type t)
    (module D : Mean with type t = t)
    (d : t) () =
  Printf.printf "E[X] = %.4f\n" (D.mean d)

let distribution_quantile (type t)
    (module D : ContinuousDistribution with type t = t)
    (d : t) () =
  Printf.printf "Q(0.5) = %.4f\n" (D.quantile ~p:0.5 d)

let distribution_sample (type t)
    (module D : DiscreteDistribution with type t = t)
    (d : t) () =
  print_int_array (D.sample ~size:10 d)


let () = begin
  distribution_mean (module Normal) (normal ~mean:0. ~sd:1.) ();
  distribution_quantile (module Beta) (beta ~alpha:1. ~beta:0.5) ();
  distribution_sample (module Poisson) (poisson ~rate:0.5) ()
end
