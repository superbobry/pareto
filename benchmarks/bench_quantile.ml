open Core
open Core_bench.Std
open Pareto


let () =
  let open Distributions.Normal in
  let vs = sample ~size:8096 standard in
  let test_quantiles = Bench.Test.create
      ~name:"Quantiles of standard Gaussian sample"
      (fun () -> ignore (Sample.quantile ~ps:[|0.; 0.25|] vs))
  in begin
    Command.run (Bench.make_command [test_quantiles])
  end
