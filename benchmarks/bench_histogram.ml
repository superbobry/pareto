open Core
open Core_bench.Std
open Pareto


let () =
  let open Distributions.Normal in
  let vs = sample ~size:1024 standard in
  let test_histogram = Bench.Test.create
      ~name:"Histogram on standard Gaussian sample"
      (fun () -> ignore (Sample.histogram vs))
  in begin
    Command.run (Bench.make_command [test_histogram])
  end
