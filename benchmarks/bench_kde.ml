open Core
open Core_bench.Std
open Pareto


let () =
  let open Distributions.Normal in
  let vs = sample ~size:1024 standard in
  let test_kde = Bench.Test.create ~name:"KDE on standard Gaussian sample"
      (fun () -> ignore (Sample.KDE.estimate_pdf vs))
  in begin
    Command.run (Bench.make_command [test_kde])
  end
