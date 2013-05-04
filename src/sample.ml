open Internal

module Stats = Gsl.Stats
module Histo = Gsl.Histo

let min = Stats.min
and max = Stats.max
and minmax = Stats.minmax

let mean vs = Stats.mean vs
let variance ?(mean=nan) vs =
  if mean = nan
  then Stats.variance vs
  else Stats.variance ~mean vs

let histogram ?(bins=10) ?range ?weights ?(density=false) vs =
  if bins <= 0 then invalid_arg "Sample.histogram: bins must be positive";

  let h = Histo.make bins in
  begin match range with
    | None ->
      let (min, max) = minmax vs in
      let d = if bins = 1 then 0. else max -. min in
      Histo.set_ranges_uniform h ~xmin:(min -. d) ~xmax:(max +. d)
    | Some (min, max) ->
      Histo.set_ranges_uniform h ~xmin:min ~xmax:max
  end;

  Array.iteri (fun i v ->
    let weight = match weights with
      | None -> 1.
      | Some weights -> weights.(i)  (* Note(superbobry): possibly unsafe. *)
    in Histo.accumulate h ~w:weight v) vs;

  if density then Histo.scale h (1. /. Histo.sum h);

  let counts = Array.make bins 0. in
  for i = 0 to bins - 1 do
    counts.(i) <- Histo.get h i
  done; counts
