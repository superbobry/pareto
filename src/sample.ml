open Internal

module Stats = Gsl.Stats
module Histo = Gsl.Histo

let min = Stats.min
and max = Stats.max
and minmax = Stats.minmax
let range vs =
  let (min, max) = minmax vs in
  max -. min

let mean vs = Stats.mean vs
let variance ?mean vs = Stats.variance ?mean vs
let sd ?mean vs = Stats.sd ?mean vs


let _resolve_ties next d = function
  | `Average    -> float_of_int next -. float_of_int d /. 2.
  | `Min        -> float_of_int (next - d)
  | `Max        -> float_of_int next

let _correct_ties ranks =
  let n = Array.length ranks in
  if n < 2
  then 1.0
  else
    let sorted = Array.copy ranks
    and t = ref 0
    and d = ref 0
    and i = ref 0 in begin
      Array.sort (fun r1 r2 ->
          compare (int_of_float r1) (int_of_float r2)) sorted;

      while !i < n - 1 do
        if sorted.(!i) = sorted.(!i + 1)
        then begin
          d := 1;
          while !i < n - 1 && sorted.(!i) = sorted.(!i + 1) do
            incr d;
            incr i;
          done;

          t := !t + (!d * !d * !d - !d)
        end;
        incr i
      done; float_of_int !t
    end

let rank ?(ties_strategy=`Average) ?(cmp=compare) vs =
  let n     = Array.length vs in
  let order = Array.sort_index cmp vs in
  let ranks = Array.make n 0. in
  let d     = ref 0 in begin
    for i = 0 to n - 1 do
      if i == n - 1 || cmp vs.(order.(i)) vs.(order.(i + 1)) <> 0
      then
        let tie_rank = _resolve_ties (i + 1) !d ties_strategy in
        for j = i - !d to i do
          ranks.(order.(j)) <- tie_rank
        done;
        d := 0
      else
        incr d  (* Found a duplicate! *)
    done;
  end; (_correct_ties ranks, ranks)


let histogram ?(bins=10) ?range ?weights ?(density=false) vs =
  if bins <= 0
  then invalid_arg "Sample.histogram: bins must be a positive integer";
  if vs = [||] then invalid_arg "Sample.histogram: empty sample";

  let h = Histo.make bins in
  begin match range with
    | None ->
      let (min, max) = minmax vs in
      let d =
        if bins = 1
        then 0.
        else (max -. min) /. float_of_int ((bins - 1) * 2)
      in Histo.set_ranges_uniform h ~xmin:(min -. d) ~xmax:(max +. d)
    | Some (min, max) when min < max ->
      Histo.set_ranges_uniform h ~xmin:min ~xmax:max
    | Some (_min, _max) ->
      invalid_arg ("Sample.histogram: max must be larger than min " ^
                   " in range paramter")
  end;

  Array.iteri (fun i v ->
    let weight = match weights with
      | None -> 1.
      | Some weights -> weights.(i)  (* Note(superbobry): possibly unsafe. *)
    in Histo.accumulate h ~w:weight v) vs;

  if density then Histo.scale h (1. /. Histo.sum h);

  let counts = Array.make bins 0. in
  let points = Array.make bins 0. in
  for i = 0 to bins - 1 do
    counts.(i) <- Histo.get h i;
    points.(i) <- fst (Histo.get_range h i)
  done; (points, counts)


module Quantile = struct
  type continous_param =
    | CADPW
    | Hazen
    | SPSS
    | S
    | MedianUnbiased
    | NormalUnbiased

  let finalize vs h j =
    let n    = Array.length vs in
    let svs  = Vector.partial_sort (bound ~b:n (j + 1)) (Vector.of_array vs) in
    let item = fun i -> svs.(bound ~b:(n - 1) i) in
    (1. -. h) *. item (j - 1) +. h *. item j

  let continous_by ?(param=S) ?(p=0.5) vs =
    if p < 0. || p > 1.
    then invalid_arg "Quantile.continous_by: p must be in range [0, 1]";
    if Array.exists is_nan vs
    then invalid_arg "Quantile.continous_by: sample contains NaNs";

    let (a, b) = match param with
      | CADPW -> (0., 1.)
      | Hazen -> (0.5, 0.5)
      | SPSS  -> (0., 0.)
      | S     -> (1., 1.)
      | MedianUnbiased -> (1. /. 3., 1. /. 3.)
      | NormalUnbiased -> (3. /. 8., 3. /. 8.)
    in

    let n    = Array.length vs in
    let nppm = a +. p *. (float_of_int n +. 1. -. a -. b) in
    let fuzz = epsilon_float *. 4. in
    let j    = int_of_float (floor (nppm +. fuzz)) in
    let h    = if abs_float (nppm -. float_of_int j) < fuzz
               then 0.
               else nppm -. float_of_int j in
    finalize vs h j

  let iqr ?param vs =
    continous_by ?param ~p:0.75 vs -. continous_by ?param ~p:0.25 vs
end

let quantile ?p vs = Quantile.continous_by ?p vs

let iqr vs = Quantile.iqr vs


let shuffle ?(rng=default_rng) vs =
  let svs = Array.copy vs in begin
    Randist.shuffle rng svs;
    svs
  end

and sample ?(rng=default_rng) ?(replace=false) ?size vs =
  let dst = match size with
    | Some n ->
      if vs = [||] || not replace && Array.length vs < n
      then invalid_arg "Sample.sample: not enough elements to sample from"
      else Array.create n vs.(0)
    | None      -> Array.copy vs
  in begin
    if replace
    then Randist.sample rng ~src:vs ~dst
    else Randist.choose rng ~src:vs ~dst;
    dst
  end


module KDE = struct
  type bandwidth =
    | Silverman
    | Scott

  type kernel =
    | Gaussian

  let build_kernel = function
    | Gaussian ->
      fun h p v ->
        let u = (v -. p) /. h in
        let open Gsl.Math in
        (1. /. (sqrt2 *. sqrtpi)) *. exp (-. sqr u /. 2.)

  let build_points points h kernel vs =
    let (min, max) = minmax vs in
    let (a, b) = match kernel with
      | Gaussian     -> (min -. 3. *. h, max +. 3. *. h)
    in
    let step = (b -. a) /. float_of_int points in
    Array.init points (fun i -> a +. (float_of_int i) *. step)

  let estimate_pdf ?(kernel=Gaussian) ?(bandwidth=Scott) ?(points=512) vs =
    if Array.length vs < 2
    then invalid_arg "KDE.estimate_pdf: sample should have multiple elements";

    let n = float_of_int (Array.length vs) in
    let s = Pervasives.min (sd vs) (iqr vs /. 1.34) in
    let h = match bandwidth with
      | Silverman  -> 0.90 *. s *. (n ** -0.2)
      | Scott      -> 1.06 *. s *. (n ** -0.2)
    in

    let points = build_points points h kernel vs in
    let k      = build_kernel kernel in
    let f      = 1. /. (h *. n) in
    let pdf    = Array.map (fun p -> f *. Array.sum_with (k h p) vs) points in
    (points, pdf)
end
