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

let skewness ?mean ?sd vs = match (mean, sd) with
  | (Some mean, Some sd) -> Stats.skew_m_sd ~mean ~sd vs
  | (Some mean, None)    -> Stats.skew_m_sd ~mean ~sd:(Stats.sd ~mean vs) vs
  | (None, Some sd)      -> Stats.skew_m_sd ~mean:(Stats.mean vs) ~sd vs
  | (None, None)         -> Stats.skew vs

let kurtosis ?mean ?sd vs = match (mean, sd) with
  | (Some mean, None)    ->
    Stats.kurtosis_m_sd ~mean ~sd:(Stats.sd ~mean vs) vs
  | (None, Some sd)      ->
    Stats.kurtosis_m_sd ~mean:(Stats.mean vs) ~sd vs
  | (Some mean, Some sd) -> Stats.kurtosis_m_sd ~mean ~sd vs
  | (None, None)         -> Stats.kurtosis vs


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
        end; incr i
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
  type continuous_param =
    | CADPW
    | Hazen
    | SPSS
    | S
    | MedianUnbiased
    | NormalUnbiased

  let continuous_by ?(param=S) ?(p=0.5) vs =
    if p < 0. || p > 1.
    then invalid_arg "Quantile.continuous_by: p must be in range [0, 1]";
    if Array.exists is_nan vs
    then invalid_arg "Quantile.continuous_by: sample contains NaNs";

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
    let bound ?(a=0) ~b i = Pervasives.(min (max i a) b) in
    let n    = Array.length vs in
    let svs  = Vector.partial_sort (bound ~b:n (j + 1)) (Vector.of_array vs) in
    let item = fun i -> svs.(bound ~b:(n - 1) i) in
    (1. -. h) *. item (j - 1) +. h *. item j

  let iqr ?param vs =
    continuous_by ?param ~p:0.75 vs -. continuous_by ?param ~p:0.25 vs
end

let quantile ?p vs = Quantile.continuous_by ?p vs

let iqr vs = Quantile.iqr vs


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


module Correlation = struct
  let pearson v1 v2 =
    let n = Array.length v1 in
    if Array.length v2 <> n
    then invalid_arg "Correlation.pearson: unequal length arrays";

    let v1_mean = mean v1
    and v2_mean = mean v2 in
    let v12_sd  = sd ~mean:v1_mean v1 *. sd ~mean:v2_mean v2
    and acc     = ref 0. in
    for i = 0 to n - 1 do
      let v1 = Array.unsafe_get v1 i
      and v2 = Array.unsafe_get v2 i
      in acc := !acc +. (v1 -. v1_mean) *. (v2 -. v2_mean)
    done; !acc /. float_of_int (n - 1) /. v12_sd

  let spearman ?cmp v1 v2 =
    let n = Array.length v1 in
    if Array.length v2 <> n
    then invalid_arg "Correlation.spearman: unequal length arrays"
    else
      (* Note(superbobry): according to Wikipedia, ties strategy is
         fixed to [`Average]. *)
      let f vs = snd (rank ~ties_strategy:`Average ?cmp vs) in
      pearson (f v1) (f v2)

  module Auto = struct
    let pearson vs =
      let n = Array.length vs in
      if n < 2
      then [||]
      else
        let mean = Stats.mean vs in
        let acf shift =
          let acc = ref 0. in
          for i = 0 to n - shift - 1 do
            let v_i = Array.unsafe_get vs i
            and v_s = Array.unsafe_get vs (i + shift) in
            acc := !acc +. (v_s -. mean) *. (v_i -. mean)
          done; !acc /. float_of_int n
        in

        let ac  = Array.init n acf in
        let ac0 = ac.(0) in begin
          for i = 0 to n - 1 do
            Array.unsafe_set ac i (Array.unsafe_get ac i /. ac0)
          done; ac
        end
  end
end


module Summary = struct
  type t = {
    k     : int;
    m_1   : float;
    m_2   : float;
    m_3   : float;
    m_4   : float;
    max_k : float;
    min_k : float
  }

  let empty = {
    m_1   = 0.0;
    m_2   = 0.0;
    m_3   = 0.0;
    m_4   = 0.0;
    k     = 0;
    max_k = min_float;
    min_k = max_float;
  }

  let add t x_k =
    let n_k       = float_of_int (succ t.k) in
    let delta     = x_k -. t.m_1 in
    let delta_nk  = delta /. n_k in
    let delta_m_2 = delta *. delta_nk *. float_of_int t.k in
    {
      m_1   = t.m_1 +. delta_nk;
      m_2   = t.m_2 +. delta_m_2;
      m_3   = t.m_3 +.
                delta_m_2 *. delta_nk *. (n_k -. 2.) -.
                (3. *. delta_nk *. t.m_2);
      m_4   = t.m_4 +.
                delta_m_2 *. sqr delta_nk *. (sqr n_k -. 3. *. n_k +. 3.) +.
                6. *. sqr delta_nk *. t.m_2 -.
                4. *. delta_nk *. t.m_3;
      k     = succ t.k;
      min_k = Pervasives.min t.min_k x_k;
      max_k = Pervasives.max t.max_k x_k;
    }

  let size { k; _ } = k

  let min { min_k; _ } = if min_k = max_float then nan else min_k
  and max { max_k; _ } = if max_k = min_float then nan else max_k

  let mean t = if t.k > 0 then t.m_1 else nan

  let variance t = match t.k with
    (* Note(superbobry): we follow R and GSL here and treat variance
       of a single number undefined. *)
    | 0 | 1 -> nan
    | _ -> t.m_2 /. float_of_int (pred t.k)
  let sd t = sqrt (variance t)

  let skewness t =
    if t.k = 0
    then nan
    else sqrt (float_of_int t.k) *. t.m_3 /. (t.m_2 ** 1.5)
  and kurtosis t =
    if t.k = 0
    then nan
    else float_of_int t.k *. t.m_4 /. (t.m_2 *.  t.m_2) -. 3.

  module Monoid = struct
    let mempty = empty
    and mappend t1 t2 =
      if t1.k = 0
      then t2
      else if t2.k = 0
      then t1
      else
        (* Note(superbobry): we can optimize it later, but right now I'd
           rather have these formulas match Wikipedia _directly_. *)
        let delta = t2.m_1 -. t1.m_1
        and t1k   = float_of_int t1.k
        and t2k   = float_of_int t2.k
        and tnk   = float_of_int (t1.k + t2.k)
        in {
          m_1 = (t1k *. t1.m_1 +. t2k *. t2.m_1) /. tnk;
          m_2 = t1.m_2 +. t2.m_2 +. sqr delta *. t1k *. t2k /. tnk;
          m_3 = t1.m_3 +. t2.m_3
                +. cube delta *. t1k *. t2k *. (t1k -. t2k) /. sqr tnk
                +. 3. *. delta *. (t1k *. t2.m_2 -. t2k *. t1.m_2) /. tnk;
          m_4 = t1.m_4 +. t2.m_4
                +. ((delta ** 4.) *. t1k *. t2k *.
                      (sqr t1k -. t1k *. t2k +. sqr t2k) /. cube tnk)
                +. (6. *. sqr delta *.
                      (sqr t1k *. t2.m_2 +. sqr t2k *.  t1.m_2) /. sqr tnk)
                +. (4. *. delta *. (t1k *. t2.m_3 -. t2k *. t1.m_3) /. tnk);
          k = t1.k + t2.k;
          max_k = Pervasives.max t1.max_k t2.max_k;
          min_k = Pervasives.min t1.min_k t2.min_k
        }
  end
end
