open Internal

let jackknife ~estimator vs =
  let n = Array.length vs in
  Array.init n (fun i ->
      (** Note(superbobry): we can be sure that [vs] has _at least_ one
          element if this function was called. *)
      let holey = Array.make (n - 1) vs.(0) in begin
        Array.blit vs 0 holey 0 i;
        Array.blit vs (i + 1) holey i (n - i - 1);
        estimator holey
      end
    )

let resample ?rng ~estimator ~n vs =
  Array.init n (fun _i -> estimator (Sample.sample ?rng ~replace:true vs))


module Bootstrap = struct
  type estimate = {
    point            : float;
    lower_bound      : float;
    upper_bound      : float;
    confidence_level : float
  }

  let bca ?rng ?(confidence_level=0.95) ~estimator ~n vs =
    if confidence_level <= 0. || confidence_level >= 1.
    then invalid_arg "Bootstrap.bca: confidence level must be in range (0, 1)";

    let point = estimator vs in
    if Array.length vs = 1
    then { point; lower_bound = point; upper_bound = point; confidence_level }
    else
      let rvs = resample ?rng ~estimator ~n vs in
      Array.sort compare rvs;  (* Sort, since we need percentiles. *)

      (* Note(superbobry): this can be done faster by first computing a
         Z-transform of jackknife results and then squaring and cubing in
         a single pass. *)
      let jack        = jackknife ~estimator vs in
      let jack_mean   = Sample.mean jack in
      let sum_cubes   = Array.sum_with (fun v -> cube (jack_mean -. v)) jack in
      let sum_squares = Array.sum_with (fun v -> sqr  (jack_mean -. v)) jack in
      let accel = sum_cubes /. (6. *. (sum_squares ** 1.5)) in
      let open Distributions.Normal in
      let bias  = quantile standard
          ~p:(Array.count (fun v -> v < point) rvs /. float_of_int n)
      and z  = quantile standard ~p:((1. -. confidence_level) /. 2.) in
      let b1 = bias +. z
      and b2 = bias -. z in
      let a1 = bias +. b1 /. (1. -. accel *. b1)
      and a2 = bias +. b2 /. (1. -. accel *. b2) in

      let pnorm x =
        round (cumulative_probability standard ~x *. float_of_int n)
      in {
        point;
        lower_bound = rvs.(max (pnorm a1) 0);
        upper_bound = rvs.(min (pnorm a2) (n - 1));
        confidence_level
      }
end
