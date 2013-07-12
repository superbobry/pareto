open Internal


let jackknife ~estimator vs =
  let n = Array.length vs in
  if n = 0
  then [||]
  else
    let init = Array.unsafe_get vs 0 in
    Array.init n ~f:(fun i ->
        let holey = Array.make (n - 1) init in begin
          Array.blit ~src:vs ~src_pos:0 ~dst:holey ~dst_pos:0 ~len:i;
          Array.blit ~src:vs ~src_pos:(i + 1)
            ~dst:holey ~dst_pos:i ~len:(n - i - 1);
          estimator holey
        end)


let resample ?rng ~estimator ~n_iter vs =
  Array.init n_iter
    ~f:(fun _i -> estimator (Base.sample ?rng ~replace:true vs))


module Bootstrap = struct
  type estimate = {
    point            : float;
    lower_bound      : float;
    upper_bound      : float;
    confidence_level : float
  }

  let bca ?rng ?(confidence_level=0.95) ~estimator ~n_iter vs =
    if confidence_level <= 0. || confidence_level >= 1.
    then invalid_arg "Bootstrap.bca: confidence level must be in range (0, 1)";

    let point = estimator vs in
    if Array.length vs = 1
    then { point; lower_bound = point; upper_bound = point; confidence_level }
    else
      let rvs = resample ?rng ~estimator ~n_iter vs in
      Array.sort ~cmp:compare rvs;  (* Sort, since we need percentiles. *)

      let jack        = jackknife ~estimator vs in
      let jack_mean   = Sample.mean jack in
      let z_2s        = Array.map jack ~f:(fun v -> sqr (jack_mean -. v)) in
      let sum_cubes   = Array.fold_left z_2s ~init:0.
          ~f:(fun acc z_2 -> acc +. sqr z_2)
      and sum_squares = Array.fold_left z_2s ~f:(+.) ~init:0. in
      let accel = sum_cubes /. (6. *. (sum_squares ** 1.5)) in
      let p     = float_of_int (Array.count ~f:(fun v -> v < point) rvs) /.
                    float_of_int n_iter
      in

      let open Distributions.Normal in
      let bias = quantile standard ~p
      and z    = quantile standard ~p:((1. -. confidence_level) /. 2.) in
      let b1 = bias +. z
      and b2 = bias -. z in
      let a1 = bias +. b1 /. (1. -. accel *. b1)
      and a2 = bias +. b2 /. (1. -. accel *. b2) in

      let pnorm x =
        round (cumulative_probability standard ~x *. float_of_int n_iter)
      in {
        point;
        lower_bound = rvs.(max (pnorm a1) 0);
        upper_bound = rvs.(min (pnorm a2) (n_iter - 1));
        confidence_level
      }
end
