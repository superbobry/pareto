open Internal

type t = {
  normal_mean : float;
  normal_sd   : float;
}

let create ~mean ~sd =
  if sd > 0.
  then { normal_mean = mean; normal_sd = sd }
  else failwith "Normal_distribution.create: standard deviation must be positive"

let standard = create ~mean:0. ~sd:1.

let cumulative_probability { normal_mean; normal_sd } ~x =
  Cdf.gaussian_P ~sigma:normal_sd ~x:(x -. normal_mean)

let density { normal_mean; normal_sd } ~x =
  Randist.gaussian_pdf ~sigma:normal_sd (x -. normal_mean)
and quantile { normal_mean; normal_sd } ~p =
  Cdf.gaussian_Pinv ~sigma:normal_sd ~p +. normal_mean

let mean { normal_mean; _ } = normal_mean
and variance { normal_sd; _ } = normal_sd *. normal_sd
