open Internal

type t = {
  normal_mean : float;
  normal_sd   : float
}

let create ~mean ~sd =
  if sd > 0.
  then { normal_mean = mean; normal_sd = sd }
  else failwith "Normal_distribution.create: standard deviation must be positive"

let standard = create ~mean:0. ~sd:1.

let cumulative_probability { normal_mean; normal_sd } ~x =
  erfc ((normal_mean -. x) /. Constants.m_sqrt_2_pi *. normal_sd) /. 2.

let density { normal_mean; normal_sd } ~x =
  exp (-. (normal_mean -. x) *. (normal_mean -. x) /.
          (2. *. normal_sd *. normal_sd)) /.
    Constants.m_sqrt_2_pi *. normal_sd

let mean { normal_mean; _ } = normal_mean
and variance { normal_sd; _ } = normal_sd *. normal_sd
