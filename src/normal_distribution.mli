open Internal

type t = {
  normal_mean : float;
  normal_sd   : float
}

include ContinuousDistribution with type t := t
include Mean with type t := t
include Variance with type t := t
