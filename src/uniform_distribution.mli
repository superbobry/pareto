open Internal

type t

include ContinuousDistribution with type t := t
include Mean with type t := t
include Variance with type t := t
