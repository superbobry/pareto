open Internal

type t

include ContinuousDistribution with type t := t

val create : lower:float -> upper:float -> t
