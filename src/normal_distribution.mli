open Internal

type t

include ContinuousDistribution with type t := t

val create   : mean:float -> sd:float -> t
val standard : t
