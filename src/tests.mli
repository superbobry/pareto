open Internal

type test_alternative = Less | Greater | TwoSided


module T : sig
  val one_sample
    :  float array
    -> ?mean:float
    -> ?alternative:test_alternative
    -> unit
    -> (float * float)

  val two_sample_independent
    :  float array
    -> float array
    -> ?equal_variance:bool
    -> ?mean:float
    -> ?alternative:test_alternative
    -> unit
    -> (float * float)

  val two_sample_related
    :  float array
    -> float array
    -> ?mean:float
    -> ?alternative:test_alternative
    -> unit
    -> (float * float)
end

module ChiSquared : sig
  val goodness_of_fit
    : float array -> ?expected:float array -> ?df:int -> unit -> (float * float)

  val independence
    : float array array -> ?correction:bool -> unit -> (float * float)
end

module Wilcoxon : sig
  val two_sample_independent
    :  'a array
    -> 'a array
    -> ?alternative:test_alternative
    -> ?correction:bool
    -> unit
    -> (float * float)
end
