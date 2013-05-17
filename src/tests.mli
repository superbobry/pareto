(** Statistical testing. *)

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
  (** Mann-Whitney U test (also known as Mann-Whitney-Wilcoxon test and
      Wilcoxon rank sum test) is a non-paramteric test, which evaluates
      the null hypothesis that two {e independent} samples have equal
      medians. *)
  val two_sample_independent
    :  'a array
    -> 'a array
    -> ?alternative:test_alternative
    -> ?correction:bool
    -> unit
    -> (float * float)

  (** {6 References}

      + Gravetter, Frederick J., and Larry B. Wallnau.
       "Statistics for the behavioral sciences". Wadsworth Publishing
       Company, 2006. *)
end
