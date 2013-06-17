(** Commonly used probability distributions. *)

open Internal


module type Mean = sig
  type t

  val mean : t -> float
end

module type MeanOpt = sig
  type t

  val mean_opt : t -> float option
end

module type Variance = sig
  type t

  val variance : t -> float
end

module type VarianceOpt = sig
  type t

  val variance_opt : t -> float option
end

module type BaseDistribution = sig
  type t
  type elt

  val generate : ?rng:Rng.t -> t -> elt
  val sample   : ?rng:Rng.t -> size:int -> t -> elt array
end

module type DiscreteDistribution = sig
  include BaseDistribution with type elt := int

  val cumulative_probability : t -> n:int -> float
  val probability : t -> n:int -> float
end

module type ContinuousDistribution = sig
  include BaseDistribution with type elt := float

  val cumulative_probability : t -> x:float -> float
  val density  : t -> x:float -> float
  val quantile : t -> p:float -> float
end


(** The normal distribution. *)
module Normal : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  (** Creates normal distribution from parameters. *)
  val create   : mean:float -> sd:float -> t

  (** Standard normal distribution with 0 [mean] and [sd] equal to 1. *)
  val standard : t
end

(** Random variate distributed uniformly in the interval. *)
module Uniform : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  (** Creates uniform distribution over a given interval. *)
  val create : lower:float -> upper:float -> t
end

(** The exponential distribution.

    The probability distribution of the times between events in a Poisson
    process, in which events occur continuously and independently at a
    constant average [rate]. *)
module Exponential : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  (** Creates exponential distribution. [rate] must be positive. *)
  val create : rate:float -> t
end

(** The Poisson distribution.

    The probability distribution of a number of events occurring in a
    fixed interval if these events occur with a known average [rate],
    and occur independently from each other within that interval. *)
module Poisson : sig
  include DiscreteDistribution
  include Mean with type t := t
  include Variance with type t := t

  (** Creates a Poisson distribution. [rate] must be positive. *)
  val create : rate:float -> t
end

(** The binomial distribution.

    The probability distribution of the number of successes in a sequence
    of independent Bernoulli [trials].
*)
module Binomial : sig
  include DiscreteDistribution
  include Mean with type t := t
  include Variance with type t := t

  (** Creates binomial distribution. Number of [trials] must be
      non-negative. *)
  val create : trials:int -> p:float -> t
end

(** The chi-squared distribution.

    The probability distribution of sum of squares of [df] independent
    standard normal distributions. *)
module ChiSquared : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  (** Construct chi-squared distribution. Number of degrees of freedom
      must be positive. *)
  val create : df:int -> t
end

(** Fisher-Snedecor distribution. *)
module F : sig
  include ContinuousDistribution
  include MeanOpt with type t := t
  include VarianceOpt with type t := t


  (** Creates Fisher-Snedecor distribution with a given number of degrees
      of freedom. *)
  val create : df1:int -> df2:int -> t
end

(* Student's t-distribution. *)
module T : sig
  include ContinuousDistribution
  include MeanOpt with type t := t
  include VarianceOpt with type t := t

  (** Creates Student's t-distribution with a given number of degrees
      of freedom. *)
  val create : df:float -> t
end

(** The gamma distribution. *)
module Gamma : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  (** Creates gamma distribution. Both shape and scale must be positive. *)
  val create : shape:float -> scale:float -> t
end

(** The Cauchy-Lorentz distribution.

    It doesn't have mean and variance. *)
module Cauchy : sig
  include ContinuousDistribution

  (** Creates Cauchy-Lorentz distribution from parameters. *)
  val create   : location:float -> scale:float -> t

  (** Cauchy-Lorentz distribution with 0 [location] and [scale] equal to 1. *)
  val standard : t
end

(** The beta distribution. *)
module Beta : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  (** Creates beta distribution. Both shape parameters must be positive. *)
  val create : alpha:float -> beta:float -> t
end

(** The Geometric distribution.

    The probability distribution of the number of failures before the
    first success, supported on the set [[0, 1, ...]]. *)
module Geometric : sig
  include DiscreteDistribution
  include Mean with type t := t
  include Variance with type t := t

  (** Creates Geometric distribution with a given probability of success. *)
  val create : p:float -> t
end

(** The Hypergeometric distribution.

    The probability distribution of obtaining [k] elements of "type 1"
    in [t] samples from a population without replacement, if the
    population contains [m] elements of "type 1" and [t - m] elements
    of "type 2". *)
module Hypergeometric : sig
  include DiscreteDistribution
  include Mean with type t := t
  include Variance with type t := t


  (** Creates Hypergeometric distribution. *)
  val create : m:int -> t:int -> k:int -> t
end

(** Negative Binomial distribution.

    The probability distribution of the number of successes in a sequence
    of Bernoulli trials before a specified  number of [failures] occur. *)
module NegativeBinomial : sig
  include DiscreteDistribution
  include Mean with type t := t
  include Variance with type t := t

  (** Creates negaive Binomial distribution with a given number of
      failures and success probability. *)
  val create : failures:int -> p:float -> t
end


val normal : mean:float -> sd:float -> Normal.t
val uniform  : lower:float -> upper:float -> Uniform.t
val exponential : rate:float -> Exponential.t
val poisson : rate:float -> Poisson.t
val binomial : trials:int -> p:float -> Binomial.t
val chi_squared : df:int -> ChiSquared.t
val f : df1:int -> df2:int -> F.t
val t : df:float -> T.t
val gamma  : shape:float -> scale:float -> Gamma.t
val cauchy : location:float -> scale:float -> Cauchy.t
val beta : alpha:float -> beta:float -> Beta.t
val geometric : p:float -> Geometric.t
val hypergeometric : m:int -> t:int -> k:int -> Hypergeometric.t
val negative_binomial : failures:int -> p:float -> NegativeBinomial.t
