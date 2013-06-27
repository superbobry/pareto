(** Commonly used probability distributions. *)

open Internal

(** Distribution features. *)
module type Features = sig
  type elt
  type t

  val mean     : t -> elt
  val variance : t -> elt
  val skewness : t -> elt
  val kurtosis : t -> elt
end

(** Distribution features, which are allowed to be undefined for some
    combinations of distribution parameters. *)
module type FeaturesOpt = sig
  type elt
  type t

  val mean_opt     : t -> elt option
  val variance_opt : t -> elt option
  val skewness_opt : t -> elt option
  val kurtosis_opt : t -> elt option
end

module type MLE = sig
  type t
  type elt

  (** Computes a MLE of distribution parameters from given data. *)
  val mle : elt array -> t
end

module type DiscreteDistribution = sig
  type t
  type elt

  (** Samples [size] data points from the distribution. *)
  val sample : ?rng:Rng.t -> size:int -> t -> elt array

  (** Computes cumulative probability function for a given value [n],
      i. e. [P(X <= n)], the probability that a random variable [X] will
      be found at a value less than or equal to [n]. *)
  val cumulative_probability : t -> n:elt -> float

  (** Computes probability mass function for a given value [n], i. e.
      [P(X = n)], the probability that a random variable [X] is
      {b exactly} equal to [n] *)
  val probability : t -> n:elt -> float
end

module type ContinuousDistribution = sig
  type t
  type elt

  (** Samples [size] data points from the distribution. *)
  val sample : ?rng:Rng.t -> size:int -> t -> elt array

  (** Computes cumulative probability function for a given value [n],
      i. e. [P(X <= n)], the probability that a random variable [X] will
      be found at a value less than or equal to [n]. *)
  val cumulative_probability : t -> x:elt -> float

  (** Computes probability density function for a given value [n], i. e.
      [P(X = n)], the probability that a random variable [X] is
      {b exactly} equal to [n] *)
  val density  : t -> x:elt -> float

  (** Computes inverse cumulative probability function for a given
      probability [p]. *)
  val quantile : t -> p:float -> elt
end


(** {2 Continuous distributions} *)

(** The normal distribution. *)
module Normal : sig
  include ContinuousDistribution with type elt = float
  include Features with type t := t and type elt := float
  include MLE with type t := t and type elt := float

  (** Creates normal distribution from parameters. *)
  val create   : mean:float -> sd:float -> t

  (** Standard normal distribution with 0 [mean] and [sd] equal to 1. *)
  val standard : t
end

(** The log-normal distribution. *)
module LogNormal : sig
  include ContinuousDistribution with type elt = float
  include Features with type t := t and type elt := float
  include MLE with type t := t and type elt := float

  (** Creates log-normal distribution from parameters. *)
  val create : mean:float -> sd:float -> t
end

(** Random variate distributed uniformly in the interval. *)
module Uniform : sig
  include ContinuousDistribution with type elt = float
  include Features with type t := t and type elt := float
  include MLE with type t := t and type elt := float

  (** Creates uniform distribution over a given interval. *)
  val create : lower:float -> upper:float -> t
end

(** The exponential distribution.

    The probability distribution of the times between events in a Poisson
    process, in which events occur continuously and independently at a
    constant average [rate]. *)
module Exponential : sig
  include ContinuousDistribution with type elt = float
  include Features with type t := t and type elt := float
  include MLE with type t := t and type elt := float

  (** Creates exponential distribution. [rate] must be positive. *)
  val create : rate:float -> t
end

(** The chi-squared distribution.

    The probability distribution of sum of squares of [df] independent
    standard normal distributions. *)
module ChiSquared : sig
  include ContinuousDistribution with type elt = float
  include Features with type t := t and type elt := float

  (** Construct chi-squared distribution. Number of degrees of freedom
      must be positive. *)
  val create : df:int -> t
end

(** Fisher-Snedecor distribution. *)
module F : sig
  include ContinuousDistribution with type elt = float
  include FeaturesOpt with type t := t and type elt := float

  (** Creates Fisher-Snedecor distribution with a given number of degrees
      of freedom. *)
  val create : df1:int -> df2:int -> t
end

(** Student's t-distribution. *)
module T : sig
  include ContinuousDistribution with type elt = float
  include FeaturesOpt with type t := t and type elt := float

  (** Creates Student's t-distribution with a given number of degrees
      of freedom. *)
  val create : df:float -> t
end

(** The gamma distribution. *)
module Gamma : sig
  include ContinuousDistribution with type elt = float
  include Features with type t := t and type elt := float

  (** Creates gamma distribution. Both shape and scale must be positive. *)
  val create : shape:float -> scale:float -> t
end

(** The Cauchy-Lorentz distribution.

    It doesn't have mean and variance. *)
module Cauchy : sig
  include ContinuousDistribution with type elt = float

  (** Creates Cauchy-Lorentz distribution from parameters. *)
  val create   : location:float -> scale:float -> t

  (** Cauchy-Lorentz distribution with 0 [location] and [scale] equal to 1. *)
  val standard : t
end

(** The beta distribution. *)
module Beta : sig
  include ContinuousDistribution with type elt = float
  include Features with type t := t and type elt := float

  (** Creates beta distribution. Both shape parameters must be positive. *)
  val create : alpha:float -> beta:float -> t
end


(** {2 Discrete distributions} *)

(** The Poisson distribution.

    The probability distribution of a number of events occurring in a
    fixed interval if these events occur with a known average [rate],
    and occur independently from each other within that interval. *)
module Poisson : sig
  include DiscreteDistribution with type elt = int
  include Features with type t := t and type elt := float
  include MLE with type t := t and type elt := int

  (** Creates a Poisson distribution. [rate] must be positive. *)
  val create : rate:float -> t
end

(** The binomial distribution.

    The probability distribution of the number of successes in a sequence
    of independent Bernoulli [trials]. *)
module Binomial : sig
  include DiscreteDistribution with type elt = int
  include Features with type t := t and type elt := float

  (** Creates binomial distribution. Number of [trials] must be
      non-negative. *)
  val create : trials:int -> p:float -> t
end

(** The Geometric distribution.

    The probability distribution of the number of failures before the
    first success, supported on the set [[0, 1, ...]]. *)
module Geometric : sig
  include DiscreteDistribution with type elt = int
  include Features with type t := t and type elt := float

  (** Creates Geometric distribution with a given probability of success. *)
  val create : p:float -> t
end

(** The Hypergeometric distribution.

    The probability distribution of obtaining [k] elements of "type 1"
    in [t] samples from a population {b without} replacement, if the
    population contains [m] elements of "type 1" and [t - m] elements
    of "type 2". *)
module Hypergeometric : sig
  include DiscreteDistribution with type elt = int
  include Features with type t := t and type elt := float

  (** Creates Hypergeometric distribution. *)
  val create : m:int -> t:int -> k:int -> t
end

(** Negative Binomial distribution.

    The probability distribution of the number of successes in a sequence
    of Bernoulli trials before a specified  number of [failures] occur. *)
module NegativeBinomial : sig
  include DiscreteDistribution with type elt = int
  include Features with type t := t and type elt := float

  (** Creates negaive Binomial distribution with a given number of
      failures and success probability. *)
  val create : failures:int -> p:float -> t
end


(** {2 Shortcuts for creating distributions} *)

val normal : mean:float -> sd:float -> Normal.t
val log_normal : mean:float -> sd:float -> LogNormal.t
val uniform  : lower:float -> upper:float -> Uniform.t
val exponential : rate:float -> Exponential.t
val chi_squared : df:int -> ChiSquared.t
val f : df1:int -> df2:int -> F.t
val t : df:float -> T.t
val gamma  : shape:float -> scale:float -> Gamma.t
val cauchy : location:float -> scale:float -> Cauchy.t
val beta : alpha:float -> beta:float -> Beta.t

val poisson : rate:float -> Poisson.t
val binomial : trials:int -> p:float -> Binomial.t
val geometric : p:float -> Geometric.t
val hypergeometric : m:int -> t:int -> k:int -> Hypergeometric.t
val negative_binomial : failures:int -> p:float -> NegativeBinomial.t
