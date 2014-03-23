(** Commonly used probability distributions. *)

module Features : sig
  (** Distribution features. *)
  module type S = sig
    type elt
    type t

    val mean     : t -> elt
    val variance : t -> elt
    val skewness : t -> elt
    val kurtosis : t -> elt
  end

  (** Distribution features, which are allowed to be undefined for some
      combinations of distribution parameters. *)
  module type Opt = sig
    type elt
    type t

    val mean_opt     : t -> elt option
    val variance_opt : t -> elt option
    val skewness_opt : t -> elt option
    val kurtosis_opt : t -> elt option
  end
end

module type DiscreteDistribution = sig
  type elt
  type t

  (** Samples a single point from the distribution. *)
  val random : ?rng:Gsl.Rng.t -> t -> elt

  (** Samples [size] data points from the distribution. *)
  val sample : ?rng:Gsl.Rng.t -> size:int -> t -> elt array

  (** Computes cumulative probability function for a given value [k],
      i. e. [P(X <= k)], the probability that a random variable [X] will
      be found at a value less than or equal to [k]. *)
  val cumulative_probability : t -> k:elt -> float

  (** Computes probability mass function for a given value [k], i. e.
      [P(X = k)], the probability that a random variable [X] is
      {b exactly} equal to [k] *)
  val probability : t -> k:elt -> float

  (** Computes natural logarithm of the probability mass function for
       a given value [k]. *)
  val log_probability : t -> k:elt -> float
end

module type ContinuousDistribution = sig
  type elt
  type t

  (** Samples a single point from the distribution. *)
  val random : ?rng:Gsl.Rng.t -> t -> elt

  (** Samples [size] data points from the distribution. *)
  val sample : ?rng:Gsl.Rng.t -> size:int -> t -> elt array

  (** Computes cumulative probability function for a given value [n],
      i. e. [P(X <= n)], the probability that a random variable [X] will
      be found at a value less than or equal to [n]. *)
  val cumulative_probability : t -> x:elt -> float

  (** Computes probability density function for a given value [n], i. e.
      [P(X = n)], the probability that a random variable [X] is
      {b exactly} equal to [n] *)
  val density : t -> x:elt -> float

  (** Computes natural logarithm of the probability density function for
      a given value [n]. *)
  val log_density  : t -> x:elt -> float

  (** Computes inverse cumulative probability function for a given
      probability [p]. *)
  val quantile : t -> p:float -> elt
end


(** {2 Continuous distributions} *)

(** The normal distribution. *)
module Normal : sig
  type t = {
    normal_mean : float;
    normal_sd   : float
  }

  include ContinuousDistribution with type t := t and type elt = float
  include Features.S with type t := t and type elt := float

  (** Creates normal distribution from parameters. *)
  val create   : mean:float -> sd:float -> t

  (** Standard normal distribution with 0 [mean] and [sd] equal to 1. *)
  val standard : t

  (** Creates normal distribution with a MLE of parameters, estimated
      from given data. *)
  val mle : float array -> t
end

(** The log-normal distribution. *)
module LogNormal : sig
  type t = {
    lognormal_mean : float;
    lognormal_sd   : float
  }

  include ContinuousDistribution with type t := t and type elt = float
  include Features.S with type t := t and type elt := float

  (** Creates log-normal distribution from parameters. *)
  val create : mean:float -> sd:float -> t

  (** Creates log-normal distribution with a MLE of parameters, estimated
      from given data. *)
  val mle : float array -> t
end

(** Random variate distributed uniformly in the interval. *)
module Uniform : sig
  type t = {
    uniform_lower : float;
    uniform_upper : float
  }

  include ContinuousDistribution with type t := t and type elt = float
  include Features.S with type t := t and type elt := float

  (** Creates uniform distribution over a given interval. *)
  val create : lower:float -> upper:float -> t

  (** Creates uniform distribution with a MLE of parameters, estimated
      from given data. *)
  val mle : float array -> t
end

(** The exponential distribution.

    The probability distribution of the times between events in a Poisson
    process, in which events occur continuously and independently at a
    constant average [rate]. *)
module Exponential : sig
  type t = { exp_scale : float }

  include ContinuousDistribution with type t := t and type elt = float
  include Features.S with type t := t and type elt := float

  (** Creates exponential distribution. [scale] must be positive. *)
  val create : scale:float -> t

  (** Creates exponential distribution with a MLE of parameters, estimated
      from given data. *)
  val mle : float array -> t
end

(** The chi-squared distribution.

    The probability distribution of sum of squares of [df] independent
    standard normal distributions. *)
module ChiSquared : sig
  type t = { chisq_df : float }

  include ContinuousDistribution with type t := t and type elt = float
  include Features.S with type t := t and type elt := float

  (** Creates chi-squared distribution. Number of degrees of freedom
      must be positive. *)
  val create : df:int -> t

  (** Creates chi-squared distribution with parameters, estimated with
      method of moments. *)
  val mme : float array -> t
end

(** Fisher-Snedecor distribution. *)
module F : sig
  type t = {
    f_df1 : float;
    f_df2 : float
  }

  include ContinuousDistribution with type t := t and type elt = float
  include Features.Opt with type t := t and type elt := float

  (** Creates Fisher-Snedecor distribution with a given number of degrees
      of freedom. *)
  val create : df1:int -> df2:int -> t

  (** Creates Fisher-Snedecor distribution with parameters, estimated
      with method of moments. *)
  val mme : float array -> t
end

(** Student's t-distribution. *)
module T : sig
  type t = { t_df : float }

  include ContinuousDistribution with type t := t and type elt = float
  include Features.Opt with type t := t and type elt := float

  (** Creates Student's t-distribution with a given number of degrees
      of freedom. *)
  val create : df:float -> t

  (** Creates Student's t-distribution with parameters, estimated with
      method of moments. *)
  val mme : float array -> t
end

(** The gamma distribution. *)
module Gamma : sig
  type t = {
    gamma_shape : float;
    gamma_scale : float
  }

  include ContinuousDistribution with type t := t and type elt = float
  include Features.S with type t := t and type elt := float

  (** Creates gamma distribution. Both shape and scale must be positive. *)
  val create : shape:float -> scale:float -> t

  (** Creates gamma distribution with parameters, estimated with method
      of moments. *)
  val mme : float array -> t

  (** Creates gamma distribution with a MLE of parameters, estimated
      from given data. *)
  val mle : n_iter:int -> epsilon:float -> float array -> t
end

(** The Cauchy-Lorentz distribution.

    It doesn't have mean and variance. *)
module Cauchy : sig
  type t = {
    cauchy_location : float;
    cauchy_scale    : float
  }

  include ContinuousDistribution with type t := t and type elt = float

  (** Creates Cauchy-Lorentz distribution from parameters. *)
  val create   : location:float -> scale:float -> t

  (** Cauchy-Lorentz distribution with 0 [location] and [scale] equal to 1. *)
  val standard : t
end

(** The beta distribution. *)
module Beta : sig
  type t = {
    beta_alpha : float;
    beta_beta  : float
  }

  include ContinuousDistribution with type t := t and type elt = float
  include Features.S with type t := t and type elt := float

  (** Creates beta distribution. Both shape parameters must be positive. *)
  val create : alpha:float -> beta:float -> t

  (** Creates beta distribution with parameters, estimated with method
      of moments. *)
  val mme : float array -> t
end

(** Logistic distribution. *)
module Logistic : sig
  type t = {
    logistic_location : float;
    logistic_scale    : float
  }

  include ContinuousDistribution with type t := t and type elt = float
  include Features.S with type t := t and type elt := float

  (** Creates logistic distribution. *)
  val create : location:float -> scale:float -> t

  (** Creates logistic distribution with parameters, estimated with method
      of moments. *)
  val mme : float array -> t
end


(** {2 Discrete distributions} *)

(** The Poisson distribution.

    The probability distribution of a number of events occurring in a
    fixed interval if these events occur with a known average [rate],
    and occur independently from each other within that interval. *)
module Poisson : sig
  type t = { poisson_rate : float }

  include DiscreteDistribution with type t := t and type elt = int
  include Features.S with type t := t and type elt := float

  (** Creates a Poisson distribution. [rate] must be positive. *)
  val create : rate:float -> t

  (** Creates a Poisson distribution with a MLE of parameters, estimated
      from given data. *)
  val mle : int array -> t
end

(** Bernoulli distribution.

    The probability distribution, which takes value [1] with success
    probability [p] and value [0] with failure probability [1 - p]. *)
module Bernoulli : sig
  type t = { bernoulli_p : float }

  include DiscreteDistribution with type t := t and type elt = int
  include Features.S with type t := t and type elt := float

  (** Creates Bernoulli distribution with given success probability [p]. *)
  val create : p:float -> t

  (** Creates a Bernoulli distribution with a MLE of parameters, estimated
      from given data. *)
  val mle : int array -> t
end

(** The binomial distribution.

    The probability distribution of the number of successes in a sequence
    of independent Bernoulli [trials]. *)
module Binomial : sig
  type t = {
    binomial_trials : int;
    binomial_p      : float
  }

  include DiscreteDistribution with type t := t and type elt = int
  include Features.S with type t := t and type elt := float

  (** Creates binomial distribution. Number of [trials] must be
      non-negative. *)
  val create : trials:int -> p:float -> t

  (** Creates binomial distribution with parameters, estimated with
      method of moments. *)
  val mme : int array -> t
end

(** The Geometric distribution.

    The probability distribution of the number of failures before the
    first success, supported on the set [[0, 1, ...]]. *)
module Geometric : sig
  type t = { geometric_p : float }

  include DiscreteDistribution with type t := t and type elt = int
  include Features.S with type t := t and type elt := float

  (** Creates Geometric distribution with a given probability of success. *)
  val create : p:float -> t

  (** Creates Geometric distribution with parameters, estimated with
      method of moments. *)
  val mme : int array -> t
end

(** The Hypergeometric distribution.

    The probability distribution of obtaining [k] elements of "type 1"
    in [t] samples from a population {b without} replacement, if the
    population contains [m] elements of "type 1" and [t - m] elements
    of "type 2". *)
module Hypergeometric : sig
  type t = {
    hyper_m : int;
    hyper_t : int;
    hyper_k : int
  }

  include DiscreteDistribution with type t := t and type elt = int
  include Features.S with type t := t and type elt := float

  (** Creates Hypergeometric distribution. *)
  val create : m:int -> t:int -> k:int -> t
end

(** Negative Binomial distribution.

    The probability distribution of the number of succeses in a sequence
    of Bernoulli trials before a specified  number of [failures] occurs. *)
module NegativeBinomial : sig
  type t = {
    nbinomial_failures : float;
    nbinomial_p        : float
  }

  include DiscreteDistribution with type t := t and type elt = int
  include Features.S with type t := t and type elt := float

  (** Creates negative Binomial distribution with a given number of
      failures and success probability. *)
  val create : failures:float -> p:float -> t

  (** Creates negative Binomial distribution with parameters, estimated
      with method of moments. *)
  val mme : int array -> t
end

module Categorical : sig
  module type OrderedType = sig
    type t

    val compare : t -> t -> int
  end

  module type S = sig
    include DiscreteDistribution

    (** Creates a categorical distribution over values of type [elt],
        where each value is given a probability, which defaults to [0]
        for values not in the list. *)
    val create : (elt * float) array -> t

    (** Creates a categorical distribution with a MLE of parameters,
        estimated from given data. *)
    val mle : elt array -> t
  end

  module Make
    :  functor (Elt : Map.OrderedType)
    -> S with type elt = Elt.t
end

(** {2 Shortcuts for creating distributions} *)

val normal : mean:float -> sd:float -> Normal.t
val log_normal : mean:float -> sd:float -> LogNormal.t
val uniform  : lower:float -> upper:float -> Uniform.t
val exponential : scale:float -> Exponential.t
val chi_squared : df:int -> ChiSquared.t
val f : df1:int -> df2:int -> F.t
val t : df:float -> T.t
val gamma  : shape:float -> scale:float -> Gamma.t
val cauchy : location:float -> scale:float -> Cauchy.t
val beta : alpha:float -> beta:float -> Beta.t
val logistic : location:float -> scale:float -> Logistic.t

val poisson : rate:float -> Poisson.t
val bernoulli : p:float -> Bernoulli.t
val binomial : trials:int -> p:float -> Binomial.t
val geometric : p:float -> Geometric.t
val hypergeometric : m:int -> t:int -> k:int -> Hypergeometric.t
val negative_binomial : failures:float -> p:float -> NegativeBinomial.t
