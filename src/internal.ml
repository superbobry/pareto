module type Mean = sig
  type t

  val mean : t -> float
end

module type Variance = sig
  type t

  val variance : t -> float
end

module type Distribution = sig
  type t

  include Mean with type t := t
  include Variance with type t := t

  val cumulative_probability : t -> x:float -> float
end

module type DiscreteDistribution = sig
  type t

  include Distribution with type t := t

  val probability : t -> x:float -> float
end

module type ContinuousDistribution = sig
  type t

  include Distribution with type t := t

  val density  : t -> x:float -> float
  val quantile : t -> p:float -> float
end


module Randist = Gsl.Randist
module Cdf = Gsl.Cdf
