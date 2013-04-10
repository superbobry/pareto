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
end

module type DiscreteDistribution = sig
  include Distribution

  val cumulative_probability : t -> k:int -> float
  val probability : t -> k:int -> float
end

module type ContinuousDistribution = sig
  include Distribution

  val cumulative_probability : t -> x:float -> float
  val density  : t -> x:float -> float
  val quantile : t -> p:float -> float
end


module Randist = Gsl.Randist
module Cdf = Gsl.Cdf
