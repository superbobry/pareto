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

module type DiscreteDistribution = sig
  type t

  val cumulative_probability : t -> n:int -> float
  val probability : t -> n:int -> float
end

module type ContinuousDistribution = sig
  type t

  val cumulative_probability : t -> x:float -> float
  val density  : t -> x:float -> float
  val quantile : t -> p:float -> float
end


module Randist = Gsl.Randist
module Cdf = Gsl.Cdf

let sqr x = x *. x
