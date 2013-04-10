open Internal

module Gaussian : sig
  include ContinuousDistribution

  val create   : mean:float -> sd:float -> t
  val standard : t
end

module Uniform : sig
  include ContinuousDistribution

  val create : lower:float -> upper:float -> t
end

module Exponential : sig
  include ContinuousDistribution

  val create : rate:float -> t
end

module Poisson : sig
  include DiscreteDistribution

  val create : rate:float -> t
end

module Binomial : sig
  include DiscreteDistribution

  val create : trials:int -> p:float -> t
end

module ChiSquared : sig
  include ContinuousDistribution

  val create : df:int -> t
end
