open Internal

module Gaussian : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  val create   : mean:float -> sd:float -> t
  val standard : t
end

module Uniform : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  val create : lower:float -> upper:float -> t
end

module Exponential : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  val create : rate:float -> t
end

module Poisson : sig
  include DiscreteDistribution
  include Mean with type t := t
  include Variance with type t := t

  val create : rate:float -> t
end

module Binomial : sig
  include DiscreteDistribution
  include Mean with type t := t
  include Variance with type t := t

  val create : trials:int -> p:float -> t
end

module ChiSquared : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  val create : df:int -> t
end

module F : sig
  include ContinuousDistribution
  include MeanOpt with type t := t
  include VarianceOpt with type t := t

  val create : df1:int -> df2:int -> t
end

module T : sig
  include ContinuousDistribution
  include MeanOpt with type t := t
  include VarianceOpt with type t := t

  val create : df:float -> t
end

module Gamma : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  val create : shape:float -> scale:float -> t
end

module Cauchy : sig
  include ContinuousDistribution
  include MeanOpt with type t := t
  include VarianceOpt with type t := t

  val create   : location:float -> scale:float -> t
  val standard : t
end

module Beta : sig
  include ContinuousDistribution
  include Mean with type t := t
  include Variance with type t := t

  val create : alpha:float -> beta:float -> t
end

module Geometric : sig
  include DiscreteDistribution
  include Mean with type t := t
  include Variance with type t := t

  val create : p:float -> t
end

module Hypergeometric : sig
  include DiscreteDistribution
  include Mean with type t := t
  include Variance with type t := t

  val create : m:int -> t:int -> k:int -> t
end

module NegativeBinomial : sig
  include DiscreteDistribution
  include Mean with type t := t
  include Variance with type t := t

  val create : failures:int -> p:float -> t
end


val gaussian : mean:float -> sd:float -> Gaussian.t
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
