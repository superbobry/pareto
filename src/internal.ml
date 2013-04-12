
module Rng = Gsl.Rng
module Randist = Gsl.Randist
module Cdf = Gsl.Cdf

let default_rng = Rng.(make (default ()))

module Vector = struct
  include Gsl.Vector

  type t = vector

  let sum v = let acc = ref 0. in
    for i = 0 to length v - 1 do
      acc := !acc +. v.{i}
    done; !acc
end

let sqr x = x *. x

let invalid_arg s = raise (Invalid_argument s)


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

module type Distribution = sig
  type t
  type elt

  val generate : ?rng:Rng.t -> t -> elt
  (* val sample   : ?rng:Rng.t -> size:int -> t -> elt array *)
end

module type DiscreteDistribution = sig
  include Distribution with type elt := int

  val cumulative_probability : t -> n:int -> float
  val probability : t -> n:int -> float
end

module type ContinuousDistribution = sig
  include Distribution with type elt := float

  val cumulative_probability : t -> x:float -> float
  val density  : t -> x:float -> float
  val quantile : t -> p:float -> float
end
