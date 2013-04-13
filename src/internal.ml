
module Rng = Gsl.Rng
module Randist = Gsl.Randist
module Cdf = Gsl.Cdf

let default_rng = let open Rng in
  env_setup();
  make (default ())

module Vector = struct
  include Gsl.Vector

  type t = vector

  let map2 ~f v1 v2 =
    let k = length v1 in
    if length v2 <> k then invalid_arg "Vector.map2";
    let acc = copy v1 in
    for i = 0 to k - 1 do
      acc.{i} <- f v1.{i} v2.{i}
    done; acc

  let sum v = let acc = ref 0. in
    for i = 0 to length v - 1 do
      acc := !acc +. v.{i}
    done; !acc
end

module Sample = Gsl.Stats

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

module type BaseDistribution = sig
  type t
  type elt

  val generate : ?rng:Rng.t -> t -> elt
  val sample   : ?rng:Rng.t -> size:int -> t -> elt array
end

let make_sampler (generate : ?rng:Rng.t -> 'a -> 'b) =
  fun ?(rng=default_rng) ~size d ->
    let rec go acc = function
    | 0 -> Array.of_list acc
    | i -> go (generate ~rng d :: acc) (i - 1)
    in go [] size

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
