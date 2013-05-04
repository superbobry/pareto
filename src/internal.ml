
module Rng = Gsl.Rng
module Randist = Gsl.Randist
module Cdf = Gsl.Cdf

let default_rng = let open Rng in
  env_setup ();
  make (default ())

let sqr x = x *. x

let invalid_arg s = raise (Invalid_argument s)

let sum_array = Array.fold_left (+.) 0.

module Matrix = struct
  include Gsl.Matrix_flat

  let iter f m =
    let (nrow, ncol) = dims m in
    for i = 0 to nrow - 1 do
      for j = 0 to ncol - 1 do
        f i j (get m i j)
      done
    done

  let exists p m =
    (* FIXME(superbobry): This may be too slow, rewrite with an exception? *)
    let res = ref false in
    iter (fun _i _j x -> res := !res || p x) m;
    !res

  let abs m = iter (fun i j x -> set m i j (abs_float x)) m

  let sum_by direction m =
    let (nrow, ncol) = dims m in
    match direction with
    | `Rows -> of_array (Array.map sum_array (to_arrays m)) 1 nrow
    | `Columns ->
      let res = create ~init:0. 1 ncol in
      iter (fun _i j x -> set res 0 j (get res 0 j +. x)) m;
      res

  let sum m = sum_array (to_array (sum_by `Rows m))
end


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
