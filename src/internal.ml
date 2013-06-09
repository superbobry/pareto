
module Rng = Gsl.Rng
module Randist = Gsl.Randist
module Cdf = Gsl.Cdf

let default_rng = let open Rng in
  env_setup ();
  make (default ())

module Combi = struct
  open Bigarray

  type t = {
    n    : int;
    k    : int;
    data : (int, int_elt, c_layout) Bigarray.Array1.t
  }

  external _init_first : t -> unit = "ml_gsl_combination_init_first"
  external _init_last  : t -> unit = "ml_gsl_combination_init_last"

  let make n k =
    let c = { n; k; data = Array1.create int c_layout k } in begin
      _init_first c;
      c
    end

  let to_array { data; _ } =
    let len = Array1.dim data in
    Array.init len (Array1.get data)

  external prev : t -> unit = "ml_gsl_combination_prev"
  external next : t -> unit = "ml_gsl_combination_next"

  external _valid : t -> bool = "ml_gsl_combination_valid"

  let valid c =
    let open Gsl in
    try _valid c
    with Error.Gsl_exn (Error.FAILURE, _) -> false
end

let sqr x = x *. x
let cube x = x *. x *. x

let flip f x y = f y x

let is_nan (x : float) = x <> x
let is_not_nan (x : float) = x = x

let round x = int_of_float (floor (x +. 0.5))

let float_of_bool b = if b then 1. else 0.

let bound ?(a=0) ~b i = min (max i a) b

let invalid_arg s = raise (Invalid_argument s)

exception Not_implemented of string

let not_implemented s = raise (Not_implemented s)

module Array = struct
  include Array

  let range a b =
    if b <= a
    then [||]
    else
      let vs = make (b - a) 0 in
      for i = a to b - 1 do
        unsafe_set vs (i - a) i
      done; vs

  let sort_index cmp vs =
    let order = range 0 (length vs) in begin
      sort (fun i j -> cmp (unsafe_get vs i) (unsafe_get vs j)) order;
      order
    end

  let cumulative f = function
    | [||] -> [||]
    | xs   ->
      let n   = length xs in
      let acc = make n (unsafe_get xs 0) in
      for i = 1 to n - 1 do
        unsafe_set acc i (f (unsafe_get acc (i - 1)) (unsafe_get xs i))
      done; acc

  let sum = fold_left (+.) 0.
  let sum_with f = fold_left (fun acc x -> acc +. f x) 0.
  let count p = sum_with (fun v -> if p v then 1. else 0.)
  let for_all p = fold_left (fun acc x -> acc && p x) true
  let exists p = fold_left (fun acc x -> acc || p x) false

  let partition p vs =
    let (l, r) = fold_left
        (fun (l, r) x -> if p x then (x :: l, r) else (l, x :: r))
        ([], [])
        vs
    in (Array.of_list l, Array.of_list r)
end

module Vector = struct
  include Gsl.Vector_flat

  let sort = Gsl.Gsl_sort.vector_flat
  let sort_index = Gsl.Gsl_sort.vector_flat_index
  let partial_sort = Gsl.Gsl_sort.vector_flat_smallest
end

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
    | `Rows -> of_array (Array.map Array.sum (to_arrays m)) 1 nrow
    | `Columns ->
      let res = create ~init:0. 1 ncol in
      iter (fun _i j x -> set res 0 j (get res 0 j +. x)) m;
      res

  let sum m = Array.sum (to_array (sum_by `Rows m))
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
