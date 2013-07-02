module Rng = Gsl.Rng

let default_rng = let open Rng in
  env_setup ();
  make (default ())


(** FIXME(superbobry): remove this once gsl-1.14.0 is out. *)
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

let is_nan (x : float) = x <> x
let is_not_nan (x : float) = x = x

let round x = int_of_float (floor (x +. 0.5))

let invalid_arg s = raise (Invalid_argument s)


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

  let reorder is src dst =
    let n = length src in
    for i = 0 to n - 1 do
      let j = unsafe_get is i in unsafe_set dst i (unsafe_get src j)
    done

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

  let exists =
    let rec on_j p m i max_j j =
      if j >= max_j then
        false
      else
        (p (get m i j)) || on_j p m i max_j (j+1)
    in
    let rec on_i p m max_i max_j i =
      if i >= max_i then
        false
      else
        (on_j p m i max_j 0) || on_i p m max_i max_j (i+1)
    in
    fun p m ->
      let (max_i, max_j) = dims m in
      on_i p m max_i max_j 0

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

  let power m =
    let open Gsl.Blas_flat in
    let mul a b =
      assert (dims a = dims b);
      let (w, h) = dims a in
      let res = create ~init:0. w h in begin
        gemm ~ta:NoTrans ~tb:NoTrans ~alpha:1. ~beta:1. ~a ~b ~c:res;
        res
      end
    in

    let rec go m = function
      | 1 -> m
      | k ->
        let m2 = go m (k / 2) in
        let mm = mul m2 m2 in
        if k mod 2 = 0
        then mm
        else mul mm m
    in go m
end
