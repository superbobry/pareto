(** Resampling statistics. *)

open Internal

(** Repeatidly resamples a given data set with replacement, computing a
    statistical estimate over the resampled data. *)
val resample
  :  ?rng:Rng.t
  -> estimator:('a array -> 'b)
  -> n:int
  -> 'a array
  -> 'b array

(** Repeatidly computes a statistical estimate over the data set, leaving
    out a single observation at a time. *)
val jackknife : estimator:('a array -> 'b) -> 'a array -> 'b array
