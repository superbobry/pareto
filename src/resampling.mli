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


module Bootstrap : sig
  type estimate = {
    point            : float;  (** Point estimate. *)
    lower_bound      : float;  (** Lower bound of the estimate confidence
                                   interval *)
    upper_bound      : float;  (** Upper bound of the estimate confidence
                                   interval *)
    confidence_level : float   (** Condifence level corresponding to the
                                   above intervals. *)
  }

  (** Bias-corrected and accelerated (BCa) bootstrap. *)
  val bca
    :  ?rng:Rng.t
    -> ?confidence_level:float
    -> estimator:(float array -> float)
    -> n:int
    -> float array
    -> estimate
end
