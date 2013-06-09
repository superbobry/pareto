(** Tutti-frutti statistical functions. *)

open Internal

(** {e O(n)} Calculates a cumulative statistic over a given array. *)
val cumulative : ('a -> 'a -> 'a) -> 'a array -> 'a array


(** Creates an array of integers given a semiopen range [\[a, b)]. *)
val range : int -> int -> int array


(** {e O(n)} Shuffles a given array using Fisher-Yates shuffle. *)
val shuffle : ?rng:Rng.t -> 'a array -> 'a array

(** {e O(n)} Takes a sample of the specified [size] from the given
    array either with or without replacement. [size] defaults to the
    whole array. *)
val sample
  : ?rng:Rng.t -> ?replace:bool -> ?size:int -> 'a array -> 'a array
