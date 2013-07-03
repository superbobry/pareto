(** Tutti-frutti statistical functions. *)

open Internal

(** {e O(n)} Calculates a cumulative statistic over a given array. *)
val cumulative : f:('a -> 'a -> 'a) -> 'a array -> 'a array


(** {e O(log n)} Searches for the index of a given element [v] in array
    [vs], sorted with a given comparison function [cmp]. *)
val search_sorted : cmp:('a -> 'a -> int) -> 'a array -> 'a -> int option


(** Creates an array of integers given a semiopen range [\[a, b)]. *)
val range : ?a:int -> b:int -> int array


(** {e O(n)} Shuffles a given array using Fisher-Yates shuffle. *)
val shuffle : ?rng:Rng.t -> 'a array -> 'a array

(** {e O(n)} Takes a sample of the specified [size] from the given
    array either with or without replacement. [size] defaults to the
    whole array. *)
val sample
  : ?rng:Rng.t -> ?replace:bool -> ?size:int -> 'a array -> 'a array
