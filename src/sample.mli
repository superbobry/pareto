(** Commonly used sample statistics. *)

val min    : float array -> float
val max    : float array -> float
val minmax : float array -> (float * float)

(** {e O(n)} Computes sample's range, i. e. the difference between the
    largest and smallest elements of a sample. *)
val range  : float array -> float

(** {e O(n)} Computes sample's arithmetic mean. *)
val mean     : float array -> float

(** {e O(n)} Computes MLE of a sample's variance. Also known as the
    {e population variance}, where the denominator is [n]. *)
val variance : ?mean:float -> float array -> float

(** {e O(n)} Computes histogram of a data set. Bin sizes are uniform,
    based on a given [range], whic defaults to
    [(min - k, max + k)], where [k = (min - max) / (bins - 1) * 2].
    This behaviour is copied from the excellent
    {{: http://github.com/bos/statistics} statistics} library by
    Brian O'Sullivan. *)
val histogram
  :  ?bins:int
  -> ?range:(float * float)
  -> ?weights:float array
  -> ?density:bool
  -> float array
  -> float array


module Quantile : sig
  (** Parameters for the continious sample method. *)
  type continous_param =
    | CADPW           (** Linear interpolation of the {e ECDF}. *)
    | Hazen           (** Hazen's definition. *)
    | SPSS            (** Definition used by the SPSS statistics application,
                          also known as Weibull's definition. *)
    | S               (** Definition used by the S statistics application.org
                          Interpolation points divide the sample range into
                          [n - 1] intervals. {b Default}. *)
    | MedianUnbiased  (** Median unbiased definition. The resulting quantile
                          estimates are approximately median unbiased
                          regardless of the distribution of [vs] *)
    | NormalUnbiased  (** Normal unbiased definition. An approximately unbiased
                          estimate if the empirical distribution approximates
                          the normal distribution. *)

  (** {e O(n log n)} Estimates sample quantile corresponding to the given
      probability [p], using the continuous sample method with given
      parameters. *)
  val continous_by
    : ?param:continous_param -> ?p:float -> float array -> float

  (** {e O(n log n)} Estimates interquantile range of a given sample,
      using the continuous sample method with given parameters. *)
  val iqr : ?param:continous_param -> float array -> float
end

(** {e O(n log n)} Estimates sample quantile corresponding to the given
    probability [p], using the continuous sample method with default
    parameters. *)
val quantile : ?p:float -> float array -> float

(** {e O(n log n)} Estimates interquantile range of a given sample,
    using the continuous sample method with given parameters. *)
val iqr : float array -> float


open Internal

(** {e O(n)} Shuffles a given array using Fisher-Yates shuffle. *)
val shuffle : ?rng:Rng.t -> 'a array -> 'a array

(** {e O(n)} Takes a sample of the specified [size] from the given
    array either with or without replacement. [size] defaults to the
    whole array. *)
val sample
  : ?rng:Rng.t -> ?replace:bool -> ?size:int -> 'a array -> 'a array
