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
