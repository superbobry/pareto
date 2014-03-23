(** Commonly used sample statistics. *)

val min    : float array -> float
val max    : float array -> float
val minmax : float array -> (float * float)

(** {e O(n)} Computes sample's range, i. e. the difference between the
    largest and smallest elements of a sample. *)
val range : float array -> float

(** {e O(n k)} Computes an array of sample moments of order 1 to k, i. e.
    [E{X^1}, E{X^2}, ..., E{X^k}]. *)
val moments : int -> float array -> float array

(** {e O(n)} Computes sample's arithmetic mean. *)
val mean : float array -> float

(** {e O(n)} Computes unbiased estimate of a sample's variance, also
    known as the {e sample variance}, where the denominator is [n - 1]. *)
val variance : ?mean:float -> float array -> float

(** {e O(n)} Computes sample's standard deviation. *)
val sd : ?mean:float -> float array -> float

(** {e O(n)} Computes the skewness of a sample, which is a measure of
    asymmetry of its distribution. *)
val skewness : ?mean:float -> ?sd:float -> float array -> float

(** {e O(n)} Computes the excess kurtosis of a sample, which is a
    measure of a "peakedness" of its distribution. *)
val kurtosis : ?mean:float -> ?sd:float -> float array -> float


(** {e O(n log n)} Computes sample's ranks, [ties_strategy] controls
    which ranks are assigned to equal values:

    - [`Average] the average of ranks should be assigned to each value.
      {b Default}.
    - [`Min] the minimum of ranks is assigned to each value.
    - [`Max] the maximum of ranks is assigned to each value.

    Returns a pair, where the first element is ties correction factor
    and second is an array of sample ranks.

    {b References}

    + P. R. Freeman, "Algorithm AS 26: Ranking an array of numbers",
      Vol. 19, Applied Statistics, pp111-113, 1970. *)
val rank
  :  ?ties_strategy:[`Average | `Min | `Max]
  -> ?cmp:('a -> 'a -> int)
  -> 'a array
  -> (float * float array)

(** {e O(n)} Computes histogram of a data set. Bin sizes are uniform,
    based on a given [range], whic defaults to
    [(min - k, max + k)], where [k = (min - max) / (bins - 1) * 2].
    This behaviour is copied from the excellent
    {{: http://github.com/bos/statistics} statistics} library by
    Brian O'Sullivan. *)
val histogram
  :  ?n_bins:int
  -> ?range:(float * float)
  -> ?weights:float array
  -> ?density:bool
  -> float array
  -> (float array * float array)


module Quantile : sig
  (** Parameters for the continious sample method. *)
  type continuous_param =
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
  val continuous_by
    : ?param:continuous_param -> ps:float array -> float array -> float array

  (** {e O(n log n)} Estimates interquantile range of a given sample,
      using the continuous sample method with given parameters. *)
  val iqr : ?param:continuous_param -> float array -> float
end

(** {e O(n log n)} Estimates sample quantile corresponding to the given
    probability [p], using the continuous sample method with default
    parameters. *)
val quantile : ps:float array -> float array -> float array

(** {e O(n log n)} Estimates interquantile range of a given sample,
    using the continuous sample method with given parameters. *)
val iqr : float array -> float


module KDE : sig
  (** Bandwidth selection rules. *)
  type bandwidth =
    | Silverman  (** Use {e rule-of-thumb} for choosing the bandwidth.
                     It defaults to
                     [0.9 * min(SD, IQR / 1.34) * n^-0.2]. *)
    | Scott      (** Same as [Silverman], but with a factor, equal to
                     [1.06]. *)

  type kernel =
    | Gaussian

  (** {e O(n * points)} Simple kernel density estimator. Returns an array
      of uniformly spaced points from the sample range at which the
      density function was estimated, and the estimates at these points. *)
  val estimate_pdf
    :  ?kernel:kernel
    -> ?bandwidth:bandwidth
    -> ?n_points:int
    -> float array
    -> (float array * float array)

  (** {6 Example}

      {[
        open Pareto
        let open Distributions.Normal in
        let vs = sample ~size:100 standard in
        let (points, pdf) = Sample.KDE.estimate_pdf ~points:10 vs in begin
          (* Output an ASCII density plot. *)
          Array.iteri (fun i d ->
              let count = int_of_float (d *. 20.) in
              printf "%9.5f " points.(i);
              for i = 0 to count do
                print_char (if i = count then '.' else ' ');
              done;

              print_newline ();
            ) pdf
        end
      ]}

      {6 References}

      + B.W. Silverman, "Density Estimation for Statistics and Data
        Analysis", Vol. 26, Monographs on Statistics and Applied
        Probability, Chapman and Hall, London, 1986. *)
end


module Correlation : sig
  (** {e O(n)} Computes Pearson product-moment correlation coefficient
      for two given samples. *)
  val pearson : float array -> float array -> float

  (** {e O(n log n)} Computes Spearman rank correlation coefficient for
      two given samples, which is essentially Pearson correlation
      calculated for sample ranks. *)
  val spearman : ?cmp:('a -> 'a -> int) -> 'a array -> 'a array -> float

  (** Autocorrelation, i. e. the correlation of the sample against a
      shifted version of itself. *)
  module Auto : sig
    (** {e O(n^2)} Computes autocorrelation, using Person product-moment
        correlation coefficient. *)
    val pearson : float array -> float array
  end
end


(** Calculates summary statistics over a possibly infinite stream of data.

    The algorithm runs in {e O(1)} space and {e O(n)} time.

    It is preferred for computing standard deviation, because roundoff
    errors in floating point operations might lead to taking a square
    root of a negative value.

    {6 References}

    + D. E. Knuth, "The Art of Computer Programming, Volume 2:
      Seminumerical Algorithms", 2nd edition, Section 4.2.2, p216.
    + T. B. Terriberry, "Computing Higher-Order Moments Online", 2008,
      http://people.xiph.org/~tterribe/notes/homs.html *)
module Summary : sig
  type t

  (** Empty data set. *)
  val empty : t

  (** Combines statistics from two data sets. *)
  val combine : t -> t -> t

  (** Adds a value to the data set. *)
  val add : t -> float -> t

  (** Returns the maximum added value or [nan] if the data set is empty. *)
  val max : t -> float

  (** Returns the minimum added value or [nan] is the data set is empty. *)
  val min : t -> float

  (** Returns the number of available values. *)
  val size : t -> int

  (** Returns the arithmetic mean of the values that have been added
      or [nan] if the data set is empty. *)
  val mean : t -> float

  (** Returns the variance of the available values or [nan] if the
      data set is empty. *)
  val variance : t -> float

  (** Returns the standard deviation of the values that have been added
      or [nan] if the data set is empty. *)
  val sd : t -> float

  (** Returns the skewness of the values that have been added or [nan] if
      the data set is empty.

      {b Note}: for small sample sizes estimated value might be inaccurate,
      See issue #20. *)
  val skewness : t -> float

  (** Returns the excess kurtosis of the values that have been added
      or [nan] if the data set is empty.

      {b Note}: for small sample sizes estimated value might be inaccurate,
      See issue #20. *)
  val kurtosis : t -> float
end
