(** Statistical testing. *)

type test_alternative = Less | Greater | TwoSided

type test_result = {
  test_statistic : float;
  test_pvalue    : float
}

(** Assess significance of the statistical test at a given
    [significance_level], which defaults to [0.05]. *)
val run_test
  :  ?significance_level:float
  -> (unit -> test_result)
  -> [`Significant | `NotSignificant]


module T : sig
  (** One sample Student's t-test, which evaluates the null hypothesis
      that a [mean] of a normally distributed variable is equal to the
      specified value. *)
  val one_sample
    :  float array
    -> ?mean:float
    -> ?alternative:test_alternative
    -> unit
    -> test_result

  (** Two sample t-test, which evaluates the null hypothesis that the
      difference of means of two {e independent} normally distributed
      populations is equal to the specified value. *)
  val two_sample_independent
    :  float array
    -> float array
    -> ?equal_variance:bool
    -> ?mean:float
    -> ?alternative:test_alternative
    -> unit
    -> test_result

  (** Paired two sample t-test, which evaluates the null hypothes that
      the difference of means of the two {e paired} normally distributed
      populations is equal to the specified value. *)
  val two_sample_paired
    :  float array
    -> float array
    -> ?mean:float
    -> ?alternative:test_alternative
    -> unit
    -> test_result
end

(** Pearson's chi-squared test. *)
module ChiSquared : sig
  val goodness_of_fit
    : float array -> ?expected:float array -> ?df:int -> unit -> test_result

  val independence
    : float array array -> ?correction:bool -> unit -> test_result
end

module KolmogorovSmirnov : sig
  (** One-sample Kolmogorov-Smirnov test for goodness of fit, which
      evaluates the distribution [G(x)] of the observed random variable
      against a given distribution [F(x)]. Under the null hypothesis
      the two distributions are identical, [G(x) = F(x)]. *)
  val goodness_of_fit
    :  float array
    -> cumulative_probability:(float -> float)
    -> ?alternative:test_alternative
    -> unit
    -> test_result

  (** Two-sample Kolmogorov-Smirnov test, which evaluates the null
      hypothesis, that two {e independent} samples are drawn from the
      same continious distribution.

      {b Note}: in the current implementation samples with ties will
      result in an [Invalid_argument] exception. *)
  val two_sample
    :  float array
    -> float array
    -> ?alternative:test_alternative
    -> unit
    -> test_result

  (** {6 References}

      + National Institute of Standards and Technology (US), et al.
        "Engineering statistics handbook", Section 1.3.5.16.
        The Institute, 2001.
      + Jingbo Wang, Wai Wan Tsang, and George Marsaglia.
        "Evaluating Kolmogorov's distribution." Journal of
        Statistical Software 8, no. 18. 2003.
      + Z. W. Birnbaum, Fred H. Tingey. "One-sided confidence contours
        for probability distribution functions." The Annals of
        Mathematical Statistics, pp592-596. 1951. *)
end

module MannWhitneyU : sig
  (** Mann-Whitney U test (also known as Mann-Whitney-Wilcoxon test and
      Wilcoxon rank sum test) is a non-paramteric test, which evaluates
      the null hypothesis that two {e independent} samples have equal
      medians. *)
  val two_sample_independent
    :  'a array
    -> 'a array
    -> ?alternative:test_alternative
    -> ?correction:bool
    -> unit
    -> test_result

  (** {6 References}

      + Gravetter, Frederick J. and Larry B. Wallnau.
        "Statistics for the behavioral sciences". Wadsworth Publishing
        Company, 2006.
      + David J. Sheskin. "Handbook of Parametric and Nonparametric
        Statistical Procedures", 3rd edition. CRC Press, 2003. *)
end

module WilcoxonT : sig
  (** Wilcoxon signed-rank test, which evaluates the null hypothesis
      that sample median is equal to the specified [shift].

      Test assumptions:

      + Sample under test was randomly selected from the population it
        represents.
      + All [vs -. shift] differences are iid and come from a continious
        population. *)
  val one_sample
    :  float array
    -> ?shift:float
    -> ?alternative:test_alternative
    -> ?correction:bool
    -> unit
    -> test_result

  (** Wilcoxon paired signed-rank test, which evaluates the null hypothesis
      that two {e related} samples have equal medians.

      Test assumptions:

      + Samples under test were randomly selected from the population
        they represent.
      + Observation differences [vs2 -. vs1] are iid and come from a
        continious population. *)
  val two_sample_paired
    :  float array
    -> float array
    -> ?alternative:test_alternative
    -> ?correction:bool
    -> unit
    -> test_result

  (** {6 References}

      + David J. Sheskin. "Handbook of Parametric and Nonparametric
        Statistical Procedures", 3rd edition. CRC Press, 2003.
      + http://www.fon.hum.uva.nl/Service/Statistics/Signed_Rank_Algorihms.html *)
end

module Sign : sig
  (** Sign test, which evaluates the null hypothesis that sample median is
      equal to the specified [shift].

      Test assumptions:

      + Sample under test was randomly selected from the population it
        represents. *)
  val one_sample
    :  float array
    -> ?shift:float
    -> ?alternative:test_alternative
    -> unit
    -> test_result

  (** Dependent samples sign test, which evaluates the null hypothesis
      that the median difference between observations from two {e related}
      samples is zero.

      Test assumptions:

      + Samples under test were randomly selected from the population they
        represent. *)
  val two_sample_paired
    :  float array
    -> float array
    -> ?alternative:test_alternative
    -> unit
    -> test_result
end


(** Adjustments for multiple comparisons. *)
module Multiple : sig
  type adjustment_method =
    | HolmBonferroni
    | BenjaminiHochberg

  (** Adjusts obtained P-values for multiple comparisons using a given
      adjustment method. *)
  val adjust : float array -> adjustment_method -> float array

  (** {6 References}

      + Yoav Benjamini and Yosef Hochberg. "Controlling the false discovery
        rate: a practical and powerful approach to multiple testing.",
        Journal of the Royal Statistical Society, Series B (Methodological),
        pp289-300, 1995. *)
end
