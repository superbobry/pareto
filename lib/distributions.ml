open Internal

module Cdf = Gsl.Cdf
module Randist = Gsl.Randist
module Rng = Gsl.Rng

module Features = struct
  module type S = sig
    type elt
    type t

    val mean     : t -> elt
    val variance : t -> elt
    val skewness : t -> elt
    val kurtosis : t -> elt
  end

  module type Opt = sig
    type elt
    type t

    val mean_opt     : t -> elt option
    val variance_opt : t -> elt option
    val skewness_opt : t -> elt option
    val kurtosis_opt : t -> elt option
  end
end

module type BaseDistribution = sig
  type elt
  type t

  val sample : ?rng:Rng.t -> size:int -> t -> elt array
end

module type DiscreteDistribution = sig
  include BaseDistribution

  val cumulative_probability : t -> n:elt -> float
  val probability : t -> n:elt -> float
end

module type ContinuousDistribution = sig
  include BaseDistribution

  val cumulative_probability : t -> x:elt -> float
  val density  : t -> x:elt -> float
  val quantile : t -> p:float -> elt
end


let make_sampler generate ?rng ~size d =
  let init = generate ?rng d in
  let vs   = Array.make size init in begin
    for i = 1 to size - 1 do
      Array.unsafe_set vs i (generate ?rng d)
    done; vs
  end


module Normal = struct
  type elt = float
  type t   = {
    normal_mean : float;
    normal_sd   : float
  }

  let create ~mean ~sd =
    if sd > 0.
    then { normal_mean = mean; normal_sd = sd }
    else invalid_arg "Normal.create: standard deviation must be positive"

  let standard = create ~mean:0. ~sd:1.

  let cumulative_probability { normal_mean; normal_sd } ~x =
    Cdf.gaussian_P ~sigma:normal_sd ~x:(x -. normal_mean)

  let density { normal_mean; normal_sd } ~x =
    Randist.gaussian_pdf ~sigma:normal_sd (x -. normal_mean)
  and quantile { normal_mean; normal_sd } ~p =
    if p < 0. || p > 1.
    then invalid_arg "Normal.quantile: p must be in range [0, 1]"
    else Cdf.gaussian_Pinv ~sigma:normal_sd ~p +. normal_mean

  let mean { normal_mean; _ } = normal_mean
  and variance { normal_sd; _ } = sqr normal_sd
  and skewness _d = 0.
  and kurtosis _d = 0.

  let generate ?(rng=default_rng) { normal_mean; normal_sd } =
    Randist.gaussian ~sigma:normal_sd rng +. normal_mean
  let sample = make_sampler generate

  let mle vs =
    let mean = Sample.mean vs in
    let sd   = Sample.sd ~mean vs in
    create ~mean ~sd
end

module LogNormal = struct
  type elt = float
  type t   = {
    lognormal_mean : float;
    lognormal_sd   : float
  }

  let create ~mean ~sd =
    if sd > 0.
    then { lognormal_mean = mean; lognormal_sd = sd }
    else invalid_arg "LogNormal.create: standard deviation must be positive"

  let cumulative_probability { lognormal_mean; lognormal_sd } ~x =
    Cdf.lognormal_P ~zeta:lognormal_mean ~sigma:lognormal_sd ~x

  let density { lognormal_mean; lognormal_sd } ~x =
    Randist.lognormal_pdf ~zeta:lognormal_mean ~sigma:lognormal_sd x
  and quantile { lognormal_mean; lognormal_sd } ~p =
    if p < 0. || p > 1.
    then invalid_arg "LogNormal.quantile: p must be in range [0, 1]"
    else Cdf.lognormal_Pinv ~zeta:lognormal_mean ~sigma:lognormal_sd ~p

  let mean { lognormal_mean; lognormal_sd } =
    exp (lognormal_mean +. sqr lognormal_sd)
  and variance { lognormal_mean; lognormal_sd } =
    (exp (sqr lognormal_sd) -. 1.) *.
      exp (2. *. lognormal_mean +. sqr lognormal_sd)
  and skewness { lognormal_sd; _ } =
    let sd2 = sqr lognormal_sd in (exp sd2 -. 2.) *. sqrt (exp sd2 -. 1.)
  and kurtosis { lognormal_sd; _ } =
    let sd2 = sqr lognormal_sd in
    exp (4. *. sd2) +. 2. *. exp (3. *. sd2) +. 3. *. exp (2. *. sd2) -. 6.

  let generate ?(rng=default_rng) { lognormal_mean; lognormal_sd } =
    Randist.lognormal ~zeta:lognormal_mean ~sigma:lognormal_sd rng
  let sample = make_sampler generate

  let mle vs =
    let log_vs = Array.map ~f:log vs in
    let mean   = Sample.mean log_vs in
    let sd     = Sample.sd ~mean:mean log_vs in
    create ~mean ~sd
end

module Uniform = struct
  type elt = float
  type t   = {
    uniform_lower : float;
    uniform_upper : float
  }

  let create ~lower ~upper =
    if lower > upper
    then { uniform_lower = upper; uniform_upper = lower }
    else { uniform_lower = lower; uniform_upper = upper }

  let cumulative_probability { uniform_lower; uniform_upper } =
    Cdf.flat_P ~a:uniform_lower ~b:uniform_upper

  let density { uniform_lower; uniform_upper } ~x =
    Randist.flat_pdf ~a:uniform_lower ~b:uniform_upper x
  and quantile { uniform_lower; uniform_upper } ~p =
    if p < 0. || p > 1.
    then invalid_arg "Uniform.quantile: p must be in range [0, 1]"
    else Cdf.flat_Pinv ~a:uniform_lower ~b:uniform_upper ~p

  let mean { uniform_lower; uniform_upper } =
    0.5 *. (uniform_lower +. uniform_upper)
  and variance { uniform_lower; uniform_upper } =
    sqr (uniform_upper -. uniform_lower) /. 12.
  and skewness _d = 0.
  and kurtosis _d = -6. /. 5.

  let generate ?(rng=default_rng) { uniform_lower; uniform_upper } =
    Randist.flat ~a:uniform_lower ~b:uniform_upper rng
  let sample = make_sampler generate

  let mle vs = create ~lower:(Sample.min vs) ~upper:(Sample.max vs)
end

module Exponential = struct
  type elt = float
  type t   = { exp_rate : float }

  let create ~rate =
    if rate > 0.
    then { exp_rate = rate }
    else invalid_arg "Exponential.create: rate must be positive"

  let cumulative_probability { exp_rate } = Cdf.exponential_P ~mu:exp_rate

  let density { exp_rate } ~x =
    Randist.exponential_pdf ~mu:exp_rate x
  and quantile { exp_rate } ~p =
    if p < 0. || p > 1.
    then invalid_arg "Exponential.quantile: p must be in range [0, 1]"
    else Cdf.exponential_Pinv ~mu:exp_rate ~p

  let mean { exp_rate } = 1. /. exp_rate
  and variance { exp_rate } = 1. /. sqrt exp_rate
  and skewness _d = 2.
  and kurtosis _d = 6.

  let generate ?(rng=default_rng) { exp_rate } =
    Randist.exponential ~mu:exp_rate rng
  let sample = make_sampler generate

  let mle vs = create ~rate:(1. /. Sample.mean vs)
end

module ChiSquared = struct
  type elt = float
  type t   = { chisq_df : float }

  let create ~df =
    if df <= 0
    then invalid_arg "ChiSquared.create: degrees of freedom must be non negative"
    else { chisq_df = float_of_int df }

  let cumulative_probability { chisq_df } = Cdf.chisq_P ~nu:chisq_df

  let density { chisq_df } ~x = Randist.chisq_pdf ~nu:chisq_df x
  and quantile { chisq_df } ~p =
    if p < 0. || p > 1.
    then invalid_arg "ChiSquared.quantile: p must be in range [0, 1]"
    else Cdf.chisq_Pinv ~nu:chisq_df ~p

  let mean { chisq_df } = chisq_df
  and variance { chisq_df } = 2. *. chisq_df
  and skewness { chisq_df } = sqrt (8. /. chisq_df)
  and kurtosis { chisq_df } = 12. /. chisq_df

  let generate ?(rng=default_rng) { chisq_df } =
    Randist.chisq ~nu:chisq_df rng
  let sample = make_sampler generate

  let mme vs = create ~df:(round (Sample.mean vs))
end

module F = struct
  type elt = float
  type t   = {
    f_df1 : float;
    f_df2 : float
  }

  let create ~df1 ~df2 =
    if df1 <= 0 || df2 <= 0
    then invalid_arg "F.create: degrees of freedom must be non negative"
    else { f_df1 = float_of_int df1; f_df2 = float_of_int df2 }

  let cumulative_probability { f_df1; f_df2 } =
    Cdf.fdist_P ~nu1:f_df1 ~nu2:f_df2

  let density { f_df1; f_df2 } ~x =
    Randist.fdist_pdf ~nu1:f_df1 ~nu2:f_df2 x
  and quantile { f_df1; f_df2 } ~p =
    if p < 0. || p > 1.
    then invalid_arg "F.quantile: p must be in range [0, 1]"
    else Cdf.fdist_Pinv ~nu1:f_df1 ~nu2:f_df2 ~p

  let mean_opt { f_df2; _ } =
    if f_df2 <= 2.
    then None
    else Some (f_df2 /. (f_df2 -. 2.))
  and variance_opt { f_df1; f_df2 } =
    if f_df2 <= 4.
    then None
    else Some (2. *. sqr f_df2 *. (f_df1 +. f_df2 -. 2.) /.
          (f_df1 *. sqr (f_df2 -. 2.) *. (f_df2 -. 4.)))
  and skewness_opt { f_df1; f_df2 } =
    if f_df2 <= 6.
    then None
    else Some ((2. *. f_df1 +. f_df2 -. 2.) *. sqrt (8. *. (f_df2 -. 4.)) /.
           ((f_df2 -. 6.) *. sqrt (f_df1 *. (f_df1 +. f_df2 -. 2.))))
  and kurtosis_opt { f_df1; f_df2 } =
    if f_df2 <= 8.
    then None
    else Some (12. *.
                 (f_df1 *. (5. *. f_df2 -. 22.) *. (f_df1 +. f_df2 -. 2.) +.
                    (f_df2 -. 4.) *. sqr (f_df2 -. 2.)) /.
                 (f_df1 *. (f_df2 -. 6.) *. (f_df2 -. 8.) *.
                    (f_df1 +. f_df2 -. 2.)))

  let generate ?(rng=default_rng) { f_df1; f_df2 } =
    Randist.fdist ~nu1:f_df1 ~nu2:f_df2 rng
  let sample = make_sampler generate

  let mme vs =
    let mean     = Sample.mean vs in
    let variance = Sample.variance ~mean vs in
    let df1 =
      -2 * round (sqr mean /. (sqr mean *. (mean -. 1.) +.
                                 mean *. variance -. 2. *. variance))
    and df2 = 2 * round (mean /. (mean -. 1.)) in
    create ~df1 ~df2
end

module T = struct
  type elt = float
  type t   = { t_df : float }

  let create ~df =
    if df <= 0.
    then invalid_arg "T.create: degrees of freedom must be non negative"
    else { t_df = df }

  let cumulative_probability { t_df } = Cdf.tdist_P ~nu:t_df

  let density { t_df } ~x = Randist.tdist_pdf ~nu:t_df x
  and quantile { t_df } ~p =
    if p < 0. || p > 1.
    then invalid_arg "T.quantile: p must be in range [0, 1]"
    else Cdf.tdist_Pinv ~nu:t_df ~p

  let mean_opt { t_df } = if t_df > 0. then Some 0. else None
  and variance_opt { t_df } =
    if t_df > 2.
    then Some (t_df /. (t_df -. 2.))
    else if t_df > 1.
    then Some infinity
    else None
  and skewness_opt { t_df } =
    if t_df > 3.
    then Some 0.
    else None
  and kurtosis_opt { t_df } =
    if t_df > 4.
    then Some (6. /. (t_df -. 4.))
    else if t_df > 2.
    then Some infinity
    else None

  let generate ?(rng=default_rng) { t_df } = Randist.tdist ~nu:t_df rng
  let sample = make_sampler generate

  let mme vs =
    let variance = Sample.variance vs in
    if abs_float variance = infinity
    then invalid_arg "T.mme: infinite sample"
    else create ~df:(2. *. variance /. (variance -. 1.))
end

module Gamma = struct
  type elt = float
  type t   = {
    gamma_shape : float;
    gamma_scale : float
  }

  let create ~shape ~scale =
    if shape <= 0.
    then invalid_arg "Gamma.create: shape must be positive"
    else if scale <= 0.
    then invalid_arg "Gamma.create: scale must be positive"
    else { gamma_shape = shape; gamma_scale = scale }

  let cumulative_probability { gamma_shape; gamma_scale } =
    Cdf.gamma_P ~a:gamma_shape ~b:gamma_scale

  let density { gamma_shape; gamma_scale } ~x =
    Randist.gamma_pdf ~a:gamma_shape ~b:gamma_scale x
  and quantile { gamma_shape; gamma_scale } ~p =
    if p < 0. || p > 1.
    then invalid_arg "Gamma.quantile: p must be in range [0, 1]"
    else Cdf.gamma_Pinv ~a:gamma_shape ~b:gamma_scale ~p

  let mean { gamma_shape; gamma_scale } = gamma_shape *. gamma_scale
  and variance { gamma_shape; gamma_scale } = gamma_shape *. sqr gamma_scale
  and skewness { gamma_shape; _ } = 2. /. sqrt gamma_shape
  and kurtosis { gamma_shape; _ } = 6. /. gamma_shape

  let generate ?(rng=default_rng) { gamma_shape; gamma_scale } =
    Randist.gamma ~a:gamma_shape ~b:gamma_scale rng
  let sample = make_sampler generate

  let mme vs =
    let mean     = Sample.mean vs in
    let variance = Sample.variance ~mean vs in
    create ~shape:(sqr mean /. variance) ~scale:(mean /. variance)
end

module Cauchy = struct
  type elt = float
  type t   = {
    cauchy_location : float;
    cauchy_scale    : float
  }

  let create ~location ~scale =
    if scale <= 0.
    then invalid_arg "Cauchy.create: scale must be positive"
    else { cauchy_location = location; cauchy_scale = scale }

  let standard = create ~location:0. ~scale:1.

  let cumulative_probability { cauchy_location; cauchy_scale } ~x =
    Cdf.cauchy_P ~a:cauchy_scale ~x:(x -. cauchy_location)

  let density { cauchy_location; cauchy_scale } ~x =
    Randist.cauchy_pdf ~a:cauchy_scale (x -. cauchy_location)
  and quantile { cauchy_location; cauchy_scale } ~p =
    if p < 0. || p > 1.
    then invalid_arg "Cauchy.quantile: p must be in range [0, 1]"
    else Cdf.cauchy_Pinv ~a:cauchy_scale ~p +. cauchy_location

  let generate ?(rng=default_rng) { cauchy_location; cauchy_scale } =
    Randist.cauchy ~a:cauchy_scale rng +. cauchy_location
  let sample = make_sampler generate
end

module Beta = struct
  type elt = float
  type   t = {
    beta_alpha : float;
    beta_beta  : float
  }

  let create ~alpha ~beta =
    if alpha <= 0. || beta <= 0.
    then invalid_arg "Beta.create: shape parameters must be positive"
    else { beta_alpha = alpha; beta_beta = beta }

  let cumulative_probability { beta_alpha; beta_beta } =
    Cdf.beta_P ~a:beta_alpha ~b:beta_beta

  let density { beta_alpha; beta_beta } ~x =
    Randist.beta_pdf ~a:beta_alpha ~b:beta_beta x
  and quantile { beta_alpha; beta_beta } ~p =
    if p < 0. || p > 1.
    then invalid_arg "Beta.quantile: p must be in range [0, 1]"
    else Cdf.beta_Pinv ~a:beta_alpha ~b:beta_beta ~p

  let mean { beta_alpha = a; beta_beta = b } = a /. (a +. b)
  and variance { beta_alpha = a; beta_beta = b} =
    a *. b /. (sqr (a +. b) *. (a +. b +. 1.))
  and skewness { beta_alpha = a; beta_beta = b } =
    2. *. (b -. a) *. sqrt (a +. b +. 1.) /. ((a +. b +. 2.) *. sqrt (a *. b))
  and kurtosis { beta_alpha = a; beta_beta = b } =
    6. *. (sqr (a -. b) *. (a +. b +. 1.) -. a *. b *. (a +. b +. 2.)) /.
      (a *. b *. (a +. b +. 2.) *. (a +. b +. 3.))

  let generate ?(rng=default_rng) { beta_alpha; beta_beta } =
    Randist.beta ~a:beta_alpha ~b:beta_beta rng
  let sample = make_sampler generate

  let mme vs =
    let mean     = Sample.mean vs in
    let variance = Sample.variance ~mean vs in
    create ~alpha:(mean *. (sqr mean -. mean +. variance) /. variance)
      ~beta:((mean -. 1.) *. (sqr mean -. mean +. variance) /. variance)
end

module Logistic = struct
  type elt = float
  type t   = {
    logistic_location : float;
    logistic_scale    : float
  }

  let create ~location ~scale =
    if scale <= 0.
    then invalid_arg "Logistic.create: scale must be positive"
    else { logistic_location = location; logistic_scale = scale }

  let cumulative_probability { logistic_location; logistic_scale } ~x =
    Cdf.logistic_P ~a:logistic_scale ~x:(x -. logistic_location)

  let density { logistic_location; logistic_scale } ~x =
    Randist.logistic_pdf ~a:logistic_scale (x -. logistic_location)
  and quantile { logistic_location; logistic_scale } ~p =
    if p < 0. || p > 1.
    then invalid_arg "Logistic.quantile: p must be in range [0, 1]"
    else Cdf.logistic_Pinv ~a:logistic_scale ~p +. logistic_location

  let mean { logistic_location; _ } = logistic_location
  and variance { logistic_scale; _ } =
    sqr (logistic_scale *. Gsl.Math.pi) /. 3.
  and skewness _d = 0.
  and kurtosis _d = 1.2

  let generate ?(rng=default_rng) { logistic_location; logistic_scale } =
    Randist.logistic ~a:logistic_scale rng +. logistic_location
  let sample = make_sampler generate

  let mme vs =
    let mean     = Sample.mean vs in
    let variance = Sample.variance ~mean vs in
    let open Gsl.Math in
    create ~location:mean ~scale:(sqrt (3. *. variance) /. pi)
end

module Empirical = struct
  type t

  let create bins vs =
    ()
end


module Poisson = struct
  type elt = int
  type t   = { poisson_rate : float }

  let create ~rate =
    if rate > 0.
    then { poisson_rate = rate }
    else invalid_arg "Poisson.create: rate must be positive"

  let cumulative_probability { poisson_rate } ~n =
    Cdf.poisson_P ~mu:poisson_rate ~k:n

  let probability { poisson_rate } ~n =
    Randist.poisson_pdf ~mu:poisson_rate n

  let mean { poisson_rate } = poisson_rate
  and variance { poisson_rate } = poisson_rate
  and skewness { poisson_rate } = 1. /. sqrt poisson_rate
  and kurtosis { poisson_rate } = 1. /. poisson_rate

  let generate ?(rng=default_rng) { poisson_rate } =
    Randist.poisson ~mu:poisson_rate rng
  let sample = make_sampler generate

  let mle vs = create ~rate:(Sample.mean (Array.map ~f:float_of_int vs))
end

module Bernoulli = struct
  type elt = int
  type t   = { bernoulli_p : float }

  let create ~p =
    if p > 1.0 || p < 0.
    then invalid_arg "Bernoulli.create: probability must be in range [0, 1]"
    else { bernoulli_p = p }

  let cumulative_probability { bernoulli_p = p } ~n =
    if n < 0
    then 0.
    else if n < 1
    then 1. -. p
    else 1.

  let probability { bernoulli_p = p } ~n =
    if n = 0
    then 1. -. p
    else if n = 1
    then p
    else 0.

  let mean { bernoulli_p = p } = p
  and variance { bernoulli_p = p } = p *. (1. -. p)
  and skewness { bernoulli_p = p } =
    (1. -. 2. *. p) /. sqrt (p *. (1. -. p))
  and kurtosis { bernoulli_p = p } =
    (1. -. 6. *. p *. (1. -. p)) /. (p *. (1. -. p))

  let generate ?(rng=default_rng) { bernoulli_p } =
    Randist.bernoulli ~p:bernoulli_p rng
  let sample = make_sampler generate

  let mle vs =
    let n = Array.length vs
    and y = Array.fold_left ~f:(+) ~init:0 vs in
    create ~p:(float_of_int y /. float_of_int n)
end

module Binomial = struct
  type elt = int
  type t   = {
    binomial_trials : int;
    binomial_p      : float
  }

  let create ~trials ~p =
    if trials < 0
    then invalid_arg "Binomial.create: number of trials must be non negative"
    else if p > 1.0 || p < 0.
    then invalid_arg "Binomial.create: probability must be in range [0, 1]"
    else { binomial_trials = trials; binomial_p = p }

  let cumulative_probability { binomial_trials; binomial_p } ~n =
    Cdf.binomial_P ~n:binomial_trials ~p:binomial_p ~k:n

  let probability { binomial_trials; binomial_p } ~n =
    Randist.binomial_pdf ~n:binomial_trials ~p:binomial_p n

  let mean { binomial_trials = n; binomial_p = p } = float_of_int n *. p
  and variance { binomial_trials = n; binomial_p = p } =
    float_of_int n *. p *. (1. -. p)
  and skewness { binomial_trials = n; binomial_p = p } =
    (1. -. 2. *. p) /. sqrt (float_of_int n *. p *. (1. -. p))
  and kurtosis { binomial_trials = n; binomial_p = p } =
    (1. -. 6. *. p *. (1. -. p)) /. (float_of_int n *. p *. (1. -. p))

  let generate ?(rng=default_rng) { binomial_trials; binomial_p } =
    Randist.binomial ~n:binomial_trials ~p:binomial_p rng
  let sample = make_sampler generate

  let mme vs =
    let vs   = Array.map ~f:float_of_int vs in
    let mean = Sample.mean vs in
    let variance = Sample.variance ~mean vs in
    create ~trials:(round (sqr mean /. (mean -. variance)))
      ~p:(1. -. variance /. mean)
end

module Geometric = struct
  type elt = int
  type t   = { geometric_p : float }

  let create ~p =
    if p > 1.0 || p <= 0.
    then invalid_arg "Geometric.create: probability must be in range (0, 1]"
    else { geometric_p = p }

  let cumulative_probability { geometric_p } ~n =
    Cdf.geometric_P ~p:geometric_p ~k:n

  let probability { geometric_p } ~n =
    Randist.geometric_pdf ~p:geometric_p n

  let mean { geometric_p } = 1. /. geometric_p
  and variance { geometric_p } =
    (1. -. geometric_p) /. sqr geometric_p
  and skewness { geometric_p } =
    (2. -. geometric_p) /. sqrt (1. -. geometric_p)
  and kurtosis { geometric_p } =
    6. +. sqr geometric_p /. (1. -. geometric_p)

  let generate ?(rng=default_rng) { geometric_p } =
    Randist.geometric ~p:geometric_p rng
  let sample = make_sampler generate

  let mme vs =
    let n = Array.length vs in
    let p = float_of_int n /.
              float_of_int (Array.fold_left ~f:(+) ~init:0 vs)
    in create ~p
end

module Hypergeometric = struct
  type elt = int
  type t   = {
    hyper_m : int;
    hyper_t : int;
    hyper_k : int
  }

  let create ~m ~t ~k =
    if t < 0
    then invalid_arg "Hypergeometric.create: t must be non negative"
    else if m < 0  || m > t
    then invalid_arg "Hypergeometric.create: m must be in range [0, t]"
    else if k <= 0 || k > t
    then invalid_arg "Hypergeometric.create: k must be in range (0, t]"
    else { hyper_m = m; hyper_t = t; hyper_k = k }

  let cumulative_probability { hyper_m; hyper_t; hyper_k } ~n =
    Cdf.hypergeometric_P ~n1:hyper_m ~n2:(hyper_t - hyper_m) ~t:hyper_k ~k:n

  let probability { hyper_m; hyper_t; hyper_k } ~n =
    Randist.hypergeometric_pdf ~n1:hyper_m ~n2:(hyper_t - hyper_m) ~t:hyper_k n

  let mean { hyper_m; hyper_t; hyper_k } =
    float_of_int (hyper_k * hyper_m) /. float_of_int hyper_t
  and variance { hyper_m; hyper_t; hyper_k } =
    let m = float_of_int hyper_m
    and t = float_of_int hyper_t
    and k = float_of_int hyper_k
    in (k *. m /. t) *. (1. -. m /. t) *. (t -. k) /. (t -. 1.)
  and skewness { hyper_m; hyper_t; hyper_k } =
    let m = float_of_int hyper_m
    and t = float_of_int hyper_t
    and k = float_of_int hyper_k
    in (t -. 2. *. m) *. sqrt (t -. 1.) *. (t -. 2. *. k) /.
         (sqrt (k *. m *. (t -. m) *. (t -. k)) *. (t -. 2.))
  and kurtosis { hyper_m; hyper_t; hyper_k } =
    let m = float_of_int hyper_m
    and t = float_of_int hyper_t
    and k = float_of_int hyper_k
    in ((t -. 1.) *. sqr t *.
          (t *. (t +. 1.) -.
             6. *. m *. (t -. m) -.
             6. *. k *. (t -. k)) +.
          6. *. k *. m *. (t -. m) *. (t -. k) *. (5. *. t -. 6.)) /.
         (k *. m *. (t -. m) *. (t -. k) *. (t -. 2.) *. (t -. 3.))

  let generate ?(rng=default_rng) { hyper_m; hyper_t; hyper_k } =
    Randist.hypergeometric ~n1:hyper_m ~n2:(hyper_t - hyper_m) ~t:hyper_k rng
  let sample = make_sampler generate
end

module NegativeBinomial = struct
  type elt = int
  type t   = {
    nbinomial_failures : int;
    nbinomial_p        : float
  }

  let create ~failures ~p =
    if failures < 0
    then invalid_arg ("NegativeBinomial.create: number of failures must " ^
                      "be non negative")
    else if p >= 1.0 || p <= 0.
    then invalid_arg "NegativeBinomial.create: probability must be in range (0, 1)"
    else { nbinomial_failures = failures; nbinomial_p = p }

  let cumulative_probability { nbinomial_failures; nbinomial_p } ~n =
    Cdf.negative_binomial_P
      ~n:(float_of_int nbinomial_failures) ~p:nbinomial_p ~k:n

  let probability { nbinomial_failures; nbinomial_p } ~n =
    Randist.negative_binomial_pdf
      ~n:(float_of_int nbinomial_failures) ~p:nbinomial_p n

  let mean { nbinomial_failures = r; nbinomial_p = p } =
    float_of_int r *. p /. (1. -. p)
  and variance { nbinomial_failures = r; nbinomial_p = p } =
    float_of_int r *. p *. sqr (1. -. p)
  and skewness { nbinomial_failures = r; nbinomial_p = p } =
    (1. +. p) /. sqrt (float_of_int r *. p)
  and kurtosis { nbinomial_failures = r; nbinomial_p = p } =
    6. /. float_of_int r +. sqr (1. +. p) /. (float_of_int r *. p)

  let generate ?(rng=default_rng) { nbinomial_failures; nbinomial_p } =
    Randist.negative_binomial
      ~n:(float_of_int nbinomial_failures) ~p:nbinomial_p rng
  let sample = make_sampler generate

  let mme vs =
    let vs       = Array.map ~f:float_of_int vs in
    let mean     = Sample.mean vs in
    let variance = Sample.variance ~mean vs in
    create ~failures:(round (sqr mean /. (variance -. mean)))
      ~p:(1. -. mean /. variance)
end

module Categorical = struct
  module type OrderedType = Map.OrderedType

  module type S = sig
    include DiscreteDistribution

    val create : (elt * float) array -> t

    val mle : elt array -> t
  end

  module Make (Elt : OrderedType) = struct
    type elt = Elt.t
    type t   = {
      categorical_values : elt array;
      categorical_probs  : float array;
      categorical_cumsum : Randist.discrete
    }

    let create dist =
      let n  = Array.length dist
      and is =
        Array.sort_index dist
          ~cmp:(fun (v1, _p1) (v2, _p2) -> Elt.compare v1 v2)
      in if n = 0 then invalid_arg "Categorical.Make: no data";

      let (v0, p0) = Array.(unsafe_get dist (unsafe_get is 0)) in
      let vs       = Array.make n v0
      and probs    = Array.make n p0 in begin
        for i = 1 to n - 1 do
          let (v, p) = Array.(unsafe_get dist (unsafe_get is i)) in
          Array.unsafe_set vs i v;
          Array.unsafe_set probs i p
        done;

        (* Note(superbobry): ideally, we should check that given
           probabilities sum up to 1., but I don't see how to do this
           for floating point numbers. *)
        {
          categorical_values = vs;
          categorical_probs  = probs;
          categorical_cumsum = Randist.discrete_preproc probs
        }
      end

    let cumulative_probability { categorical_values; categorical_probs; _ } ~n =
      match Base.search_sorted ~cmp:Elt.compare categorical_values n with
        | Some pos ->
          let acc = ref 0. in begin
            for i = 0 to pos do
              acc := !acc +. Array.unsafe_get categorical_probs i
            done
          end; !acc
        | None ->
          if Elt.compare n (Array.unsafe_get categorical_values 0) < 0
          then 0.
          else
            (* Then it must be the case that forall vs : v > n. *)
            1.

    let probability { categorical_values; categorical_probs; _ } ~n =
      match Base.search_sorted ~cmp:Elt.compare categorical_values n with
        | Some pos -> Array.unsafe_get categorical_probs pos
        | None     -> 0.

    let generate ?(rng=default_rng)
        { categorical_values; categorical_cumsum; _ } =
      let pos = Randist.discrete rng categorical_cumsum in
      Array.unsafe_get categorical_values pos
    let sample = make_sampler generate

    let mle vs =
      let n = Array.length vs in
      if n = 0
      then invalid_arg "Categorical.Make.mle: no data"
      else
        let counts = Hashtbl.create 8 in begin
          for i = 0 to n - 1 do
            let v = Array.unsafe_get vs i in
            if Hashtbl.mem counts v
            then Hashtbl.replace counts v (Hashtbl.find counts v + 1)
            else Hashtbl.add counts v 1
          done;

          let dist = Hashtbl.fold (fun k v dist ->
              ((k, float_of_int v /. float_of_int n) :: dist)) counts []
          in create (Array.of_list dist)
        end
  end
end


let normal = Normal.create
and log_normal = LogNormal.create
and uniform = Uniform.create
and exponential = Exponential.create
and chi_squared = ChiSquared.create
and f = F.create
and t = T.create
and gamma = Gamma.create
and cauchy = Cauchy.create
and beta = Beta.create
and logistic = Logistic.create

let poisson = Poisson.create
and bernoulli = Bernoulli.create
and binomial = Binomial.create
and geometric = Geometric.create
and hypergeometric = Hypergeometric.create
and negative_binomial = NegativeBinomial.create
