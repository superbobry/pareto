open Internal

module Gaussian = struct
  type t = {
    gaussian_mean : float;
    gaussian_sd   : float;
  }

  let create ~mean ~sd =
    if sd > 0.
    then { gaussian_mean = mean; gaussian_sd = sd }
    else failwith "Gaussian.create: standard deviation must be positive"

  let standard = create ~mean:0. ~sd:1.

  let cumulative_probability { gaussian_mean; gaussian_sd } ~x =
    Cdf.gaussian_P ~sigma:gaussian_sd ~x:(x -. gaussian_mean)

  let density { gaussian_mean; gaussian_sd } ~x =
    Randist.gaussian_pdf ~sigma:gaussian_sd (x -. gaussian_mean)
  and quantile { gaussian_mean; gaussian_sd } ~p =
    if p < 0. || p > 1.
    then failwith "Gaussian.quantile: p must be in range [0, 1]"
    else Cdf.gaussian_Pinv ~sigma:gaussian_sd ~p +. gaussian_mean

  let mean { gaussian_mean; _ } = gaussian_mean
  and variance { gaussian_sd; _ } = gaussian_sd *. gaussian_sd
end

module Uniform = struct
  type t = {
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
    then failwith "Uniform.quantile: p must be in range [0, 1]"
    else Cdf.flat_Pinv ~a:uniform_lower ~b:uniform_upper ~p

  let mean { uniform_lower; uniform_upper } = 0.5 *. (uniform_lower +. uniform_upper)
  and variance { uniform_lower; uniform_upper } =
    (uniform_upper -. uniform_lower) *. (uniform_upper -. uniform_lower) /. 12.
end

module Exponential = struct
  type t = { exp_rate : float }

  let create ~rate =
    if rate > 0.
    then { exp_rate = rate }
    else failwith "Exponential.create: rate must be positive"

  let cumulative_probability { exp_rate } = Cdf.exponential_P ~mu:exp_rate

  let density { exp_rate } ~x =
    Randist.exponential_pdf ~mu:exp_rate x
  and quantile { exp_rate } ~p =
    if p < 0. || p > 1.
    then failwith "Exponential.quantile: p must be in range [0, 1]"
    else Cdf.exponential_Pinv ~mu:exp_rate ~p

  let mean { exp_rate } = 1. /. exp_rate
  and variance { exp_rate } = 1. /. sqrt exp_rate
end

module Poisson = struct
  type t = { poisson_rate : float }

  let create ~rate =
    if rate > 0.
    then { poisson_rate = rate }
    else failwith "Poisson.create: rate must be positive"

  let cumulative_probability { poisson_rate } =
    Cdf.poisson_P ~mu:poisson_rate

  let probability { poisson_rate } ~k =
    Randist.poisson_pdf ~mu:poisson_rate k

  let mean { poisson_rate } = poisson_rate
  and variance { poisson_rate } = poisson_rate
end

module Binomial = struct
  type t = {
    binomial_trials : int;
    binomial_p      : float
  }

  let create ~trials ~p =
    if trials < 0
    then failwith "Binomial.create: number of trials must be non negative"
    else if p > 1.0 || p < 0.
    then failwith "Binomial.create: probability must be in range [0, 1]"
    else { binomial_trials = trials; binomial_p = p }

  let cumulative_probability { binomial_trials; binomial_p } =
    Cdf.binomial_P ~n:binomial_trials ~p:binomial_p

  let probability { binomial_trials; binomial_p } ~k =
    Randist.binomial_pdf ~n:binomial_trials ~p:binomial_p k

  let mean { binomial_trials = n; binomial_p = p } = float_of_int n *. p
  and variance { binomial_trials = n; binomial_p = p } =
    float_of_int n *. p *. (1. -. p)
end

module ChiSquared = struct
  type t = { chisq_df : float }

  let create ~df =
    if df <= 0
    then failwith "ChiSquared.create: degrees of freedom must be non negative"
    else { chisq_df = float_of_int df }

  let cumulative_probability { chisq_df } = Cdf.chisq_P ~nu:chisq_df

  let density { chisq_df } ~x = Randist.chisq_pdf ~nu:chisq_df x
  and quantile { chisq_df } ~p =
    if p < 0. || p > 1.
    then failwith "ChiSquared.quantile: p must be in range [0, 1]"
    else Cdf.chisq_Pinv ~nu:chisq_df ~p

  let mean { chisq_df } = chisq_df
  and variance { chisq_df } = 2. *. chisq_df
end

module F = struct
  type t = {
    f_df1 : float;
    f_df2 : float
  }

  let create ~df1 ~df2 =
    if df1 <= 0 || df2 <= 0
    then failwith "F.create: degrees of freedom must be non negative"
    else { f_df1 = float_of_int df1; f_df2 = float_of_int df2 }

  let cumulative_probability { f_df1; f_df2 } =
    Cdf.fdist_P ~nu1:f_df1 ~nu2:f_df2

  let density { f_df1; f_df2 } ~x =
    Randist.fdist_pdf ~nu1:f_df1 ~nu2:f_df2 x
  and quantile { f_df1; f_df2 } ~p =
    if p < 0. || p > 1.
    then failwith "F.quantile: p must be in range [0, 1]"
    else Cdf.fdist_Pinv ~nu1:f_df1 ~nu2:f_df2 ~p

  let mean_opt { f_df2; _ } =
    if f_df2 < 2.
    then None
    else Some (f_df2 /. (f_df2 -. 2.))
  and variance_opt { f_df1; f_df2 } =
    if f_df2 < 4.
    then None
    else Some (2. *. f_df2 *. f_df2 *. (f_df1 +. f_df2 -. 2.) /.
          (f_df1 *. (f_df2 -. 2.) *. (f_df2 -. 2.) *. (f_df2 -. 4.)))
end

module T = struct
  type t = { t_df : float }

  let create ~df =
    if df <= 0.
    then failwith "T.create: degrees of freedom must be non negative"
    else { t_df = df }

  let cumulative_probability { t_df } = Cdf.tdist_P ~nu:t_df

  let density { t_df } ~x = Randist.tdist_pdf ~nu:t_df x
  and quantile { t_df } ~p =
    if p < 0. || p > 1.
    then failwith "T.quantile: p must be in range [0, 1]"
    else Cdf.tdist_Pinv ~nu:t_df ~p

  let mean_opt { t_df } = if t_df > 0. then Some 0. else None
  and variance_opt { t_df } =
    if t_df > 2.
    then Some (t_df /. (t_df -. 2.))
    else if t_df > 1.
    then Some infinity
    else None
end

module Gamma = struct
  type t = {
    gamma_shape : float;
    gamma_scale : float
  }

  let create ~shape ~scale =
    if shape <= 0.
    then failwith "Gamma.create: shape must be positive"
    else if scale <= 0.
    then failwith "Gamma.create: scale must be positive"
    else { gamma_shape = shape; gamma_scale = scale }

  let cumulative_probability { gamma_shape; gamma_scale } =
    Cdf.gamma_P ~a:gamma_shape ~b:gamma_scale

  let density { gamma_shape; gamma_scale } ~x =
    Randist.gamma_pdf ~a:gamma_shape ~b:gamma_scale x
  and quantile { gamma_shape; gamma_scale } ~p =
    if p < 0. || p > 1.
    then failwith "Gamma.quantile: p must be in range [0, 1]"
    else Cdf.gamma_Pinv ~a:gamma_shape ~b:gamma_scale ~p

  let mean { gamma_shape; gamma_scale } = gamma_shape *. gamma_scale
  and variance { gamma_shape; gamma_scale } =
    gamma_shape *. gamma_scale *. gamma_scale
end

module Cauchy = struct
  type t = {
    cauchy_location : float;
    cauchy_scale    : float
  }

  let create ~location ~scale =
    if scale <= 0.
    then failwith "Cauchy.create: scale must be positive"
    else { cauchy_location = location; cauchy_scale = scale }

  let standard = create ~location:0. ~scale:1.

  let cumulative_probability { cauchy_location; cauchy_scale } ~x =
    Cdf.cauchy_P ~a:cauchy_scale ~x:(x -. cauchy_location)

  let density { cauchy_location; cauchy_scale } ~x =
    Randist.cauchy_pdf ~a:cauchy_scale (x -. cauchy_location)
  and quantile { cauchy_location; cauchy_scale } ~p =
    if p < 0. || p > 1.
    then failwith "Cauchy.quantile: p must be in range [0, 1]"
    else Cdf.cauchy_Pinv ~a:cauchy_scale ~p +. cauchy_location

  let mean_opt _d = None
  and variance_opt _d = None
end

module Beta = struct
  type t = {
    beta_alpha : float;
    beta_beta  : float
  }

  let create ~alpha ~beta =
    if alpha <= 0. || beta <= 0.
    then failwith "Beta.create: shape parameters must be positive"
    else { beta_alpha = alpha; beta_beta = beta }

  let cumulative_probability { beta_alpha; beta_beta } =
    Cdf.beta_P ~a:beta_alpha ~b:beta_beta

  let density { beta_alpha; beta_beta } ~x =
    Randist.beta_pdf ~a:beta_alpha ~b:beta_beta x
  and quantile { beta_alpha; beta_beta } ~p =
    if p < 0. || p > 1.
    then failwith "Beta.quantile: p must be in range [0, 1]"
    else Cdf.beta_Pinv ~a:beta_alpha ~b:beta_beta ~p

  let mean { beta_alpha; beta_beta } =
    beta_alpha /. (beta_alpha +. beta_beta)
  and variance { beta_alpha; beta_beta } =
    beta_alpha *. beta_beta /.
      ((beta_alpha +. beta_beta) *. (beta_alpha +. beta_beta) *.
         (beta_alpha +. beta_beta +. 1.))
end
