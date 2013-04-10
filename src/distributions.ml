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

  let mean { chisq_df; _ } = chisq_df
  and variance { chisq_df; _ } = 2. *. chisq_df
end
