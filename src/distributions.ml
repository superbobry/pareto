open Internal

module Gaussian = struct
  type t = {
    normal_mean : float;
    normal_sd   : float;
  }

  let create ~mean ~sd =
    if sd > 0.
    then { normal_mean = mean; normal_sd = sd }
    else failwith "Gaussian.create: standard deviation must be positive"

  let standard = create ~mean:0. ~sd:1.

  let cumulative_probability { normal_mean; normal_sd } ~x =
    Cdf.gaussian_P ~sigma:normal_sd ~x:(x -. normal_mean)

  let density { normal_mean; normal_sd } ~x =
    Randist.gaussian_pdf ~sigma:normal_sd (x -. normal_mean)
  and quantile { normal_mean; normal_sd } ~p =
    Cdf.gaussian_Pinv ~sigma:normal_sd ~p +. normal_mean

  let mean { normal_mean; _ } = normal_mean
  and variance { normal_sd; _ } = normal_sd *. normal_sd
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
  and quantile { uniform_lower; uniform_upper } =
    Cdf.flat_Pinv ~a:uniform_lower ~b:uniform_upper

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
  and quantile { exp_rate } =
    Cdf.exponential_Pinv ~mu:exp_rate

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

  let mean { binomial_trials; binomial_p } =
    float_of_int binomial_trials *. binomial_p
  and variance { binomial_trials; binomial_p } =
    float_of_int binomial_trials *. binomial_p *. (1. -. binomial_p)
end
