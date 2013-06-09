open Internal

module Randist = Gsl.Randist
module Cdf = Gsl.Cdf


module type Mean = sig
  type t

  val mean : t -> float
end

module type MeanOpt = sig
  type t

  val mean_opt : t -> float option
end

module type Variance = sig
  type t

  val variance : t -> float
end

module type VarianceOpt = sig
  type t

  val variance_opt : t -> float option
end

module type BaseDistribution = sig
  type t
  type elt

  val generate : ?rng:Rng.t -> t -> elt
  val sample   : ?rng:Rng.t -> size:int -> t -> elt array
end

module type DiscreteDistribution = sig
  include BaseDistribution with type elt := int

  val cumulative_probability : t -> n:int -> float
  val probability : t -> n:int -> float
end

module type ContinuousDistribution = sig
  include BaseDistribution with type elt := float

  val cumulative_probability : t -> x:float -> float
  val density  : t -> x:float -> float
  val quantile : t -> p:float -> float
end


let make_sampler (generate : ?rng:Rng.t -> 'a -> 'b) =
  fun ?(rng=default_rng) ~size d ->
    let rec go acc = function
    | 0 -> Array.of_list acc
    | i -> go (generate ~rng d :: acc) (i - 1)
    in go [] size


module Normal = struct
  type t = {
    normal_mean : float;
    normal_sd   : float;
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

  let generate ?(rng=default_rng) { normal_mean; normal_sd } =
    Randist.gaussian ~sigma:normal_sd rng +. normal_mean
  let sample = make_sampler generate
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
    then invalid_arg "Uniform.quantile: p must be in range [0, 1]"
    else Cdf.flat_Pinv ~a:uniform_lower ~b:uniform_upper ~p

  let mean { uniform_lower; uniform_upper } =
    0.5 *. (uniform_lower +. uniform_upper)
  and variance { uniform_lower; uniform_upper } =
    sqr (uniform_upper -. uniform_lower) /. 12.

  let generate ?(rng=default_rng) { uniform_lower; uniform_upper } =
    Randist.flat ~a:uniform_lower ~b:uniform_upper rng
  let sample = make_sampler generate
end

module Exponential = struct
  type t = { exp_rate : float }

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

  let generate ?(rng=default_rng) { exp_rate } =
    Randist.exponential ~mu:exp_rate rng
  let sample = make_sampler generate
end

module Poisson = struct
  type t = { poisson_rate : float }

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

  let generate ?(rng=default_rng) { poisson_rate } =
    Randist.poisson ~mu:poisson_rate rng
  let sample = make_sampler generate
end

module Binomial = struct
  type t = {
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

  let generate ?(rng=default_rng) { binomial_trials; binomial_p } =
    Randist.binomial ~n:binomial_trials ~p:binomial_p rng
  let sample = make_sampler generate
end

module ChiSquared = struct
  type t = { chisq_df : float }

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

  let generate ?(rng=default_rng) { chisq_df } =
    Randist.chisq ~nu:chisq_df rng
  let sample = make_sampler generate
end

module F = struct
  type t = {
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
    if f_df2 < 2.
    then None
    else Some (f_df2 /. (f_df2 -. 2.))
  and variance_opt { f_df1; f_df2 } =
    if f_df2 < 4.
    then None
    else Some (2. *. sqr f_df2 *. (f_df1 +. f_df2 -. 2.) /.
          (f_df1 *. sqr (f_df2 -. 2.) *. (f_df2 -. 4.)))

  let generate ?(rng=default_rng) { f_df1; f_df2 } =
    Randist.fdist ~nu1:f_df1 ~nu2:f_df2 rng
  let sample = make_sampler generate
end

module T = struct
  type t = { t_df : float }

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

  let generate ?(rng=default_rng) { t_df } = Randist.tdist ~nu:t_df rng
  let sample = make_sampler generate
end

module Gamma = struct
  type t = {
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

  let generate ?(rng=default_rng) { gamma_shape; gamma_scale } =
    Randist.gamma ~a:gamma_shape ~b:gamma_scale rng
  let sample = make_sampler generate
end

module Cauchy = struct
  type t = {
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
  type t = {
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

  let mean { beta_alpha; beta_beta } =
    beta_alpha /. (beta_alpha +. beta_beta)
  and variance { beta_alpha; beta_beta } =
    beta_alpha *. beta_beta /.
      (sqr (beta_alpha +. beta_beta) *. (beta_alpha +. beta_beta +. 1.))

  let generate ?(rng=default_rng) { beta_alpha; beta_beta } =
    Randist.beta ~a:beta_alpha ~b:beta_beta rng
  let sample = make_sampler generate
end

module Geometric = struct
  type t = { geometric_p : float }

  let create ~p =
    if p > 1.0 || p <= 0.
    then invalid_arg "Geometric.create: probability must be in range (0, 1]"
    else { geometric_p = p }

  let cumulative_probability { geometric_p } ~n =
    Cdf.geometric_P ~p:geometric_p ~k:n

  let probability { geometric_p } ~n =
    Randist.geometric_pdf ~p:geometric_p n

  let mean { geometric_p } = 1. /. geometric_p
  and variance { geometric_p } = (1. -. geometric_p) /. sqr geometric_p

  let generate ?(rng=default_rng) { geometric_p } =
    Randist.geometric ~p:geometric_p rng
  let sample = make_sampler generate
end

module Hypergeometric = struct
  type t = {
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

  (** FIXME(superbobry): remove this once gsl-1.14.0 is out. *)
  external hypergeometric_P
    : k:int -> n1:int -> n2:int -> t:int -> float
    = "ml_gsl_cdf_hypergeometric_P"

  let cumulative_probability { hyper_m; hyper_t; hyper_k } ~n =
    hypergeometric_P ~n1:hyper_m ~n2:(hyper_t - hyper_m) ~t:hyper_k ~k:n

  let probability { hyper_m; hyper_t; hyper_k } ~n =
    Randist.hypergeometric_pdf ~n1:hyper_m ~n2:(hyper_t - hyper_m) ~t:hyper_k n

  let mean { hyper_m; hyper_t; hyper_k } =
    float_of_int (hyper_k * hyper_m) /. float_of_int hyper_t
  and variance { hyper_m; hyper_t; hyper_k } =
    let m = float_of_int hyper_m
    and t = float_of_int hyper_t
    and k = float_of_int hyper_k
    in (k *. m /. t) *. (1. -. m /. t) *. (t -. k) /. (t -. 1.)

  let generate ?(rng=default_rng) { hyper_m; hyper_t; hyper_k } =
    Randist.hypergeometric ~n1:hyper_m ~n2:(hyper_t - hyper_m) ~t:hyper_k rng
  let sample = make_sampler generate
end

module NegativeBinomial = struct
  type t = {
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
    (** Interface inconsistency reported, see
        https://bitbucket.org/mmottl/gsl-ocaml/issue/5 for details. *)
    Cdf.negative_binomial_P ~n:(float_of_int nbinomial_failures)
      ~p:nbinomial_p ~k:n

  let probability { nbinomial_failures; nbinomial_p } ~n =
    Randist.negative_binomial_pdf ~n:nbinomial_failures ~p:nbinomial_p n

  let mean { nbinomial_failures = r; nbinomial_p = p } =
    float_of_int r *. p /. (1. -. p)
  and variance { nbinomial_failures = r; nbinomial_p = p } =
    float_of_int r *. p *. sqr (1. -. p)

  let generate ?(rng=default_rng) { nbinomial_failures; nbinomial_p } =
    Randist.negative_binomial ~n:nbinomial_failures ~p:nbinomial_p rng
  let sample = make_sampler generate
end


let normal = Normal.create
let uniform = Uniform.create
let exponential = Exponential.create
let poisson = Poisson.create
let binomial = Binomial.create
let chi_squared = ChiSquared.create
let f = F.create
let t = T.create
let gamma = Gamma.create
let cauchy = Cauchy.create
let beta = Beta.create
let geometric = Geometric.create
let hypergeometric = Hypergeometric.create
let negative_binomial = NegativeBinomial.create
