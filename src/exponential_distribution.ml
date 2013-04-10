open Internal

type t = { exp_lambda : float }

let create ~lambda =
  if lambda > 0.
  then { exp_lambda = lambda }
  else failwith "Exponential_distribution.create: rate must be positive"

let cumulative_probability { exp_lambda } = Cdf.exponential_P ~mu:exp_lambda

let density { exp_lambda } ~x =
  Randist.exponential_pdf ~mu:exp_lambda x
and quantile { exp_lambda } =
  Cdf.exponential_Pinv ~mu:exp_lambda

let mean { exp_lambda } = 1. /. exp_lambda
and variance { exp_lambda } = 1. /. sqrt exp_lambda
