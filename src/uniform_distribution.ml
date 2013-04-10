open Internal

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
