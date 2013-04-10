type t = {
  uniform_lower : float;
  uniform_upper : float
}

let create ~lower ~upper =
  if lower > upper
  then { uniform_lower = upper; uniform_upper = lower }
  else { uniform_lower = lower; uniform_upper = upper }

let cumulative_probability { uniform_lower; uniform_upper } ~x =
  if x < uniform_lower
  then 0.
  else if x > uniform_upper
  then 1.
  else (x -. uniform_lower) /. (uniform_upper -. uniform_lower)

let density { uniform_lower; uniform_upper } ~x =
  if x < uniform_lower || x > uniform_upper
  then 0.
  else 1. /. (uniform_upper -. uniform_lower)

let mean { uniform_lower; uniform_upper } = 0.5 *. (uniform_lower +. uniform_upper)
and variance { uniform_lower; uniform_upper } =
  (uniform_upper -. uniform_lower) *. (uniform_upper -. uniform_lower) /. 12.
