open OUnit

let assert_almost_equal ?(epsilon=1e-6) =
  assert_equal ~cmp:(cmp_float ~epsilon) ~printer:(Printf.sprintf "%.10f")

let cmp_array ~cmp v1 v2 =
  let n   = min (Array.length v1) (Array.length v2)
  and res = ref true in
  for i = 0 to n - 1 do
    res := !res && cmp (Array.unsafe_get v1 i) (Array.unsafe_get v2 i)
  done; !res

let printer_array ~printer vs =
  let inner = String.concat ", " Array.(to_list (map printer vs)) in
  Printf.sprintf "[%s]" inner
