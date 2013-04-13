open Internal

type test_alternative = Less | Greater | TwoSided


module T = struct
  let finish d t alternative =
    let open Distributions.T in
    let pvalue = match alternative with
    | Less     -> cumulative_probability d ~x:t
    | Greater  -> 1. -. cumulative_probability d ~x:t
    | TwoSided -> 2. *. cumulative_probability d ~x:(-. (abs_float t))
    in (t, pvalue)

  let one_sample v ?(mean=0.) ?(alternative=TwoSided) () =
    let n = float_of_int (Array.length v) in
    let t = (Sample.mean v -. mean) *. sqrt (n /. Sample.variance v)
    in finish (Distributions.T.create ~df:(n -. 1.)) t alternative

  let two_sample_independent v1 v2
      ?(equal_variance=true) ?(mean=0.) ?(alternative=TwoSided) () =
    let n1 = float_of_int (Array.length v1)
    and n2 = float_of_int (Array.length v2)
    and (var1, var2) = (Sample.variance v1, Sample.variance v2) in

    let (df, denom) = if equal_variance
      then
        let df = n1 +. n2 -. 2. in
        let var12 = ((n1 -. 1.) *. var1 +. (n2 -. 1.) *. var2) /. df
        in (df, sqrt (var12 *. (1. /. n1 +. 1. /. n2)))
      else
        let vn1 = var1 /. n1 in
        let vn2 = var2 /. n2 in
        let df  =
          sqr (vn1 +. vn2) /. (sqr vn1 /. (n1 -. 1.) +. sqr vn2 /. (n2 -. 1.))
        in (df, sqrt (vn1 +. vn2))
    in

    let t = (Sample.mean v1 -. Sample.mean v2 -. mean) /. denom in
    finish (Distributions.T.create ~df) t alternative

  let two_sample_related v1 v2 ?(mean=0.) ?(alternative=TwoSided) () =
    let n = Array.length v1 in
    if n <> Array.length v2
    then invalid_arg "T.two_sample_related: unequal length arrays";
    one_sample (Array.mapi (fun i x -> x -. v2.(i)) v1) ~mean ~alternative ()
end
