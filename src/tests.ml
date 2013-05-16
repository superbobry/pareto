open Internal

type test_alternative = Less | Greater | TwoSided


module T = struct
  let finalize d t alternative =
    let open Distributions.T in
    let pvalue = match alternative with
    | Less     -> cumulative_probability d ~x:t
    | Greater  -> 1. -. cumulative_probability d ~x:t
    | TwoSided -> 2. *. cumulative_probability d ~x:(-. (abs_float t))
    in (t, pvalue)

  let one_sample v ?(mean=0.) ?(alternative=TwoSided) () =
    let n = float_of_int (Array.length v) in
    let t = (Sample.mean v -. mean) *. sqrt (n /. Sample.variance v)
    in finalize (Distributions.T.create ~df:(n -. 1.)) t alternative

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
    finalize (Distributions.T.create ~df) t alternative

  let two_sample_related v1 v2 ?(mean=0.) ?(alternative=TwoSided) () =
    let n = Array.length v1 in
    if n <> Array.length v2
    then invalid_arg "T.two_sample_related: unequal length arrays";
    one_sample (Array.mapi (fun i x -> x -. v2.(i)) v1) ~mean ~alternative ()
end

module ChiSquared = struct
  let finalize d chisq =
    let open Distributions.ChiSquared in
    (chisq, 1. -. cumulative_probability ~x:chisq d)

  let goodness_of_fit observed ?(expected=[||]) ?(df=0) () =
    let n = Array.length observed in
    let k = Array.length expected in
    let expected =
      if k = 0
      then Array.make n (Array.sum observed /. float_of_int n)
      else if k != n
      then invalid_arg "ChiSquared.goodness_of_fit: unequal length arrays"
      else
        (* TODO(superbobry): make sure we have wellformed frequencies. *)
        expected
    in

    let chisq = ref 0. in
    for i = 0 to n - 1 do
      chisq := !chisq +. sqr (observed.(i) -. expected.(i)) /. expected.(i)
    done;

    finalize (Distributions.ChiSquared.create ~df:(n - 1 - df)) !chisq

  let independence observed ?(correction=false) () =
    let observed = Matrix.of_arrays observed in
    let (m, n) = Matrix.dims observed in
    if m = 0 || n = 0 then invalid_arg "ChiSquared.independence: no data"
    else if Matrix.exists (fun x -> x < 0.) observed
    then invalid_arg ("ChiSquared.independence: observed values must " ^
                      "be non negative");

    let expected = Matrix.create m n in
    let open Gsl.Blas_flat in
    gemm ~ta:Trans ~tb:NoTrans ~alpha:(1. /. Matrix.sum observed) ~beta:1.
      ~a:(Matrix.sum_by `Rows observed)
      ~b:(Matrix.sum_by `Columns observed)
      ~c:expected;

    if Matrix.exists ((=) 0.) expected
    then invalid_arg ("ChiSquared.independence: computed expected " ^
                      " frequencies matrix has a zero element");

    match (m - 1) * (n - 1) with
    | 0  ->
      (* This degenerate case is shamelessly ripped of from SciPy
        'chi2_contingency' function. *)
      (0., 1.)
    | df ->
      let chisq =
        let open Matrix in
        let t = create m n in begin
          memcpy ~src:expected ~dst:t;
          sub t observed;
          if df = 1 && correction then begin
            abs t;  (* Use Yates' correction for continuity. *)
            add_constant t (-. 0.5)
          end;
          mul_elements t t;
          div_elements t expected;
          sum t
        end
      in finalize (Distributions.ChiSquared.create ~df) chisq
end

module Wilcoxon = struct
  let w_table_ref = ref (Array.make 0 [|[||]|])

  let rec ensure_w_table_size m n =
    if m > n
    then ensure_w_table_size n m
    else
      let w = Array.make (m + 1) [|[||]|] in begin
        for i = 0 to m do
          Array.unsafe_set w i (Array.make (n + 1) [||]);
        done;

        w_table_ref := w
      end

  let rec compute_w k m n =
    let u = m * n in
    if k < 0 || k > u
    then 0.
    else
      let w      = !w_table_ref in
      let c      = u / 2 in
      let k      = if k > c then u - k else k in
      let (i, j) = if m < n then (m, n) else (n, m) in begin
        if i = 0 && j = 0
        then float_of_bool (k = 0)
        else begin
          if w.(i).(j) = [||]
          then w.(i).(j) <- Array.make (c + 1) (-. 1.);

          if w.(i).(j).(k) < 0.
          then begin
            if i = 0 && j = 0
            then w.(i).(j).(k) <- float_of_bool (k = 0)
            else w.(i).(j).(k) <-
                   compute_w (k - j) (i - 1) j +. compute_w k i (j - 1);
          end;

          w.(i).(j).(k)
        end
      end

  let w_cumulative_probability x m n =
    if x < 0.
    then 0.
    else if x > m *. n
    then 1.
    else
      let (m, n) = (round m, round n) in
      let c      = Gsl.Sf.choose (m + n) n in begin
        ensure_w_table_size m n;
        let p = ref 0. in
        let j = if x < float_of_int (m * n) /. 2.
          then round x
          else round (float_of_int (m * n) -. x) - 1
        in begin
          for i = 0 to j do
            p := !p +. compute_w i m n /. c
          done; !p
        end
      end

  let two_sample_independent v1 v2
      ?(alternative=TwoSided) ?(correction=true) () =
    let n1 = float_of_int (Array.length v1)
    and n2 = float_of_int (Array.length v2) in
    if n1 = 0. || n2 = 0.
    then invalid_arg "Wilcoxon.two_sample_independent: no data";

    let (t, ranks) = Sample.rank (Array.append v1 v2) in
    let n  = n1 +. n2 in
    let w1 = Array.sum (Array.sub ranks 0 (int_of_float n1)) in
    let w2 = Array.sum (Array.sub ranks (int_of_float n1) (int_of_float n2)) in
    let u1 = w1 -. n1 *. (n1 +. 1.) /. 2. in
    let u2 = w2 -. n2 *. (n2 +. 1.) /. 2. in
    let u  = min u1 u2 in
    assert (u1 +. u2 = n1 *. n2);

    (* Lower bounds for normal approximation were taken from
       Gravetter, Frederick J., and Larry B. Wallnau.
       "Statistics for the behavioral sciences". Wadsworth Publishing
       Company, 2006. *)
    if t <> 0. || n1 > 20. && n2 > 20.
    then
      (* Normal approximation. *)
      let mean  = n1 *. n2 /. 2. in
      let sd    = sqrt ((n1 *. n2 /. 12.) *.
                          ((n +. 1.) -. t /. (n *. (n -. 1.)))) in
      let delta =
        if correction
        then match alternative with
          | Less     -> -. 0.5
          | Greater  -> 0.5
          | TwoSided -> if u > mean then 0.5 else -. 0.5
        else 0.
      in

      let z = (u -. mean -. delta) /. sd in
      let open Distributions.Normal in
      let pvalue = match alternative with
        | Less     -> cumulative_probability standard ~x:z
        | Greater  -> 1. -. cumulative_probability standard ~x:z
        | TwoSided ->
          2. *. (min (cumulative_probability standard ~x:z)
                     (1. -. cumulative_probability standard ~x:z))
      in (u, pvalue)
    else
      (* Exact critical value. *)
      let pvalue = match alternative with
        | Less     -> w_cumulative_probability u n1 n2
        | Greater  -> 1. -. w_cumulative_probability u n1 n2
        | TwoSided ->
          2. *. (min (w_cumulative_probability u n1 n2)
                     (1. -. w_cumulative_probability u n1 n2))
      in (u, pvalue)
end
