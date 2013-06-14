open Internal

type test_alternative = Less | Greater | TwoSided

type test_result = {
  test_statistic : float;
  test_pvalue    : float
}

let run_test ?(significance_level=0.05) f =
  let { test_pvalue = pvalue; _ } = f () in
  if pvalue <= significance_level
  then `NotSignificant
  else `Significant


module T = struct
  let finalize d t alternative =
    let open Distributions.T in
    let pvalue = match alternative with
    | Less     -> cumulative_probability d ~x:t
    | Greater  -> 1. -. cumulative_probability d ~x:t
    | TwoSided -> 2. *. cumulative_probability d ~x:(-. (abs_float t))
    in { test_statistic = t; test_pvalue = pvalue }

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

  let two_sample_paired v1 v2 ?(mean=0.) ?(alternative=TwoSided) () =
    let n = Array.length v1 in
    if n <> Array.length v2
    then invalid_arg "T.two_sample_paired: unequal length arrays";
    one_sample (Array.mapi (fun i x -> x -. v2.(i)) v1) ~mean ~alternative ()
end

module ChiSquared = struct
  let finalize d chisq =
    let open Distributions.ChiSquared in
    {
      test_statistic = chisq;
      test_pvalue    = 1. -. cumulative_probability ~x:chisq d
    }

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
      { test_statistic = 0.; test_pvalue = 1. }
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

module KolmogorovSmirnov = struct
  let create_h n d =
    let k    = floor (float_of_int n *. d) +. 1. in
    let h    = k -. float_of_int n *. d in
    let size = 2 * int_of_float k - 1 in
    let m    = Matrix.create ~init:0. size size in begin
      (* Set all element in the lower triangle to 1. *)
      for i = 0 to size - 1 do
        for j = max 0 (i - 1) to size - 1 do
          Matrix.set m (size - 1 - i) (size - 1 - j) 1.
        done
      done;

      (* Pre-calculate 'h' powers for the first column and bottom row. *)
      let h_powers = Array.make size 0. in
      Array.unsafe_set h_powers 0 h;
      for i = 1 to size - 1 do
        Array.unsafe_set h_powers i
          ((Array.unsafe_get h_powers (i - 1)) *. h)
      done;

      (* Correct first column and bottom row. *)
      for i = 0 to size - 1 do
        Matrix.set m i 0 (Matrix.get m i 0 -. Array.get h_powers i);
        Matrix.set m (size - 1) i
          (Matrix.get m (size - 1) i -. Array.get h_powers (size - 1 - i))
      done;

      (* Correct bottom left element if needed. *)
      if 2. *. h > 1.
      then Matrix.set m (size - 1) 0
          (Matrix.get m (size - 1) 0 +. (2. *. h -. 1.) ** float_of_int size);

      (* Here come factorials! *)
      let facts = Array.make size 1 in
      for i = 1 to size - 1 do
        Array.unsafe_set facts i (Array.unsafe_get facts (i - 1) * (i + 1))
      done;

      for i = 0 to size - 1 do
        for j = i + 1 to size - 1 do
          Matrix.set m (size - 1 - i) (size - 1 - j)
            (Matrix.get m (size - 1 - i) (size - 1 - j) /.
               float_of_int (Array.unsafe_get facts (j - i)))
        done
      done
    end; m

  let goodness_of_fit vs cp ?(alternative=TwoSided) () =
    let n = Array.length vs in
    if n < 1 then invalid_arg "KolmogorovSmirnov.goodness_of_fit: no data";

    let is       = Array.sort_index compare vs in
    let ds_plus  = ref min_float
    and ds_minus = ref min_float in begin
      for i = 0 to n - 1 do
        let j = Array.unsafe_get is i in
        ds_minus := max !ds_minus
            (cp vs.(j) -. float_of_int i /. float_of_int n);
        ds_plus  := max !ds_plus
            (float_of_int (i + 1) /. float_of_int n -. cp vs.(j))
      done;
    end;

    let d = match alternative with
      | Less     -> !ds_minus
      | Greater  -> !ds_plus
      | TwoSided -> max !ds_minus !ds_plus
    in

    let pvalue = match alternative with
      | Less | Greater ->
        if d <= 0.
        then 0.
        else if d >= 1.
        then 1.
        else
          (** See Section 3 in Birnbaum and Tingey. *)
          let acc = ref 0. in begin
            for i = 0 to int_of_float (floor (float_of_int n *. (1. -. d))) do
              let j  = float_of_int i in
              let s1 = (float_of_int n -. j) *.
                         log (1. -. d -. j /. float_of_int n)
              and s2 = (j -. 1.) *. log (d +. j /. float_of_int n)
              in acc := !acc +. exp (Gsl.Sf.lnchoose n i +. s1 +. s2)
            done
          end; d *. !acc
      | TwoSided ->
        (** FIXME(superbobry): control for overflow in matrix power
            calculation. *)
        let h   = Matrix.power (create_h n d) n in
        let k   = int_of_float (ceil (float_of_int n *. d)) in
        let acc = ref (Matrix.get h (k - 1) (k - 1)) in begin
          for i = 1 to n do
            acc := !acc *. float_of_int i /. float_of_int n
          done
        end; 1. -. !acc
    in { test_statistic = d; test_pvalue = pvalue }
end

module MannWhitneyU = struct
  let two_sample_independent v1 v2
      ?(alternative=TwoSided) ?(correction=true) () =
    let n1 = float_of_int (Array.length v1)
    and n2 = float_of_int (Array.length v2) in
    if n1 = 0. || n2 = 0.
    then invalid_arg "MannWhitneyU.two_sample_independent: no data";

    let n  = n1 +. n2 in
    let (t, ranks) = Sample.rank (Array.append v1 v2) in
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
    if t <> 0. || (n1 > 20. && n2 > 20.)
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
      in { test_statistic = u; test_pvalue = pvalue }
    else
      (* Exact critical value. *)
      let k  = int_of_float (min n1 n2) in
      let c  = Combi.make (int_of_float n) k in
      let c_n_k = Gsl.Sf.choose (int_of_float n) k in
      let le = ref 0 in
      let gt = ref 0 in
      begin
        for _i = 0 to int_of_float c_n_k do
          let cu = Array.sum_with (fun i -> ranks.(i)) (Combi.to_array c) -.
                     float_of_int (k * (k + 1)) /. 2.
          in incr (if cu <= u then le else gt);

          Combi.next c;
        done;

        let pvalue = match alternative with
          | Less     -> float_of_int !le /. c_n_k
          | Greater  -> float_of_int !gt /. c_n_k
          | TwoSided -> 2. *. float_of_int (min !le !gt) /. c_n_k
        in { test_statistic = u; test_pvalue = pvalue }
      end
end

module WilcoxonT = struct
  let two_sample_paired v1 v2 ?(alternative=TwoSided) ?(correction=true) () =
    let n = Array.length v1 in
    if n = 0
    then invalid_arg "WilcoxonT.two_sample_paired: no data";
    if n <> Array.length v2
    then invalid_arg "WilcoxonT.two_sample_paired: unequal length arrays";

    let d  = Array.init n (fun i -> v2.(i) -. v1.(i)) in
    let (zeros, non_zeros) = Array.partition ((=) 0.) d in
    let nz = float_of_int (Array.length non_zeros) in
    let (t, ranks) = Sample.rank non_zeros
        ~cmp:(fun d1 d2 -> compare (abs_float d1) (abs_float d2)) in
    let w_plus  = Array.sum
        (Array.mapi (fun i v -> if v > 0. then ranks.(i) else 0.) non_zeros) in
    let w_minus = nz *. (nz +. 1.) /. 2. -. w_plus in

    (* Following Sheskin, W is computed as a minimum of W+ and W-. *)
    let w = min w_plus w_minus in

    if t <> 0. || Array.length zeros <> 0 || n > 20
    then
      (* Normal approximation. *)
      let mean  = nz *. (nz +. 1.) /. 4. in
      let sd    = sqrt (nz *. (nz +. 1.) *. (2. *. nz +. 1.) /. 24. -.
                          t /. 48.) in
      let delta =
        if correction
        then match alternative with
          | Less     -> -. 0.5
          | Greater  -> 0.5
          | TwoSided -> if w > mean then 0.5 else -. 0.5
        else 0.
      in

      let z = (w -. mean -. delta) /. sd in
      let open Distributions.Normal in
      let pvalue = match alternative with
        | Less     -> cumulative_probability standard ~x:z
        | Greater  -> 1. -. cumulative_probability standard ~x:z
        | TwoSided ->
          2. *. (min (cumulative_probability standard ~x:z)
                     (1. -. cumulative_probability standard ~x:z))
      in { test_statistic = w; test_pvalue = pvalue }
    else
      (* Exact critical value. *)
      let le = ref 0 in
      let gt = ref 0 in
      let two_n = float_of_int (2 lsl (int_of_float nz)) in
      begin
        for i = 0 to int_of_float two_n - 1 do
          let pw = ref 0. in
          for j = 0 to int_of_float nz - 1 do
            if (i lsr j) land 1 = 1
            then pw := !pw +. ranks.(j);
          done;

          incr (if !pw <= w then le else gt);
        done;

        let pvalue = match alternative with
          | Less     -> float_of_int !le /. two_n
          | Greater  -> float_of_int !gt /. two_n
          | TwoSided -> 2. *. float_of_int (min !le !gt) /. two_n
        in { test_statistic = w; test_pvalue = pvalue }
      end

  let one_sample vs ?(shift=0.) =
    two_sample_paired (Array.make (Array.length vs) shift) vs
end

module Sign = struct
  let two_sample_paired v1 v2 ?(alternative=TwoSided) () =
    let n = Array.length v1 in
    if n = 0
    then invalid_arg "WilcoxonT.two_sample_paired: no data";
    if n <> Array.length v2
    then invalid_arg "WilcoxonT.two_sample_paired: unequal length arrays";

    let ds = Array.init n (fun i -> v2.(i) -. v1.(i)) in
    let (pi_plus, pi_minus) = Array.fold_left
        (fun (p, m) d ->
           if d > 0.
           then (succ p, m)
           else if d < 0. then (p, succ m)
           else (p, m))
        (0, 0) ds
    in

    let open Distributions.Binomial in
    let d = create ~trials:(pi_plus + pi_minus) ~p:0.5 in
    let pvalue = match alternative with
      | Less     -> cumulative_probability d ~n:pi_plus
      | Greater  -> 1. -. cumulative_probability d ~n:(pi_plus - 1)
      | TwoSided ->
        2. *. (min (cumulative_probability d ~n:pi_plus)
                 (1. -. cumulative_probability d ~n:(pi_plus - 1)))
    in { test_statistic = float_of_int pi_plus; test_pvalue = min 1. pvalue }

  let one_sample vs ?(shift=0.) =
    two_sample_paired (Array.make (Array.length vs) shift) vs
end


module Multiple = struct
  type adjustment_method =
    | HolmBonferroni
    | BenjaminiHochberg

  let adjust pvalues how =
    let m = Array.length pvalues in
    let adjusted_pvalues = Array.make m 0. in
    begin match how with
      | HolmBonferroni ->
        let is = Array.sort_index compare pvalues in
        let iu = Array.sort_index compare is in begin
          for i = 0 to m - 1 do
            let j = Array.unsafe_get is i in
            Array.unsafe_set adjusted_pvalues i
              (min 1. (float_of_int (m - i) *. pvalues.(j)))
          done;

          let cm = Base.cumulative ~f:max adjusted_pvalues in
          Array.reorder iu cm adjusted_pvalues
        end
      | BenjaminiHochberg ->
        let is = Array.sort_index (flip compare) pvalues in
        let iu = Array.sort_index compare is in begin
          for i = 0 to m - 1 do
            let j = Array.unsafe_get is i in
            Array.unsafe_set adjusted_pvalues i
              (min 1. (float_of_int m /. float_of_int (m - i) *. pvalues.(j)))
          done;

          let cm = Base.cumulative ~f:min adjusted_pvalues in
          Array.reorder iu cm adjusted_pvalues
        end
    end; adjusted_pvalues
end
