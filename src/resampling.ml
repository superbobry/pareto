open Internal

let jackknife ~estimator vs =
  let n = Array.length vs in
  Array.init n (fun i ->
      (** Note(superbobry): we can be sure that [vs] has _at least_ one
          element if this function was called. *)
      let holey = Array.make (n - 1) vs.(0) in begin
        Array.blit vs 0 holey 0 i;
        Array.blit vs (i + 1) holey i (n - i - 1);
        estimator holey
      end
    )

let resample ?(rng=default_rng) ~estimator ~n vs =
  Array.init n (fun _i -> estimator (Sample.sample ~rng ~replace:true vs))
