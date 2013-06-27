module Randist = Gsl.Randist

open Internal


let range ?(a=0) ~b = Array.range a b


let cumulative ~f = function
  | [||] -> [||]
  | xs   ->
    let n   = Array.length xs in
    let acc = Array.make n (Array.unsafe_get xs 0) in
    for i = 1 to n - 1 do
      Array.unsafe_set acc i
        (f (Array.unsafe_get acc (i - 1)) (Array.unsafe_get xs i))
    done; acc


let shuffle ?(rng=default_rng) vs =
  let svs = Array.copy vs in begin
    Randist.shuffle rng svs;
    svs
  end

and sample ?(rng=default_rng) ?(replace=false) ?size vs =
  let dst = match size with
    | Some n ->
      if vs = [||] || not replace && Array.length vs < n
      then invalid_arg "Base.sample: not enough elements to sample from"
      else Array.create n vs.(0)
    | None   -> Array.copy vs
  in begin
    if replace
    then Randist.sample rng ~src:vs ~dst
    else Randist.choose rng ~src:vs ~dst;
    dst
  end
