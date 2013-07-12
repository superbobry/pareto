module Randist = Gsl.Randist

open Internal


let range ?(a=0) ~b =
  if b <= a
  then [||]
  else
    let vs = Array.make (b - a) 0 in begin
      for i = a to b - 1 do
        Array.unsafe_set vs (i - a) i
      done; vs
    end


let cumulative ~f = function
  | [||] -> [||]
  | xs   ->
    let n   = Array.length xs in
    let acc = Array.make n (Array.unsafe_get xs 0) in
    for i = 1 to n - 1 do
      Array.unsafe_set acc i
        (f (Array.unsafe_get acc (i - 1)) (Array.unsafe_get xs i))
    done; acc



let reorder is ~src ~dst =
  let n = Array.length src in
  for i = 0 to n - 1 do
    let j = Array.unsafe_get is i in
    Array.unsafe_set dst i (Array.unsafe_get src j)
  done


let search_sorted ~cmp vs v =
  let rec loop l r =
    (* We're looking for [v] in a semiclosed interval [l, r).
       Invariants: mid < r,
                   0 < l <= r. *)
    if l >= r
    then None
    else
      let mid = (r - l) / 2 + l in
      let res = cmp v (Array.unsafe_get vs mid) in
      if res = 0
      then Some mid
      else if res < 0
      then loop l mid
      else loop (mid + 1) r
  in loop 0 (Array.length vs)


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
