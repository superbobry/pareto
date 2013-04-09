module type Distribution = sig
  type t

  val cumulative_probability : t -> x:float -> float
end

module type DiscreteDistribution = sig
  type t

  include Distribution with type t := t

  val probability : t -> x:float -> float
end

module type ContinuousDistribution = sig
  type t

  include Distribution with type t := t

  val density : t -> x:float -> float
end

module type Mean = sig
  type t

  val mean : t -> float
end

module type Variance = sig
  type t

  val variance : t -> float
end


external erfc : float -> float = "_erfc"


module Constants = struct
  (* See http://www.gnu.org/software/libc/manual/html_node/Mathematical-Constants.html *)

  (* sqrt 2. *)
  let m_sqrt_2 = 1.4142135623730950488016887242096980785696718753769480731766

  (* sqrt (2. *. pi) *)
  and m_sqrt_2_pi = 2.5066282746310005024157652848110452530069867406099383166299

  (* 2. / sqrt pi *)
  and m_2_sqrt_pi = 1.1283791670955125738961589031215451716881012586579977136881
end
