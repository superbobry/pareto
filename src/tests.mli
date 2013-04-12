open Internal

type test_type = OneTailed | TwoTailed
type test_result = Significant | NonSignigicant

(** FIXME(superbobry): add test type argument. *)
val run_test : ?pvalue:float -> (unit -> (float * float)) -> test_result

val chi_squared : Vector.t -> Vector.t -> ?df:int -> unit -> (float * float)
