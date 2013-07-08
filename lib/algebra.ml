(** A monoid is a type with an id and an associative binary operation. *)
module Monoid = struct
  module type S = sig
    type t

    (** Identity, subject to [mappend mempty x = mappend x mempty = x]. *)
    val mempty  : t

    (** Associative binary operation. *)
    val mappend : t -> t -> t
  end
end
