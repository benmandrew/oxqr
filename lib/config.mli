(* Error Correction Level *)
module ECL : sig
  type t = L | M | Q | H

  val compare : t -> t -> int
end

type t = { version : int; ecl : ECL.t }

val make : version:int -> ecl:ECL.t -> t
(** [make ~version ~ecl] creates a QR code configuration with the given
    [version] and error correction level [ecl]. *)

val get_capacity : t -> int
(** Get the total data capacity in bits for the given configuration [t]. *)
