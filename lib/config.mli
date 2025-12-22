(* Error Correction Level *)
module ECL : sig
  type t = L | M | Q | H

  val compare : t -> t -> int
end

type t = { version : int; ecl : ECL.t }

val make : version:int -> ecl:ECL.t -> t
val compare : t -> t -> int
val char_count_indicator_length : t -> int
val mode_indicator_length : int
val mode_indicator : int
val get_config_and_capacity : string -> ECL.t -> t * int
