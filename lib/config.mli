(* Error Correction Level *)
module ECL : sig
  type t = L | M | Q | H

  val compare : t -> t -> int
end

type t = { version : int; ecl : ECL.t }

val make : version:int -> ecl:ECL.t -> t
val compare : t -> t -> int
val char_count_indicator_length : t -> int
val alphanumeric_encode : char -> int
val alphanumeric_encode_res : char -> local_ (int, string) result

type ec_info = {
  ec_codewords_per_block : int;
  group1_blocks : int;
  group1_data_codewords : int;
  group2_blocks : int;
  group2_data_codewords : int;
}

val mode_indicator_length : int
val mode_indicator : int
val get_config : string -> ECL.t -> t
val get_ec_info : t -> ec_info
