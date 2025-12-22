type t = { buf : bytes; width : int }

val make : version:int -> t
(** [make ~version] creates an empty QR code matrix for the given version. *)

val place_pattern_modules : t -> int -> unit
(** [place_pattern_modules t version] places the fixed pattern modules (finder
    patterns, separators, timing patterns, alignment patterns, dark module) into
    the QR code matrix [t] for the given [version]. *)

val place_format_info : t -> ecl:Config.ECL.t -> mask_pattern:int -> unit
(** [place_format_info t ~ecl ~mask_pattern] writes the 15 format bits (with BCH
    error correction and fixed XOR mask) into their two locations. *)

val place_data : t -> bytes -> int -> unit
(** [place_data t data version] places the encoded data bytes into the QR code
    matrix [t], avoiding reserved areas for the given [version]. *)

val to_unicode_string : t -> string
(** Convert the QR code matrix [t] into a Unicode string representation. *)
