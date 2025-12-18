type t = { buf : bytes; width : int }

val make : version:int -> t
(** [make ~version] creates an empty QR code matrix for the given version. *)

val place_pattern_modules : t -> int -> unit
(** [place_pattern_modules t version] places the fixed pattern modules (finder
    patterns, separators, timing patterns, alignment patterns, dark module) into
    the QR code matrix [t] for the given [version]. *)

val to_unicode_string : t -> string
(** Convert the QR code matrix [t] into a Unicode string representation. *)
