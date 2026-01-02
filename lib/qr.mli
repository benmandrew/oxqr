type t = { buf : bytes; width : int }

val make : version:int -> t
(** [make ~version] creates an empty QR code matrix for the given version. *)

val place_pattern_modules : local_ t -> int -> unit
[@@zero_alloc]
(** [place_pattern_modules t version] places the fixed pattern modules (finder
    patterns, separators, timing patterns, alignment patterns, dark module) into
    the QR code matrix [t] for the given [version]. *)

val compute_format_bits : local_ Config.ECL.t -> int -> int
[@@zero_alloc]
(** [compute_format_bits ecl mask_pattern] computes the 15 format bits (with BCH
    error correction and fixed XOR mask) for the given error correction level
    [ecl] and [mask_pattern]. *)

val place_format_info : local_ t -> ecl:Config.ECL.t -> mask_pattern:int -> unit
[@@zero_alloc]
(** [place_format_info t ~ecl ~mask_pattern] writes the 15 format bits (with BCH
    error correction and fixed XOR mask) into their two locations. *)

val place_data : local_ t -> bytes -> int -> int -> unit
[@@zero_alloc]
(** [place_data t data data_len version] places the encoded data bytes into the
    QR code matrix [t], avoiding reserved areas for the given [version]. *)

val apply_mask_pattern : local_ t -> unit
[@@zero_alloc]
(** [apply_mask_pattern t] applies mask pattern 0 to the QR code [t]. *)

val to_unicode_string : t -> string
(** Convert the QR code matrix [t] into a Unicode string representation. *)
