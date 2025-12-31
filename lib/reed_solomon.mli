val generate_error_correction : local_ bytes -> int -> bytes
(** [generate_error_correction data ec_count] generates [ec_count] error
    correction codewords for the given [data] using Reed-Solomon encoding in
    GF(256). *)
