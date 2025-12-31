val generate_error_correction : local_ bytes -> int -> bytes -> unit
(** [generate_error_correction data ec_count out_buf] generates [ec_count] error
    correction codewords for the given [data] using Reed-Solomon encoding in
    GF(256), writing the result into [out_buf]. *)
