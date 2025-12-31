val generate_error_correction :
  local_ bytes ->
  pos:int ->
  len:int ->
  int ->
  local_ bytes ->
  out_pos:int ->
  unit
[@@zero_alloc]
(** [generate_error_correction data ~pos ~len ec_count out ~out_pos] fills [out]
    with [ec_count] error-correction codewords for the slice of [data] starting
    at [pos] with length [len] using Reed-Solomon encoding in GF(256). The
    caller must ensure the buffers are large enough; the function allocates no
    heap memory at runtime. *)
