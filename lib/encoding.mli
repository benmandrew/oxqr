val encode : string -> Config.ECL.t -> local_ Bitbuf.t
(** [encode data ecl] encodes the given [data] string into a bit buffer using
    the specified error correction level [ecl]. Handles mode selection, data
    encoding, and error correction codeword generation. *)

val generate_qr : string -> Config.ECL.t -> Qr.t
(** [generate data ecl] generates a QR code matrix for the given [data] string
    and error correction level [ecl]. Returns a QR code matrix ready to be
    rendered. *)
