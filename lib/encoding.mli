module Arena : sig
  type t

  val create : int option -> t
  val get_qr_buffer : local_ t -> local_ Bytes.t
  val get_remainder_scratch : local_ t -> local_ int array
end

val encode : Arena.t -> string -> Config.ECL.t -> int
[@@zero_alloc]
(** [encode arena data ecl] encodes the given [data] string into the QR code
    structure stored in [arena] with error correction level [ecl]. *)

val generate_qr : Arena.t -> string -> Config.ECL.t -> Qr.t
(** [generate arena data ecl] generates a QR code matrix for the given [data]
    string and error correction level [ecl]. Returns a QR code matrix ready to
    be rendered. *)

val generate_qr_stack : Arena.t -> string -> Config.ECL.t -> unit
[@@zero_alloc]
(** Version of [generate_qr] that only allocates on the stack *)
