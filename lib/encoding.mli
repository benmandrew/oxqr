val generate : string -> Config.ECL.t -> Qr.t
(** [generate data ecl] generates a QR code matrix for the given [data] string
    and error correction level [ecl]. Returns a QR code matrix ready to be
    rendered. *)
