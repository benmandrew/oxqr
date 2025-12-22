type t

val create : bytes:int -> t
(** [create ~bytes] preallocates exactly [bytes] capacity; writing past it
    raises [Invalid_argument]. *)

val clear : t -> unit
val length_bits : t -> int
val length_bytes : t -> int

val add_bit : t -> bool -> unit
(** Append a single bit (MSB-first within a byte). *)

val add_bits : t -> int -> int -> unit
(** [add_bits t value count] appends the [count] most-significant bits of
    [value], MSB-first. Requires [0 <= count <= 31] and [value] fits in [count]
    bits. Raises if capacity is exceeded. *)

val add_byte : t -> int -> unit
(** Append one full byte [0..255]. Raises if capacity is exceeded. *)

val add_bytes : t -> bytes -> unit
(** Append a sequence of bytes as-is. Raises if capacity is exceeded. *)

val to_bytes : t -> bytes
(** Snapshot the contents as bytes. If there is a partially-filled byte, it is
    padded with 0s in the least-significant bits. Does not modify [t]. *)
