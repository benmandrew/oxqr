(** Fixed-size bit-level write buffer with MSB-first packing. *)

type t

val create : int -> t
(** [create len_bytes] allocates a zeroed buffer of [len_bytes]. *)

val capacity_bits : t -> int
val bits_written : t -> int
val remaining_bits : t -> int

val reset : t -> unit
(** Reset the write position and zero the buffer. *)

val write_bit : t -> bool -> unit
(** Write a single bit (MSB-first within the current byte). *)

val write_bits_msb : t -> value:int -> width:int -> unit
(** [write_bits_msb t ~value ~width] writes the top [width] bits of [value] into
    the buffer, MSB-first. [width] must be in [0, 31] and [value] must fit:
    [value < 1 lsl width]. *)

val write_byte : t -> int -> unit
(** Write an 8-bit value aligned to the current bit position. *)

val to_bytes : t -> bytes
(** Access the underlying bytes. Mutating the returned value mutates [t]. *)

val pos : t -> int
(** Current bit position from start (0-based). *)

val pos_byte : t -> int
(** Current byte index (0-based). *)

val pos_bit_in_byte : t -> int
(** Bit offset inside current byte [0..7], counting written bits. *)
