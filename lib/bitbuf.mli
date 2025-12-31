(** Fixed-size bit-level write buffer with MSB-first packing. *)

type t

val create : int -> local_ t
(** [create len_bytes] allocates a zeroed buffer of [len_bytes]. *)

val capacity_bits : local_ t -> int [@@zero_alloc]
val bits_written : local_ t -> int [@@zero_alloc]
val remaining_bits : local_ t -> int [@@zero_alloc]

val reset : local_ t -> unit
[@@zero_alloc]
(** Reset the write position and zero the buffer. *)

val write_bit : local_ t -> bool -> unit
[@@zero_alloc]
(** Write a single bit (MSB-first within the current byte). *)

val write_bits_msb : local_ t -> int -> int -> unit
[@@zero_alloc]
(** [write_bits_msb t value width] writes the top [width] bits of [value] into
    the buffer, MSB-first. [width] must be in [0, 31] and [value] must fit:
    [value < 1 lsl width]. *)

val write_byte : local_ t -> int -> unit
[@@zero_alloc]
(** Write an 8-bit value aligned to the current bit position. *)

val to_bytes_local : local_ t -> local_ bytes
[@@zero_alloc]
(** Access the underlying bytes. Mutating the returned value mutates [t]. *)

val to_bytes : t -> bytes
[@@zero_alloc]
(** Access the underlying bytes. Mutating the returned value mutates [t]. *)

val pos : local_ t -> int
[@@zero_alloc]
(** Current bit position from start (0-based). *)

val pos_byte : local_ t -> int
[@@zero_alloc]
(** Current byte index (0-based). *)

val pos_bit_in_byte : local_ t -> int
[@@zero_alloc]
(** Bit offset inside current byte [0..7], counting written bits. *)
