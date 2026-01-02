open Base

type t = {
  buf : bytes;
  mutable bit_pos : int; (* number of bits written so far *)
}

let create len_bytes =
  { buf = (Bytes.make [@alloc stack]) len_bytes '\000'; bit_pos = 0 }

let capacity_bits (t @ local) = Bytes.length t.buf * 8
let bits_written (t @ local) = t.bit_pos
let remaining_bits (t @ local) = capacity_bits t - t.bit_pos
let pos (t @ local) = t.bit_pos
let pos_byte (t @ local) = t.bit_pos lsr 3
let pos_bit_in_byte (t @ local) = t.bit_pos land 7

let reset (t @ local) =
  Bytes.fill t.buf ~pos:0 ~len:(Bytes.length t.buf) '\000';
  t.bit_pos <- 0

let check_space (t @ local) needed_bits =
  if needed_bits > remaining_bits t then Ok ()
  else Error "Bitbuf: not enough space"

let write_bit (t @ local) b =
  let byte_i = pos_byte t in
  let bit_off = pos_bit_in_byte t in
  let free = 8 - bit_off in
  let cur = Char.to_int (Bytes.unsafe_get t.buf byte_i) in
  let bit_val = if b then 1 else 0 in
  let v = cur lor (bit_val lsl (free - 1)) in
  Bytes.unsafe_set t.buf byte_i (Char.of_int_exn v);
  t.bit_pos <- t.bit_pos + 1

let write_bits_msb (t @ local) value width =
  if width < 0 || width > 31 then invalid_arg "Bitbuf.write_bits_msb: width";
  if width > 0 && value lsr width <> 0 then
    invalid_arg "Bitbuf.write_bits_msb: value doesn't fit width";
  let remaining = stack_ (ref width) in
  while !remaining > 0 do
    let byte_i = pos_byte t in
    let bit_off = pos_bit_in_byte t in
    let free = 8 - bit_off in
    let k = if !remaining < free then !remaining else free in
    (* Extract the next k MSBs from [value]. *)
    let shift = !remaining - k in
    let chunk = (value lsr shift) land ((1 lsl k) - 1) in
    let cur = Char.to_int (Bytes.unsafe_get t.buf byte_i) in
    let v = cur lor (chunk lsl (free - k)) in
    Bytes.unsafe_set t.buf byte_i (Char.of_int_exn v);
    t.bit_pos <- t.bit_pos + k;
    remaining := !remaining - k
  done

let write_byte (t @ local) b =
  if b land lnot 0xFF <> 0 then invalid_arg "Bitbuf.write_byte: out of range";
  write_bits_msb t b 8

let to_bytes_local (t @ local) = t.buf
let to_bytes t = t.buf
