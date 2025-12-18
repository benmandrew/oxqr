type t = {
  buf : bytes;
  mutable bit_pos : int; (* number of bits written so far *)
}

let create len_bytes = { buf = Bytes.make len_bytes '\000'; bit_pos = 0 }
let capacity_bits t = Bytes.length t.buf * 8
let bits_written t = t.bit_pos
let remaining_bits t = capacity_bits t - t.bit_pos
let pos t = t.bit_pos
let pos_byte t = t.bit_pos lsr 3
let pos_bit_in_byte t = t.bit_pos land 7

let reset t =
  Bytes.fill t.buf 0 (Bytes.length t.buf) '\000';
  t.bit_pos <- 0

let[@inline] check_space t needed_bits =
  if needed_bits > remaining_bits t then invalid_arg "Bitbuf: overflow"

let write_bit t b =
  check_space t 1;
  let byte_i = pos_byte t in
  let bit_off = pos_bit_in_byte t in
  let free = 8 - bit_off in
  let cur = int_of_char (Bytes.unsafe_get t.buf byte_i) in
  let bit_val = if b then 1 else 0 in
  let v = cur lor (bit_val lsl (free - 1)) in
  Bytes.unsafe_set t.buf byte_i (char_of_int v);
  t.bit_pos <- t.bit_pos + 1

let write_bits_msb t ~value ~width =
  if width < 0 || width > 31 then invalid_arg "Bitbuf.write_bits_msb: width";
  if width > 0 && value lsr width <> 0 then
    invalid_arg "Bitbuf.write_bits_msb: value doesn't fit width";
  check_space t width;
  let remaining = ref width in
  while !remaining > 0 do
    let byte_i = pos_byte t in
    let bit_off = pos_bit_in_byte t in
    let free = 8 - bit_off in
    let k = if !remaining < free then !remaining else free in
    (* Extract the next k MSBs from [value]. *)
    let shift = !remaining - k in
    let chunk = (value lsr shift) land ((1 lsl k) - 1) in
    let cur = int_of_char (Bytes.unsafe_get t.buf byte_i) in
    let v = cur lor (chunk lsl (free - k)) in
    Bytes.unsafe_set t.buf byte_i (char_of_int v);
    t.bit_pos <- t.bit_pos + k;
    remaining := !remaining - k
  done

let write_byte t b =
  if b land lnot 0xFF <> 0 then invalid_arg "Bitbuf.write_byte: out of range";
  write_bits_msb t ~value:b ~width:8

let to_bytes t = t.buf
