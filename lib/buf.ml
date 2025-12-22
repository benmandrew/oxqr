type t = {
  buf : bytes;
  cap_bytes : int;
  mutable len_bytes : int; (* full bytes flushed *)
  mutable cur_byte : int; (* accumulated bits, MSB-first flush *)
  mutable cur_bits_filled : int; (* 0..7 *)
  mutable len_bits : int;
}

let create ~bytes =
  if bytes <= 0 then invalid_arg "Bit_buffer.create: bytes must be positive";
  {
    buf = Bytes.create bytes;
    cap_bytes = bytes;
    len_bytes = 0;
    cur_byte = 0;
    cur_bits_filled = 0;
    len_bits = 0;
  }

let clear t =
  t.len_bytes <- 0;
  t.cur_byte <- 0;
  t.cur_bits_filled <- 0;
  t.len_bits <- 0

let length_bits t = t.len_bits
let length_bytes t = t.len_bytes + if t.cur_bits_filled > 0 then 1 else 0

let flush_full_byte t v =
  if t.len_bytes >= t.cap_bytes then invalid_arg "Bit_buffer: overflow";
  Bytes.set t.buf t.len_bytes (Char.unsafe_chr (v land 0xFF));
  t.len_bytes <- t.len_bytes + 1

let add_bit t b =
  if t.len_bits >= t.cap_bytes * 8 then
    invalid_arg "Bit_buffer.add_bit: overflow";
  (* Shift left and append the new bit on the right. After 8 bits, the first
     appended bit ends up as the MSB of the byte. *)
  let bit = if b then 1 else 0 in
  t.cur_byte <- (t.cur_byte lsl 1) lor bit land 0xFF;
  t.cur_bits_filled <- t.cur_bits_filled + 1;
  t.len_bits <- t.len_bits + 1;
  if t.cur_bits_filled = 8 then (
    flush_full_byte t t.cur_byte;
    t.cur_byte <- 0;
    t.cur_bits_filled <- 0)

let add_bits t value count =
  if count < 0 || count > 31 then invalid_arg "Bit_buffer.add_bits: count";
  if t.len_bits + count > t.cap_bytes * 8 then
    invalid_arg "Bit_buffer.add_bits: overflow";
  (* Optional: bounds-check that value fits in [count] bits. *)
  let maxv =
    if count = 31 then Int32.to_int Int32.max_int else (1 lsl count) - 1
  in
  if value < 0 || value > maxv then invalid_arg "Bit_buffer.add_bits: value";
  for i = count - 1 downto 0 do
    add_bit t ((value lsr i) land 1 = 1)
  done

let add_byte t v =
  if v < 0 || v > 255 then invalid_arg "Bit_buffer.add_byte: byte range";
  if t.cur_bits_filled = 0 then (
    (* Fast path: already byte-aligned. *)
    if t.len_bytes >= t.cap_bytes then
      invalid_arg "Bit_buffer.add_byte: overflow";
    flush_full_byte t v)
  else (* Add 8 bits MSB-first. *)
    add_bits t v 8

let add_bytes t bs =
  if t.cur_bits_filled = 0 then (
    let n = Bytes.length bs in
    if t.len_bytes + n > t.cap_bytes then
      invalid_arg "Bit_buffer.add_bytes: overflow";
    Bytes.blit bs 0 t.buf t.len_bytes n;
    t.len_bytes <- t.len_bytes + n;
    t.len_bits <- t.len_bits + (n * 8))
  else
    for (* Not aligned: add byte-by-byte. *)
        i = 0 to Bytes.length bs - 1 do
      add_byte t (Char.code (Bytes.get bs i))
    done

let to_bytes t =
  let extra = if t.cur_bits_filled > 0 then 1 else 0 in
  let out = Bytes.create (t.len_bytes + extra) in
  (* Copy full bytes *)
  Bytes.blit t.buf 0 out 0 t.len_bytes;
  (* If partial, pad with zeros on the right (LSBs) to complete the last byte. *)
  (if extra = 1 then
     let v = (t.cur_byte lsl (8 - t.cur_bits_filled)) land 0xFF in
     Bytes.set out t.len_bytes (Char.unsafe_chr v));
  out
