open Base

(* Maximum sizes for QR version 40 (largest): 81 blocks, 119 data codewords,
   30 EC codewords per block, 3706 total codewords. *)
let max_blocks = 81
let max_ec_per_block = 30
let _max_data_per_block = 119
let max_total_ec_bytes = max_blocks * max_ec_per_block (* 2430 *)
let max_total_bytes = 3706
let block_data_pos = Array.create ~len:max_blocks 0
let block_data_len = Array.create ~len:max_blocks 0
let block_ec_pos = Array.create ~len:max_blocks 0
let block_ec_len = Array.create ~len:max_blocks 0
let ec_storage = Bytes.create max_total_ec_bytes
let interleave_buffer = Bytes.create max_total_bytes

let[@zero_alloc] rec encode_pairs (buf @ local) s len i =
  let[@zero_alloc] decode c =
    match c with
    | '0' .. '9' -> Char.to_int c - Char.to_int '0'
    | 'A' .. 'Z' -> Char.to_int c - Char.to_int 'A' + 10
    | ' ' -> 36
    | '$' -> 37
    | '%' -> 38
    | '*' -> 39
    | '+' -> 40
    | '-' -> 41
    | '.' -> 42
    | '/' -> 43
    | ':' -> 44
    | _ -> failwith "Invalid alphanumeric character"
  in
  if i >= len then ()
  else if i = len - 1 then
    let value = decode s.[i] in
    Bitbuf.write_bits_msb buf value 6
  else
    let v1 = decode s.[i] and v2 = decode s.[i + 1] in
    let value = (v1 * 45) + v2 in
    Bitbuf.write_bits_msb buf value 11;
    encode_pairs buf s len (i + 2)

let[@zero_alloc] encode_alphanumeric_data buf s =
  let len = String.length s in
  encode_pairs buf s len 0

let[@zero_alloc] add_terminator_and_padding (buf @ local) total_data_codewords =
  (* Add terminator (0000, up to 4 bits) *)
  let bits_used = Bitbuf.bits_written buf in
  let max_bits = total_data_codewords * 8 in
  let terminator_bits = min 4 (max_bits - bits_used) in
  if terminator_bits > 0 then Bitbuf.write_bits_msb buf 0 terminator_bits;
  (* Pad to byte boundary *)
  let bits_after_term = Bitbuf.bits_written buf in
  let pad_to_byte = (8 - (bits_after_term % 8)) % 8 in
  if pad_to_byte > 0 then Bitbuf.write_bits_msb buf 0 pad_to_byte;
  (* Add padding bytes (alternating 0xEC and 0x11) *)
  let bytes_used = Bitbuf.bits_written buf / 8 in
  let rec add_padding i =
    if i >= total_data_codewords then ()
    else
      let pad_byte = if (i - bytes_used) % 2 = 0 then 0xEC else 0x11 in
      Bitbuf.write_byte buf pad_byte;
      add_padding (i + 1)
  in
  add_padding bytes_used;
  ()

let[@zero_alloc] split_into_blocks (data @ local) ec_info =
  let g1_count = ec_info.Config.group1_blocks in
  let g1_size = ec_info.group1_data_codewords in
  let g2_count = ec_info.group2_blocks in
  let g2_size = ec_info.group2_data_codewords in
  let ec_per_block = ec_info.ec_codewords_per_block in
  let total_blocks = g1_count + g2_count in
  (* Group 1 blocks *)
  for idx = 0 to g1_count - 1 do
    let data_pos = idx * g1_size in
    let ec_pos = idx * ec_per_block in
    block_data_pos.(idx) <- data_pos;
    block_data_len.(idx) <- g1_size;
    block_ec_pos.(idx) <- ec_pos;
    block_ec_len.(idx) <- ec_per_block;
    Reed_solomon.generate_error_correction data ~pos:data_pos ~len:g1_size
      ec_per_block ec_storage ~out_pos:ec_pos
  done;
  (* Group 2 blocks *)
  let base_data_pos = g1_count * g1_size in
  for j = 0 to g2_count - 1 do
    let idx = g1_count + j in
    let data_pos = base_data_pos + (j * g2_size) in
    let ec_pos = idx * ec_per_block in
    block_data_pos.(idx) <- data_pos;
    block_data_len.(idx) <- g2_size;
    block_ec_pos.(idx) <- ec_pos;
    block_ec_len.(idx) <- ec_per_block;
    Reed_solomon.generate_error_correction data ~pos:data_pos ~len:g2_size
      ec_per_block ec_storage ~out_pos:ec_pos
  done;
  total_blocks

let[@zero_alloc] interleave_blocks block_count ec_info data =
  let max_data_size =
    max ec_info.Config.group1_data_codewords ec_info.group2_data_codewords
  in
  let ec_size = ec_info.ec_codewords_per_block in
  let _total_size =
    (ec_info.group1_blocks * ec_info.group1_data_codewords)
    + (ec_info.group2_blocks * ec_info.group2_data_codewords)
    + (block_count * ec_size)
  in
  let out = interleave_buffer in
  let rec interleave_data i out_pos =
    if i = max_data_size then out_pos
    else
      let rec loop_blocks idx out_pos =
        if idx = block_count then out_pos
        else if i < block_data_len.(idx) then (
          Bytes.set out out_pos (Bytes.get data (block_data_pos.(idx) + i));
          loop_blocks (idx + 1) (out_pos + 1))
        else loop_blocks (idx + 1) out_pos
      in
      let next_pos = loop_blocks 0 out_pos in
      interleave_data (i + 1) next_pos
  in
  let rec interleave_ec i out_pos =
    if i = ec_size then out_pos
    else
      let rec loop_blocks idx out_pos =
        if idx = block_count then out_pos
        else
          let ec_pos = block_ec_pos.(idx) + i in
          Bytes.set out out_pos (Bytes.get ec_storage ec_pos);
          loop_blocks (idx + 1) (out_pos + 1)
      in
      let next_pos = loop_blocks 0 out_pos in
      interleave_ec (i + 1) next_pos
  in
  let after_data = interleave_data 0 0 in
  let _ = interleave_ec 0 after_data in
  out

let encode s (ecl @ local) =
  let config = Config.get_config s ecl in
  let ec_info = Config.get_ec_info config in
  let total_data_codewords =
    (ec_info.group1_blocks * ec_info.group1_data_codewords)
    + (ec_info.group2_blocks * ec_info.group2_data_codewords)
  in
  exclave_
  let buf = Bitbuf.create total_data_codewords in
  Bitbuf.write_bits_msb buf Config.mode_indicator Config.mode_indicator_length;
  let cci_len = Config.char_count_indicator_length config in
  Bitbuf.write_bits_msb buf (String.length s) cci_len;
  encode_alphanumeric_data buf s;
  add_terminator_and_padding buf total_data_codewords;
  buf

let generate_qr s ecl =
  let config = Config.get_config s ecl in
  let ec_info = Config.get_ec_info config in
  let buf = encode s ecl in
  let data = Bitbuf.to_bytes_local buf in
  let block_count = split_into_blocks data ec_info in
  let final_data = interleave_blocks block_count ec_info data in
  let qr = Qr.make ~version:config.version in
  Qr.place_pattern_modules qr config.version;
  let mask_pattern = 0 in
  Qr.place_format_info qr ~ecl:config.ecl ~mask_pattern;
  Qr.place_data qr final_data config.version;
  Qr.apply_mask_pattern qr;
  qr
