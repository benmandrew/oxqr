open Base

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
  exclave_ encode_pairs buf s len 0

let add_terminator_and_padding (buf @ local) total_data_codewords =
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

let split_into_blocks data ec_info =
  let g1_count = ec_info.Config.group1_blocks in
  let g1_size = ec_info.group1_data_codewords in
  let g2_count = ec_info.group2_blocks in
  let g2_size = ec_info.group2_data_codewords in
  let ec_per_block = ec_info.ec_codewords_per_block in
  let blocks = ref [] in
  let offset = ref 0 in
  (* Group 1 blocks *)
  for _ = 1 to g1_count do
    let block_data = Bytes.sub data ~pos:!offset ~len:g1_size in
    let ec_data =
      Reed_solomon.generate_error_correction block_data ec_per_block
    in
    blocks := (block_data, ec_data) :: !blocks;
    offset := !offset + g1_size
  done;
  (* Group 2 blocks *)
  for _ = 1 to g2_count do
    let block_data = Bytes.sub data ~pos:!offset ~len:g2_size in
    let ec_data =
      Reed_solomon.generate_error_correction block_data ec_per_block
    in
    blocks := (block_data, ec_data) :: !blocks;
    offset := !offset + g2_size
  done;
  List.rev !blocks

let interleave_blocks blocks ec_info =
  let max_data_size =
    max ec_info.Config.group1_data_codewords ec_info.group2_data_codewords
  in
  let ec_size = ec_info.ec_codewords_per_block in
  let total_size =
    (ec_info.group1_blocks * ec_info.group1_data_codewords)
    + (ec_info.group2_blocks * ec_info.group2_data_codewords)
    + (List.length blocks * ec_size)
  in
  let interleaved = Buffer.create total_size in
  (* Interleave data codewords *)
  for i = 0 to max_data_size - 1 do
    List.iter
      ~f:(fun (data, _) ->
        if i < Bytes.length data then
          Buffer.add_char interleaved (Bytes.get data i))
      blocks
  done;
  (* Interleave error correction codewords *)
  for i = 0 to ec_size - 1 do
    List.iter
      ~f:(fun (_, ec) -> Buffer.add_char interleaved (Bytes.get ec i))
      blocks
  done;
  Buffer.contents_bytes interleaved

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
  let blocks = split_into_blocks data ec_info in
  let final_data = interleave_blocks blocks ec_info in
  let qr = Qr.make ~version:config.version in
  Qr.place_pattern_modules qr config.version;
  let mask_pattern = 0 in
  Qr.place_format_info qr ~ecl:config.ecl ~mask_pattern;
  Qr.place_data qr final_data config.version;
  Qr.apply_mask_pattern qr;
  qr
