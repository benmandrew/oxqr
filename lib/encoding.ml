let encode_alphanumeric_data buf s =
  let rec encode_pairs i =
    if i >= String.length s then ()
    else if i = String.length s - 1 then
      (* Single character remaining: encode as 6 bits *)
      let value = Config.alphanumeric_encode s.[i] in
      Bitbuf.write_bits_msb buf ~value ~width:6
    else
      (* Encode pair of characters as 11 bits *)
      let v1 = Config.alphanumeric_encode s.[i] in
      let v2 = Config.alphanumeric_encode s.[i + 1] in
      let value = (v1 * 45) + v2 in
      Bitbuf.write_bits_msb buf ~value ~width:11;
      encode_pairs (i + 2)
  in
  encode_pairs 0

let add_terminator_and_padding buf total_data_codewords =
  (* Add terminator (0000, up to 4 bits) *)
  let bits_used = Bitbuf.bits_written buf in
  let max_bits = total_data_codewords * 8 in
  let terminator_bits = min 4 (max_bits - bits_used) in
  if terminator_bits > 0 then
    Bitbuf.write_bits_msb buf ~value:0 ~width:terminator_bits;
  (* Pad to byte boundary *)
  let bits_after_term = Bitbuf.bits_written buf in
  let pad_to_byte = (8 - (bits_after_term mod 8)) mod 8 in
  if pad_to_byte > 0 then Bitbuf.write_bits_msb buf ~value:0 ~width:pad_to_byte;
  (* Add padding bytes (alternating 0xEC and 0x11) *)
  let bytes_used = Bitbuf.bits_written buf / 8 in
  let rec add_padding i =
    if i >= total_data_codewords then ()
    else
      let pad_byte = if (i - bytes_used) mod 2 = 0 then 0xEC else 0x11 in
      Bitbuf.write_byte buf pad_byte;
      add_padding (i + 1)
  in
  add_padding bytes_used

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
    let block_data = Bytes.sub data !offset g1_size in
    let ec_data =
      Reed_solomon.generate_error_correction block_data ec_per_block
    in
    blocks := (block_data, ec_data) :: !blocks;
    offset := !offset + g1_size
  done;
  (* Group 2 blocks *)
  for _ = 1 to g2_count do
    let block_data = Bytes.sub data !offset g2_size in
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
      (fun (data, _) ->
        if i < Bytes.length data then
          Buffer.add_char interleaved (Bytes.get data i))
      blocks
  done;
  (* Interleave error correction codewords *)
  for i = 0 to ec_size - 1 do
    List.iter
      (fun (_, ec) -> Buffer.add_char interleaved (Bytes.get ec i))
      blocks
  done;
  Buffer.to_bytes interleaved

let format_output config data blocks final_data =
  let total_data_codewords = Bytes.length data in
  let total_ec = List.length blocks * (List.hd blocks |> snd |> Bytes.length) in
  Printf.sprintf
    "Version %d-%s, %d data + %d EC = %d bytes:\nData: %s\nEC:   %s\nFinal: %s"
    config.Config.version
    (match config.ecl with
    | Config.ECL.L -> "L"
    | M -> "M"
    | Q -> "Q"
    | H -> "H")
    total_data_codewords total_ec (Bytes.length final_data)
    (Bytes.to_seq data
    |> Seq.map (fun c -> Printf.sprintf "%02X" (Char.code c))
    |> List.of_seq |> String.concat " ")
    (List.map
       (fun (_, ec) ->
         Bytes.to_seq ec
         |> Seq.map (fun c -> Printf.sprintf "%02X" (Char.code c))
         |> List.of_seq |> String.concat " ")
       blocks
    |> String.concat " | ")
    (Bytes.to_seq final_data
    |> Seq.map (fun c -> Printf.sprintf "%02X" (Char.code c))
    |> List.of_seq |> String.concat " ")

let generate s ecl =
  let config, _capacity = Config.get_config_and_capacity s ecl in
  let ec_info = Config.get_ec_info config in
  let total_data_codewords =
    (ec_info.group1_blocks * ec_info.group1_data_codewords)
    + (ec_info.group2_blocks * ec_info.group2_data_codewords)
  in
  let buf = Bitbuf.create total_data_codewords in
  Bitbuf.write_bits_msb buf ~value:Config.mode_indicator
    ~width:Config.mode_indicator_length;
  let cci_len = Config.char_count_indicator_length config in
  Bitbuf.write_bits_msb buf ~value:(String.length s) ~width:cci_len;
  encode_alphanumeric_data buf s;
  add_terminator_and_padding buf total_data_codewords;
  let data = Bitbuf.to_bytes buf in
  let blocks = split_into_blocks data ec_info in
  let final_data = interleave_blocks blocks ec_info in
  (* Create QR code matrix and place data *)
  let qr = Qr.make ~version:config.version in
  Qr.place_pattern_modules qr config.version;
  (* TODO: choose best mask; for now use mask pattern 0 *)
  let mask_pattern = 0 in
  Qr.place_format_info qr ~ecl:config.ecl ~mask_pattern;
  Qr.place_data qr final_data config.version;
  qr

let generate_debug s ecl =
  let config, _capacity = Config.get_config_and_capacity s ecl in
  let ec_info = Config.get_ec_info config in
  let total_data_codewords =
    (ec_info.group1_blocks * ec_info.group1_data_codewords)
    + (ec_info.group2_blocks * ec_info.group2_data_codewords)
  in
  let buf = Bitbuf.create total_data_codewords in
  Bitbuf.write_bits_msb buf ~value:Config.mode_indicator
    ~width:Config.mode_indicator_length;
  let cci_len = Config.char_count_indicator_length config in
  Bitbuf.write_bits_msb buf ~value:(String.length s) ~width:cci_len;
  encode_alphanumeric_data buf s;
  add_terminator_and_padding buf total_data_codewords;
  let data = Bitbuf.to_bytes buf in
  let blocks = split_into_blocks data ec_info in
  let final_data = interleave_blocks blocks ec_info in
  format_output config data blocks final_data
