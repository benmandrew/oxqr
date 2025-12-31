open Base

type t = { buf : bytes; width : int }

let size version = ((version - 1) * 4) + 21

let make ~version =
  let size = size version in
  { buf = Bytes.make (size * size) '\000'; width = size }

let set_module t x y value =
  if x >= 0 && x < t.width && y >= 0 && y < t.width then
    Bytes.set t.buf ((y * t.width) + x) value

let place_finders t =
  let positions = [ (0, 0); (t.width - 7, 0); (0, t.width - 7) ] in
  List.iter positions ~f:(fun (dx, dy) ->
      for i = 0 to 6 do
        for j = 0 to 6 do
          let is_border = i = 0 || i = 6 || j = 0 || j = 6 in
          let is_inner = i >= 2 && i <= 4 && j >= 2 && j <= 4 in
          let value = if is_border || is_inner then '\001' else '\000' in
          set_module t (dx + i) (dy + j) value
        done
      done)

let place_separators t =
  (* Around top-left finder *)
  for i = 0 to 7 do
    set_module t i 7 '\000';
    set_module t 7 i '\000'
  done;
  (* Around top-right finder *)
  for i = 0 to 7 do
    set_module t (t.width - 8 + i) 7 '\000';
    set_module t (t.width - 8) i '\000'
  done;
  (* Around bottom-left finder *)
  for i = 0 to 7 do
    set_module t i (t.width - 8) '\000';
    set_module t 7 (t.width - 8 + i) '\000'
  done

let place_timing_patterns t =
  for i = 8 to t.width - 9 do
    let value = if (i - 8) % 2 = 0 then '\001' else '\000' in
    set_module t i 6 value;
    set_module t 6 i value
  done

let alignment_coords =
  [|
    [];
    [ 6; 18 ];
    [ 6; 22 ];
    [ 6; 26 ];
    [ 6; 30 ];
    [ 6; 34 ];
    [ 6; 22; 38 ];
    [ 6; 24; 42 ];
    [ 6; 26; 46 ];
    [ 6; 28; 50 ];
    [ 6; 30; 54 ];
    [ 6; 32; 58 ];
    [ 6; 34; 62 ];
    [ 6; 26; 46; 66 ];
    [ 6; 26; 48; 70 ];
    [ 6; 26; 50; 74 ];
    [ 6; 30; 54; 78 ];
    [ 6; 30; 56; 82 ];
    [ 6; 30; 58; 86 ];
    [ 6; 34; 62; 90 ];
    [ 6; 28; 50; 72; 94 ];
    [ 6; 26; 50; 74; 98 ];
    [ 6; 30; 54; 78; 102 ];
    [ 6; 28; 54; 80; 106 ];
    [ 6; 32; 58; 84; 110 ];
    [ 6; 30; 58; 86; 114 ];
    [ 6; 34; 62; 90; 118 ];
    [ 6; 26; 50; 74; 98; 122 ];
    [ 6; 30; 54; 78; 102; 126 ];
    [ 6; 26; 52; 78; 104; 130 ];
    [ 6; 30; 56; 82; 108; 134 ];
    [ 6; 34; 60; 86; 112; 138 ];
    [ 6; 30; 58; 86; 114; 142 ];
    [ 6; 34; 62; 90; 118; 146 ];
    [ 6; 30; 54; 78; 102; 126; 150 ];
    [ 6; 24; 50; 76; 102; 128; 154 ];
    [ 6; 28; 54; 80; 106; 132; 158 ];
    [ 6; 32; 58; 84; 110; 136; 162 ];
    [ 6; 26; 54; 82; 110; 138; 166 ];
    [ 6; 30; 58; 86; 114; 142; 170 ];
  |]

let place_alignment_patterns t version =
  if version < 2 then ()
  else
    let coords = Array.get alignment_coords (version - 1) in
    (* Create Cartesian product of coordinates *)
    List.iter coords ~f:(fun cx ->
        List.iter coords ~f:(fun cy ->
            (* Don't place over finders *)
            if
              not
                ((cx < 9 && cy < 9)
                || (cx >= t.width - 8 && cy < 9)
                || (cx < 9 && cy >= t.width - 8))
            then
              (* Draw 5x5 alignment pattern centered at (cx, cy) *)
              for i = -2 to 2 do
                for j = -2 to 2 do
                  let is_border = i = -2 || i = 2 || j = -2 || j = 2 in
                  let is_center = i = 0 && j = 0 in
                  let value =
                    if is_border || is_center then '\001' else '\000'
                  in
                  set_module t (cx + i) (cy + j) value
                done
              done))

let place_dark_module t version =
  let y = (4 * version) + 9 in
  set_module t 8 y '\001'

let alignment_coords_for_version version =
  if version >= 1 && version <= 40 then Array.get alignment_coords (version - 1)
  else []

let is_in_alignment_pattern t x y version =
  let coords = alignment_coords_for_version version in
  List.exists coords ~f:(fun cx ->
      List.exists coords ~f:(fun cy ->
          (* Check if (x, y) is within the 5x5 alignment pattern centered at (cx, cy) *)
          abs (x - cx) <= 2
          && abs (y - cy) <= 2
          && not
               ((* Don't consider alignment patterns that overlap finders *)
                (cx < 9 && cy < 9)
               || (cx >= t.width - 8 && cy < 9)
               || (cx < 9 && cy >= t.width - 8))))

let is_reserved t x y version =
  let in_top_left = x <= 8 && y <= 8 in
  let in_top_right = x >= t.width - 8 && y <= 8 in
  let in_bottom_left = x <= 8 && y >= t.width - 8 in
  let in_finder_or_sep = in_top_left || in_top_right || in_bottom_left in
  let on_timing_row = y = 6 && x >= 8 && x <= t.width - 9 in
  let on_timing_col = x = 6 && y >= 8 && y <= t.width - 9 in
  let on_format_info =
    (y = 8 && (x <= 8 || x >= t.width - 8))
    || (x = 8 && (y <= 8 || y >= t.width - 8))
  in
  in_finder_or_sep || on_timing_row || on_timing_col
  || is_in_alignment_pattern t x y version
  || on_format_info

let place_data t data version =
  let bit_pos = ref 0 in
  let data_bits = Bytes.length data * 8 in
  (* Scan right-to-left in 2-column strips, alternating vertical direction per spec *)
  let x = ref (t.width - 1) in
  let upward = ref true in
  while !x > 0 && !bit_pos < data_bits do
    if !x = 6 then Int.decr x;
    (* skip vertical timing column *)
    if !x > 0 then (
      if !upward then
        for y = t.width - 1 downto 0 do
          for dx = 0 to 1 do
            let px = !x - dx in
            if (not (is_reserved t px y version)) && !bit_pos < data_bits then (
              let byte_idx = !bit_pos / 8 in
              let bit_idx = 7 - (!bit_pos % 8) in
              let bit =
                (Char.to_int (Bytes.get data byte_idx) lsr bit_idx) land 1
              in
              let value = if bit = 1 then '\001' else '\000' in
              set_module t px y value;
              Int.incr bit_pos)
          done
        done
      else
        for y = 0 to t.width - 1 do
          for dx = 0 to 1 do
            let px = !x - dx in
            if (not (is_reserved t px y version)) && !bit_pos < data_bits then (
              let byte_idx = !bit_pos / 8 in
              let bit_idx = 7 - (!bit_pos % 8) in
              let bit =
                (Char.to_int (Bytes.get data byte_idx) lsr bit_idx) land 1
              in
              let value = if bit = 1 then '\001' else '\000' in
              set_module t px y value;
              Int.incr bit_pos)
          done
        done;
      x := !x - 2;
      upward := not !upward)
  done

let place_pattern_modules t version =
  place_finders t;
  place_separators t;
  place_timing_patterns t;
  place_alignment_patterns t version;
  place_dark_module t version

let ecl_format_bits = function
  | Config.ECL.L -> 0b01
  | Config.ECL.M -> 0b00
  | Config.ECL.Q -> 0b11
  | Config.ECL.H -> 0b10

let compute_format_bits ecl mask_pattern =
  (* Build 15-bit format string: 5 bits data (ECL||mask) + 10-bit BCH *)
  let data = (ecl_format_bits ecl lsl 3) lor mask_pattern in
  let v = ref (data lsl 10) in
  let generator = 0b10100110111 in
  for i = 14 downto 10 do
    if (!v lsr i) land 1 = 1 then v := !v lxor (generator lsl (i - 10))
  done;
  let bch = !v land 0x3FF in
  let raw = (data lsl 10) lor bch in
  raw lxor 0x5412 (* apply mask per spec *)

let place_format_info t ~ecl ~mask_pattern =
  let bits = compute_format_bits ecl mask_pattern in
  let bit i = if (bits lsr i) land 1 = 1 then '\001' else '\000' in
  (* First copy around top-left finder (bit0 is LSB) *)
  set_module t 8 0 (bit 0);
  set_module t 8 1 (bit 1);
  set_module t 8 2 (bit 2);
  set_module t 8 3 (bit 3);
  set_module t 8 4 (bit 4);
  set_module t 8 5 (bit 5);
  set_module t 8 7 (bit 6);
  set_module t 8 8 (bit 7);
  set_module t 7 8 (bit 8);
  set_module t 5 8 (bit 9);
  set_module t 4 8 (bit 10);
  set_module t 3 8 (bit 11);
  set_module t 2 8 (bit 12);
  set_module t 1 8 (bit 13);
  set_module t 0 8 (bit 14);
  (* Second copy: top-right then bottom-left column *)
  let w = t.width in
  set_module t (w - 1) 8 (bit 0);
  set_module t (w - 2) 8 (bit 1);
  set_module t (w - 3) 8 (bit 2);
  set_module t (w - 4) 8 (bit 3);
  set_module t (w - 5) 8 (bit 4);
  set_module t (w - 6) 8 (bit 5);
  set_module t (w - 7) 8 (bit 6);
  set_module t (w - 8) 8 (bit 7);
  set_module t 8 (w - 7) (bit 8);
  set_module t 8 (w - 6) (bit 9);
  set_module t 8 (w - 5) (bit 10);
  set_module t 8 (w - 4) (bit 11);
  set_module t 8 (w - 3) (bit 12);
  set_module t 8 (w - 2) (bit 13);
  set_module t 8 (w - 1) (bit 14)

let apply_mask_pattern t =
  for y = 0 to t.width - 1 do
    for x = 0 to t.width - 1 do
      if not (is_reserved t x y 1) then
        let mask = (x + y) % 2 = 0 (* Mask pattern 0: (x + y) % 2 = 0 *) in
        if mask then
          let current = Bytes.get t.buf ((y * t.width) + x) in
          let new_value =
            if Char.equal__local current '\000' then '\001' else '\000'
          in
          Bytes.set t.buf ((y * t.width) + x) new_value
    done
  done

let to_unicode_string t =
  let rtrim s =
    let rec find i =
      if i < 0 then ""
      else if Char.equal__local s.[i] ' ' then find (i - 1)
      else String.sub s ~pos:0 ~len:(i + 1)
    in
    if String.length s = 0 then "" else find (String.length s - 1)
  in
  let quiet_zone = 4 in
  let buf = Buffer.create (t.width * t.width * 3) in
  for y = -quiet_zone to t.width + quiet_zone - 1 do
    let line_buf = Buffer.create ((t.width + (2 * quiet_zone)) * 2) in
    for x = -quiet_zone to t.width + quiet_zone - 1 do
      let cell =
        if x < 0 || x >= t.width || y < 0 || y >= t.width then '\000'
        else Bytes.get t.buf ((y * t.width) + x)
      in
      Buffer.add_string line_buf
        (if not (Char.equal__local cell '\000') then "  " else "██")
    done;
    let line = Buffer.contents line_buf in
    let trimmed = rtrim line in
    Buffer.add_string buf trimmed;
    Buffer.add_char buf '\n'
  done;
  Buffer.contents buf
