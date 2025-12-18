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
  List.iter
    (fun (dx, dy) ->
      for i = 0 to 6 do
        for j = 0 to 6 do
          let is_border = i = 0 || i = 6 || j = 0 || j = 6 in
          let is_inner = i >= 2 && i <= 4 && j >= 2 && j <= 4 in
          let value = if is_border || is_inner then '\001' else '\000' in
          set_module t (dx + i) (dy + j) value
        done
      done)
    positions

let place_separators t =
  (* Horizontal separators *)
  for x = 0 to 7 do
    set_module t x 7 '\010';
    set_module t (t.width - 8 + x) 7 '\010'
  done;
  (* Vertical separators *)
  for y = 0 to 7 do
    set_module t 7 y '\010';
    set_module t 7 (t.width - 8 + y) '\010'
  done

let place_timing_patterns t =
  for i = 8 to t.width - 9 do
    let value = if (i - 8) mod 2 = 0 then '\001' else '\000' in
    set_module t i 6 value;
    set_module t 6 i value
  done

let alignment_coords =
  [|
    [];
    [];
    [ 6; 18 ];
    [ 6; 22 ];
    [ 6; 26 ];
    [ 6; 30; 34 ];
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
  else if version >= 0 && version < Array.length alignment_coords then
    let coords = Array.get alignment_coords version in
    (* Create Cartesian product of coordinates *)
    List.iter
      (fun cx ->
        List.iter
          (fun cy ->
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
              done)
          coords)
      coords

let place_dark_module t version =
  let y = (4 * version) + 9 in
  set_module t 8 y '\001'

let alignment_coords_for_version version =
  if version >= 0 && version < Array.length alignment_coords then
    Array.get alignment_coords version
  else []

let is_in_alignment_pattern t x y version =
  let coords = alignment_coords_for_version version in
  List.exists
    (fun cx ->
      List.exists
        (fun cy ->
          (* Check if (x, y) is within the 5x5 alignment pattern centered at (cx, cy) *)
          abs (x - cx) <= 2
          && abs (y - cy) <= 2
          && not
               ((* Don't consider alignment patterns that overlap finders *)
                (cx < 9 && cy < 9)
               || (cx >= t.width - 8 && cy < 9)
               || (cx < 9 && cy >= t.width - 8)))
        coords)
    coords

let is_reserved t x y version =
  (* Finders: three 7x7 squares *)
  (x < 9 && y < 9)
  || (x >= t.width - 8 && y < 9)
  || (x < 9 && y >= t.width - 8)
  (* Separators: white borders *)
  || (y = 7 && x < 9)
  || (y = 7 && x >= t.width - 8)
  || (x = 7 && y < 9)
  || (x = 7 && y >= t.width - 8)
  || (x = 7 && y = 7)
  || (x = t.width - 8 && y >= t.width - 8)
  || (y = t.width - 8 && x >= t.width - 8)
  (* Timing patterns: rows/cols 6 *)
  || (x = 6 && y >= 8 && y < t.width - 8)
  || (y = 6 && x >= 8 && x < t.width - 8)
  (* Alignment patterns *)
  || is_in_alignment_pattern t x y version
  (* Format info: row 8 and col 8 *)
  || (y = 8 && (x < 9 || x >= t.width - 8))
  || (x = 8 && (y < 9 || y >= t.width - 8))

let place_data t data version =
  let bit_pos = ref 0 in
  let data_bits = Bitbuf.bits_written data in
  let data_bytes = Bitbuf.to_bytes data in
  (* Scan right-to-left, bottom-to-top in 2-column strips *)
  let x = ref (t.width - 1) in
  while !x > 0 do
    for y = t.width - 1 downto 0 do
      for dx = 0 to 1 do
        let px = !x - dx in
        if (not (is_reserved t px y version)) && !bit_pos < data_bits then (
          let byte_idx = !bit_pos / 8 in
          let bit_idx = 7 - (!bit_pos mod 8) in
          let bit =
            (int_of_char (Bytes.get data_bytes byte_idx) lsr bit_idx) land 1
          in
          let value = if bit = 1 then '\001' else '\000' in
          set_module t px y value;
          incr bit_pos)
      done
    done;
    x := !x - 2
  done

let place_pattern_modules t version =
  place_finders t;
  place_separators t;
  place_timing_patterns t;
  place_alignment_patterns t version;
  place_dark_module t version

let to_unicode_string t =
  let rtrim s =
    let rec find i =
      if i < 0 then ""
      else if s.[i] = ' ' then find (i - 1)
      else String.sub s 0 (i + 1)
    in
    if String.length s = 0 then "" else find (String.length s - 1)
  in
  let buf = Buffer.create (t.width * t.width * 3) in
  for y = 0 to t.width - 1 do
    let line_buf = Buffer.create (t.width * 2) in
    for x = 0 to t.width - 1 do
      let cell = Bytes.get t.buf ((y * t.width) + x) in
      Buffer.add_string line_buf (if cell <> '\000' then "██" else "  ")
    done;
    let line = Buffer.contents line_buf in
    let trimmed = rtrim line in
    Buffer.add_string buf trimmed;
    Buffer.add_char buf '\n'
  done;
  Buffer.contents buf
