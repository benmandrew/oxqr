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

let place_alignment_patterns t version =
  if version < 2 then () (* Version 1 has no alignment patterns *)
  else
    (* Alignment pattern coordinates for each version *)
    let alignment_coords =
      [
        [];
        (* version 0 - unused *)
        [];
        (* version 1 - none *)
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
      ]
    in

    if version >= 0 && version < List.length alignment_coords then
      let coords = List.nth alignment_coords version in
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

let place_pattern_modules t version =
  place_finders t;
  place_separators t;
  place_timing_patterns t;
  place_alignment_patterns t version;
  place_dark_module t version

let to_unicode_string t =
  let buf = Buffer.create (t.width * t.width * 3) in
  for y = 0 to t.width - 1 do
    for x = 0 to t.width - 1 do
      let cell = Bytes.get t.buf ((y * t.width) + x) in
      Buffer.add_string buf (if cell <> '\000' then "██" else "  ")
    done;
    Buffer.add_char buf '\n'
  done;
  Buffer.contents buf
