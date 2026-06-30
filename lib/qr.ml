open Base

type t = { buf : bytes; reserved : bytes; width : int }

let[@zero_alloc] size version = ((version - 1) * 4) + 21

let make ~version =
  let size = size version in
  { buf = Bytes.make (size * size) '\000'; reserved = Bytes.make (size * size) '\000'; width = size }

let[@zero_alloc] set_module (t @ local) x y value =
  if x >= 0 && x < t.width && y >= 0 && y < t.width then
    Bytes.set t.buf ((y * t.width) + x) value

(* place_finders/place_separators/place_timing_patterns/place_alignment_patterns/
   place_dark_module below bypass [set_module]'s runtime bounds check and write
   directly via [Bytes.unsafe_set], because each callsite's (x, y) is
   statically in range [0, t.width) by construction of the QR spec geometry
   (verified per-callsite; widths are always >= 21 since version >= 1):
   - place_finders: dx/dy in {0, w-7}, i/j in [0,6] => coords in [0,6] or
     [w-7,w-1], both within [0,w-1].
   - place_separators: i in [0,7]; w-8 >= 13 > 0; w-8+i <= w-1.
   - place_timing_patterns: i in [8, w-9] (w-9 >= 8 since w >= 21).
   - place_alignment_patterns: alignment_coords entries always lie in
     [6, w-7] (the last coordinate in every row of the table equals w-7),
     so coord +/- 2 lies in [4, w-5], within [0, w-1].
   - place_dark_module: y = 4*version+9 = w-8 exactly (algebraically, since
     w = (version-1)*4+21), so y in [13, w-1]. *)

let[@zero_alloc] place_finders (t @ local) =
  let w = t.width in
  let positions = stack_ [ (0, 0); (w - 7, 0); (0, w - 7) ] in
  for i = 0 to List.length positions - 1 do
    let dx, dy = List.nth_exn__local positions i in
    for i = 0 to 6 do
      for j = 0 to 6 do
        let is_border = i = 0 || i = 6 || j = 0 || j = 6 in
        let is_inner = i >= 2 && i <= 4 && j >= 2 && j <= 4 in
        let value = if is_border || is_inner then '\001' else '\000' in
        Bytes.unsafe_set t.buf (((dy + j) * w) + (dx + i)) value
      done
    done
  done

let[@zero_alloc] place_separators (t @ local) =
  let w = t.width in
  (* Around top-left finder *)
  for i = 0 to 7 do
    Bytes.unsafe_set t.buf ((7 * w) + i) '\000';
    Bytes.unsafe_set t.buf ((i * w) + 7) '\000'
  done;
  (* Around top-right finder *)
  for i = 0 to 7 do
    Bytes.unsafe_set t.buf ((7 * w) + (w - 8 + i)) '\000';
    Bytes.unsafe_set t.buf ((i * w) + (w - 8)) '\000'
  done;
  (* Around bottom-left finder *)
  for i = 0 to 7 do
    Bytes.unsafe_set t.buf (((w - 8) * w) + i) '\000';
    Bytes.unsafe_set t.buf (((w - 8 + i) * w) + 7) '\000'
  done

let[@zero_alloc] place_timing_patterns (t @ local) =
  let w = t.width in
  for i = 8 to w - 9 do
    let value = if (i - 8) % 2 = 0 then '\001' else '\000' in
    Bytes.unsafe_set t.buf ((6 * w) + i) value;
    Bytes.unsafe_set t.buf ((i * w) + 6) value
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

let[@zero_alloc] place_alignment_patterns (t @ local) version =
  (* Fused pass: writes both the module pattern (t.buf) and the reserved
     marker (t.reserved) for every alignment-pattern cell in a single
     traversal of the cartesian product of alignment_coords, instead of
     walking it twice (once here, once in mark_reserved). cx/cy +/- 2 are
     in-bounds; see the bounds note above place_finders. *)
  if version < 2 then ()
  else
    let w = t.width in
    let coords = Array.get alignment_coords (version - 1) in
    (* Imperative cartesian product to avoid allocating closures *)
    let cx_list = ref coords in
    while not (List.is_empty !cx_list) do
      match !cx_list with
      | [] -> ()
      | cx :: rest_cx ->
          let cy_list = ref coords in
          while not (List.is_empty !cy_list) do
            match !cy_list with
            | [] -> ()
            | cy :: rest_cy ->
                if
                  not
                    ((cx < 9 && cy < 9)
                    || (cx >= w - 8 && cy < 9)
                    || (cx < 9 && cy >= w - 8))
                then
                  for i = -2 to 2 do
                    for j = -2 to 2 do
                      let is_border = i = -2 || i = 2 || j = -2 || j = 2 in
                      let is_center = i = 0 && j = 0 in
                      let value =
                        if is_border || is_center then '\001' else '\000'
                      in
                      let idx = ((cy + j) * w) + (cx + i) in
                      Bytes.unsafe_set t.buf idx value;
                      Bytes.unsafe_set t.reserved idx '\001'
                    done
                  done;
                cy_list := rest_cy
          done;
          cx_list := rest_cx
    done

let[@zero_alloc] place_dark_module (t @ local) version =
  (* y = 4*version+9 == t.width-8 algebraically (width = (version-1)*4+21),
     so y is always in [13, t.width-1]; 8 is always in [0, t.width-1]. *)
  let y = (4 * version) + 9 in
  Bytes.unsafe_set t.buf ((y * t.width) + 8) '\001'

let[@zero_alloc] mark_reserved (t @ local) _version =
  (* Alignment-pattern reservations are now written by place_alignment_patterns
     in the same pass as their module values, so this function only needs to
     mark the corner regions and timing patterns; [_version] is unused. *)
  let w = t.width in
  (* Three 9×9 corner regions cover finders, separators, and format info.
     y,x range over [0,8] or [w-8,w-1], both within [0,w-1] since w >= 21. *)
  for y = 0 to 8 do
    for x = 0 to 8 do
      Bytes.unsafe_set t.reserved ((y * w) + x) '\001'
    done
  done;
  for y = 0 to 8 do
    for x = w - 8 to w - 1 do
      Bytes.unsafe_set t.reserved ((y * w) + x) '\001'
    done
  done;
  for y = w - 8 to w - 1 do
    for x = 0 to 8 do
      Bytes.unsafe_set t.reserved ((y * w) + x) '\001'
    done
  done;
  (* Timing patterns (between corner regions at row 6 and col 6); i in
     [8, w-9], in-bounds since w-9 >= 8 for w >= 21. *)
  for i = 8 to w - 9 do
    Bytes.unsafe_set t.reserved ((6 * w) + i) '\001';
    Bytes.unsafe_set t.reserved ((i * w) + 6) '\001'
  done

let[@zero_alloc] place_data (t @ local) data _version =
  let bit_pos = ref 0 in
  let data_bits = Bytes.length data * 8 in
  let x = ref (t.width - 1) in
  let upward = ref true in
  while !x > 0 && !bit_pos < data_bits do
    if !x = 6 then Int.decr x;
    if !x > 0 then (
      if !upward then
        for y = t.width - 1 downto 0 do
          for dx = 0 to 1 do
            let px = !x - dx in
            if Char.equal__local (Bytes.get t.reserved ((y * t.width) + px)) '\000' && !bit_pos < data_bits then (
              let byte_idx = !bit_pos / 8 in
              let bit_idx = 7 - (!bit_pos land 7) in
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
            if Char.equal__local (Bytes.get t.reserved ((y * t.width) + px)) '\000' && !bit_pos < data_bits then (
              let byte_idx = !bit_pos / 8 in
              let bit_idx = 7 - (!bit_pos land 7) in
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

let[@zero_alloc] place_data_and_apply_mask (t @ local) data _version =
  (* Combined pass: place data bits and apply mask pattern 0 in one scan.
     Mask 0 flips a module when (x+y) is even; XOR with (x+y+1 land 1) encodes that.

     Bounds proof for the unsafe t.buf/t.reserved accesses below: [w] is
     hoisted once. [y] always ranges over [0, w-1] in both the "downto" and
     "to" loops. [px = !x - dx] with dx in {0,1}; the outer while-loop only
     enters the body when [!x > 0] (checked just above, after the x=6 skip),
     and [!x] starts at [w-1] and only decreases, so 0 <= !x <= w-1 inside
     the body, hence 0 <= px <= w-1. So idx = y*w+px always lies in
     [0, w*w-1], i.e. within both t.buf and t.reserved (each sized w*w). *)
  let bit_pos = ref 0 in
  let data_bits = Bytes.length data * 8 in
  let w = t.width in
  let x = ref (w - 1) in
  let upward = ref true in
  while !x > 0 && !bit_pos < data_bits do
    if !x = 6 then Int.decr x;
    if !x > 0 then (
      if !upward then
        for y = w - 1 downto 0 do
          for dx = 0 to 1 do
            let px = !x - dx in
            let idx = (y * w) + px in
            if Char.equal__local (Bytes.unsafe_get t.reserved idx) '\000' && !bit_pos < data_bits then (
              let byte_idx = !bit_pos / 8 in
              let bit_idx = 7 - (!bit_pos land 7) in
              let bit = (Char.to_int (Bytes.get data byte_idx) lsr bit_idx) land 1 in
              let masked = bit lxor ((px + y + 1) land 1) in
              Bytes.unsafe_set t.buf idx (if masked = 0 then '\000' else '\001');
              Int.incr bit_pos)
          done
        done
      else
        for y = 0 to w - 1 do
          for dx = 0 to 1 do
            let px = !x - dx in
            let idx = (y * w) + px in
            if Char.equal__local (Bytes.unsafe_get t.reserved idx) '\000' && !bit_pos < data_bits then (
              let byte_idx = !bit_pos / 8 in
              let bit_idx = 7 - (!bit_pos land 7) in
              let bit = (Char.to_int (Bytes.get data byte_idx) lsr bit_idx) land 1 in
              let masked = bit lxor ((px + y + 1) land 1) in
              Bytes.unsafe_set t.buf idx (if masked = 0 then '\000' else '\001');
              Int.incr bit_pos)
          done
        done;
      x := !x - 2;
      upward := not !upward)
  done

let[@zero_alloc] place_pattern_modules (t @ local) version =
  place_finders t;
  place_separators t;
  place_timing_patterns t;
  place_alignment_patterns t version;
  place_dark_module t version;
  mark_reserved t version

let[@zero_alloc] ecl_format_bits (ecl @ local) =
  match ecl with
  | Config.ECL.L -> 0b01
  | Config.ECL.M -> 0b00
  | Config.ECL.Q -> 0b11
  | Config.ECL.H -> 0b10

let[@zero_alloc] compute_format_bits (ecl @ local) mask_pattern =
  (* Build 15-bit format string: 5 bits data (ECL||mask) + 10-bit BCH *)
  let data = (ecl_format_bits ecl lsl 3) lor mask_pattern in
  let v = stack_ (ref (data lsl 10)) in
  let generator = 0b10100110111 in
  for i = 14 downto 10 do
    if (!v lsr i) land 1 = 1 then v := !v lxor (generator lsl (i - 10))
  done;
  let bch = !v land 0x3FF in
  let raw = (data lsl 10) lor bch in
  raw lxor 0x5412 (* apply mask per spec *)

let[@zero_alloc] place_format_info (t @ local) ~ecl ~mask_pattern =
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

let[@zero_alloc] apply_mask_pattern (t @ local) =
  for y = 0 to t.width - 1 do
    for x = 0 to t.width - 1 do
      if Char.equal__local (Bytes.get t.reserved ((y * t.width) + x)) '\000' then
        let mask = (x + y) land 1 = 0 in
        if mask then begin
          let idx = (y * t.width) + x in
          let new_value =
            if Char.equal__local (Bytes.get t.buf idx) '\000' then '\001' else '\000'
          in
          Bytes.set t.buf idx new_value
        end
    done
  done

let to_unicode_string (t @ local) =
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
