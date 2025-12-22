module ECL = struct
  type t = L | M | Q | H

  let compare e1 e2 =
    let to_int = function L -> 0 | M -> 1 | Q -> 2 | H -> 3 in
    Int.compare (to_int e1) (to_int e2)
end

type t = { version : int; ecl : ECL.t }

let make ~version ~ecl =
  assert (version >= 1 && version <= 40);
  { version; ecl }

let compare c1 c2 =
  let ver_cmp = Int.compare c1.version c2.version in
  if ver_cmp <> 0 then ver_cmp else ECL.compare c1.ecl c2.ecl

let char_count_indicator_length t =
  match t.version with
  | v when v >= 1 && v <= 9 -> 9
  | v when v >= 10 && v <= 26 -> 11
  | v when v >= 27 && v <= 40 -> 13
  | _ -> raise (Invalid_argument "Invalid version number")

module ConfigMap = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

(* Alphanumeric mode *)
let capacity_table : int ConfigMap.t =
  let add_entry map (version, ecl, capacity) =
    ConfigMap.add { version; ecl } capacity map
  in
  let open ECL in
  let entries =
    [
      (* Version 1 *)
      (1, L, 25);
      (1, M, 20);
      (1, Q, 16);
      (1, H, 10);
      (* Version 2 *)
      (2, L, 47);
      (2, M, 38);
      (2, Q, 29);
      (2, H, 20);
      (* Version 3 *)
      (3, L, 77);
      (3, M, 61);
      (3, Q, 47);
      (3, H, 35);
      (* Version 4 *)
      (4, L, 114);
      (4, M, 90);
      (4, Q, 67);
      (4, H, 50);
      (* Version 5 *)
      (5, L, 154);
      (5, M, 122);
      (5, Q, 87);
      (5, H, 64);
      (* Version 6 *)
      (6, L, 195);
      (6, M, 154);
      (6, Q, 108);
      (6, H, 84);
      (* Version 7 *)
      (7, L, 224);
      (7, M, 178);
      (7, Q, 125);
      (7, H, 93);
      (* Version 8 *)
      (8, L, 279);
      (8, M, 221);
      (8, Q, 157);
      (8, H, 122);
      (* Version 9 *)
      (9, L, 335);
      (9, M, 262);
      (9, Q, 189);
      (9, H, 143);
      (* Version 10 *)
      (10, L, 395);
      (10, M, 311);
      (10, Q, 221);
      (10, H, 174);
      (* Version 11 *)
      (11, L, 468);
      (11, M, 366);
      (11, Q, 259);
      (11, H, 200);
      (* Version 12 *)
      (12, L, 535);
      (12, M, 419);
      (12, Q, 296);
      (12, H, 227);
      (* Version 13 *)
      (13, L, 619);
      (13, M, 483);
      (13, Q, 352);
      (13, H, 259);
      (* Version 14 *)
      (14, L, 667);
      (14, M, 528);
      (14, Q, 376);
      (14, H, 283);
      (* Version 15 *)
      (15, L, 758);
      (15, M, 600);
      (15, Q, 426);
      (15, H, 321);
      (* Version 16 *)
      (16, L, 854);
      (16, M, 656);
      (16, Q, 470);
      (16, H, 365);
      (* Version 17 *)
      (17, L, 938);
      (17, M, 734);
      (17, Q, 531);
      (17, H, 408);
      (* Version 18 *)
      (18, L, 1046);
      (18, M, 816);
      (18, Q, 574);
      (18, H, 452);
      (* Version 19 *)
      (19, L, 1153);
      (19, M, 909);
      (19, Q, 644);
      (19, H, 493);
      (* Version 20 *)
      (20, L, 1249);
      (20, M, 970);
      (20, Q, 702);
      (20, H, 557);
      (* Version 21 *)
      (21, L, 1352);
      (21, M, 1035);
      (21, Q, 742);
      (21, H, 587);
      (* Version 22 *)
      (22, L, 1460);
      (22, M, 1134);
      (22, Q, 823);
      (22, H, 640);
      (* Version 23 *)
      (23, L, 1588);
      (23, M, 1248);
      (23, Q, 890);
      (23, H, 672);
      (* Version 24 *)
      (24, L, 1704);
      (24, M, 1326);
      (24, Q, 963);
      (24, H, 744);
      (* Version 25 *)
      (25, L, 1853);
      (25, M, 1451);
      (25, Q, 1041);
      (25, H, 779);
      (* Version 26 *)
      (26, L, 1990);
      (26, M, 1542);
      (26, Q, 1094);
      (26, H, 864);
      (* Version 27 *)
      (27, L, 2132);
      (27, M, 1637);
      (27, Q, 1172);
      (27, H, 910);
      (* Version 28 *)
      (28, L, 2223);
      (28, M, 1732);
      (28, Q, 1263);
      (28, H, 958);
      (* Version 29 *)
      (29, L, 2369);
      (29, M, 1839);
      (29, Q, 1322);
      (29, H, 1016);
      (* Version 30 *)
      (30, L, 2520);
      (30, M, 1994);
      (30, Q, 1429);
      (30, H, 1080);
      (* Version 31 *)
      (31, L, 2677);
      (31, M, 2113);
      (31, Q, 1499);
      (31, H, 1150);
      (* Version 32 *)
      (32, L, 2840);
      (32, M, 2238);
      (32, Q, 1618);
      (32, H, 1226);
      (* Version 33 *)
      (33, L, 3009);
      (33, M, 2369);
      (33, Q, 1700);
      (33, H, 1307);
      (* Version 34 *)
      (34, L, 3183);
      (34, M, 2506);
      (34, Q, 1787);
      (34, H, 1394);
      (* Version 35 *)
      (35, L, 3351);
      (35, M, 2632);
      (35, Q, 1867);
      (35, H, 1431);
      (* Version 36 *)
      (36, L, 3537);
      (36, M, 2780);
      (36, Q, 1966);
      (36, H, 1530);
      (* Version 37 *)
      (37, L, 3729);
      (37, M, 2894);
      (37, Q, 2071);
      (37, H, 1591);
      (* Version 38 *)
      (38, L, 3927);
      (38, M, 3054);
      (38, Q, 2181);
      (38, H, 1658);
      (* Version 39 *)
      (39, L, 4087);
      (39, M, 3220);
      (39, Q, 2298);
      (39, H, 1774);
      (* Version 40 *)
      (40, L, 4296);
      (40, M, 3391);
      (40, Q, 2420);
      (40, H, 1852);
    ]
  in
  List.fold_left add_entry ConfigMap.empty entries

let get_capacity config = ConfigMap.find config capacity_table
let mode_indicator_length = 4
let mode_indicator = 0b0010

let get_config_and_capacity s ecl =
  let length = String.length s in
  let rec find_version v =
    if v > 40 then raise (Invalid_argument "Data too long to encode in QR code")
    else
      let config = make ~version:v ~ecl in
      let capacity = get_capacity config in
      if length <= capacity then (config, capacity) else find_version (v + 1)
  in
  find_version 1
