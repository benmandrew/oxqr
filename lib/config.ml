open Base

module ECL = struct
  type t = L | M | Q | H [@@deriving sexp_of, compare, hash]
end

module T = struct
  type t = { version : int; ecl : ECL.t } [@@deriving sexp_of, compare, hash]
end

include T

let make_local ~version ~(ecl @ local) =
  assert (version >= 1 && version <= 40);
  exclave_ { version; ecl }

let make ~version ~(ecl @ local) =
  assert (version >= 1 && version <= 40);
  { version; ecl }

let[@zero_alloc] char_count_indicator_length t =
  match t.version with
  | v when v >= 1 && v <= 9 -> 9
  | v when v >= 10 && v <= 26 -> 11
  | v when v >= 27 && v <= 40 -> 13
  | _ -> raise (Invalid_argument "Invalid version number")

let[@zero_alloc] alphanumeric_encode c =
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
  | _ -> raise (Invalid_argument "Invalid alphanumeric character")

let[@zero_alloc] alphanumeric_encode_res c = exclave_
  match c with
  | '0' .. '9' -> Ok (Char.to_int c - Char.to_int '0')
  | 'A' .. 'Z' -> Ok (Char.to_int c - Char.to_int 'A' + 10)
  | ' ' -> Ok 36
  | '$' -> Ok 37
  | '%' -> Ok 38
  | '*' -> Ok 39
  | '+' -> Ok 40
  | '-' -> Ok 41
  | '.' -> Ok 42
  | '/' -> Ok 43
  | ':' -> Ok 44
  | _ -> Error "Invalid alphanumeric character"

type ec_info = {
  ec_codewords_per_block : int;
  group1_blocks : int;
  group1_data_codewords : int;
  group2_blocks : int;
  group2_data_codewords : int;
}

(* Alphanumeric mode *)
let capacity_table =
  let hash_table = Hashtbl.create (module T) in
  let add_entry (version, ecl, capacity) =
    Hashtbl.set hash_table ~key:{ version; ecl } ~data:capacity
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
  List.iter entries ~f:add_entry;
  hash_table

let ec_table =
  let hash_table = Hashtbl.create (module T) in
  let add_entry
      (version, ecl, ec_per_block, g1_blocks, g1_data, g2_blocks, g2_data) =
    Hashtbl.set hash_table ~key:{ version; ecl }
      ~data:
        {
          ec_codewords_per_block = ec_per_block;
          group1_blocks = g1_blocks;
          group1_data_codewords = g1_data;
          group2_blocks = g2_blocks;
          group2_data_codewords = g2_data;
        }
  in
  let open ECL in
  let entries =
    [
      (* Version 1 *)
      (1, L, 7, 1, 19, 0, 0);
      (1, M, 10, 1, 16, 0, 0);
      (1, Q, 13, 1, 13, 0, 0);
      (1, H, 17, 1, 9, 0, 0);
      (* Version 2 *)
      (2, L, 10, 1, 34, 0, 0);
      (2, M, 16, 1, 28, 0, 0);
      (2, Q, 22, 1, 22, 0, 0);
      (2, H, 28, 1, 16, 0, 0);
      (* Version 3 *)
      (3, L, 15, 1, 55, 0, 0);
      (3, M, 26, 1, 44, 0, 0);
      (3, Q, 18, 2, 17, 0, 0);
      (3, H, 22, 2, 13, 0, 0);
      (* Version 4 *)
      (4, L, 20, 1, 80, 0, 0);
      (4, M, 18, 2, 32, 0, 0);
      (4, Q, 26, 2, 24, 0, 0);
      (4, H, 16, 4, 9, 0, 0);
      (* Version 5 *)
      (5, L, 26, 1, 108, 0, 0);
      (5, M, 24, 2, 43, 0, 0);
      (5, Q, 18, 2, 15, 2, 16);
      (5, H, 22, 2, 11, 2, 12);
      (* Version 6 *)
      (6, L, 18, 2, 68, 0, 0);
      (6, M, 16, 4, 27, 0, 0);
      (6, Q, 24, 4, 19, 0, 0);
      (6, H, 28, 4, 15, 0, 0);
      (* Version 7 *)
      (7, L, 20, 2, 78, 0, 0);
      (7, M, 18, 4, 31, 0, 0);
      (7, Q, 18, 2, 14, 4, 15);
      (7, H, 26, 4, 13, 1, 14);
      (* Version 8 *)
      (8, L, 24, 2, 97, 0, 0);
      (8, M, 22, 2, 38, 2, 39);
      (8, Q, 22, 4, 18, 2, 19);
      (8, H, 26, 4, 14, 2, 15);
      (* Version 9 *)
      (9, L, 30, 2, 116, 0, 0);
      (9, M, 22, 3, 36, 2, 37);
      (9, Q, 20, 4, 16, 4, 17);
      (9, H, 24, 4, 12, 4, 13);
      (* Version 10 *)
      (10, L, 18, 2, 68, 2, 69);
      (10, M, 26, 4, 43, 1, 44);
      (10, Q, 24, 6, 19, 2, 20);
      (10, H, 28, 6, 15, 2, 16);
      (* Version 11 *)
      (11, L, 20, 4, 81, 0, 0);
      (11, M, 30, 1, 50, 4, 51);
      (11, Q, 28, 4, 22, 4, 23);
      (11, H, 24, 3, 12, 8, 13);
      (* Version 12 *)
      (12, L, 24, 2, 92, 2, 93);
      (12, M, 22, 6, 36, 2, 37);
      (12, Q, 26, 4, 20, 6, 21);
      (12, H, 28, 7, 14, 4, 15);
      (* Version 13 *)
      (13, L, 26, 4, 107, 0, 0);
      (13, M, 22, 8, 37, 1, 38);
      (13, Q, 24, 8, 20, 4, 21);
      (13, H, 22, 12, 11, 4, 12);
      (* Version 14 *)
      (14, L, 30, 3, 115, 1, 116);
      (14, M, 24, 4, 40, 5, 41);
      (14, Q, 20, 11, 16, 5, 17);
      (14, H, 24, 11, 12, 5, 13);
      (* Version 15 *)
      (15, L, 22, 5, 87, 1, 88);
      (15, M, 24, 5, 41, 5, 42);
      (15, Q, 30, 5, 24, 7, 25);
      (15, H, 24, 11, 12, 7, 13);
      (* Version 16 *)
      (16, L, 24, 5, 98, 1, 99);
      (16, M, 28, 7, 45, 3, 46);
      (16, Q, 24, 15, 19, 2, 20);
      (16, H, 30, 3, 15, 13, 16);
      (* Version 17 *)
      (17, L, 28, 1, 107, 5, 108);
      (17, M, 28, 10, 46, 1, 47);
      (17, Q, 28, 1, 22, 15, 23);
      (17, H, 28, 2, 14, 17, 15);
      (* Version 18 *)
      (18, L, 30, 5, 120, 1, 121);
      (18, M, 26, 9, 43, 4, 44);
      (18, Q, 28, 17, 22, 1, 23);
      (18, H, 28, 2, 14, 19, 15);
      (* Version 19 *)
      (19, L, 28, 3, 113, 4, 114);
      (19, M, 26, 3, 44, 11, 45);
      (19, Q, 26, 17, 21, 4, 22);
      (19, H, 26, 9, 13, 16, 14);
      (* Version 20 *)
      (20, L, 28, 3, 107, 5, 108);
      (20, M, 26, 3, 41, 13, 42);
      (20, Q, 30, 15, 24, 5, 25);
      (20, H, 28, 15, 15, 10, 16);
      (* Version 21 *)
      (21, L, 28, 4, 116, 4, 117);
      (21, M, 26, 17, 42, 0, 0);
      (21, Q, 28, 17, 22, 6, 23);
      (21, H, 30, 19, 16, 6, 17);
      (* Version 22 *)
      (22, L, 28, 2, 111, 7, 112);
      (22, M, 28, 17, 46, 0, 0);
      (22, Q, 30, 7, 24, 16, 25);
      (22, H, 24, 34, 13, 0, 0);
      (* Version 23 *)
      (23, L, 30, 4, 121, 5, 122);
      (23, M, 28, 4, 47, 14, 48);
      (23, Q, 30, 11, 24, 14, 25);
      (23, H, 30, 16, 15, 14, 16);
      (* Version 24 *)
      (24, L, 30, 6, 117, 4, 118);
      (24, M, 28, 6, 45, 14, 46);
      (24, Q, 30, 11, 24, 16, 25);
      (24, H, 30, 30, 16, 2, 17);
      (* Version 25 *)
      (25, L, 26, 8, 106, 4, 107);
      (25, M, 28, 8, 47, 13, 48);
      (25, Q, 30, 7, 24, 22, 25);
      (25, H, 30, 22, 15, 13, 16);
      (* Version 26 *)
      (26, L, 28, 10, 114, 2, 115);
      (26, M, 28, 19, 46, 4, 47);
      (26, Q, 28, 28, 22, 6, 23);
      (26, H, 30, 33, 16, 4, 17);
      (* Version 27 *)
      (27, L, 30, 8, 122, 4, 123);
      (27, M, 28, 22, 45, 3, 46);
      (27, Q, 30, 8, 23, 26, 24);
      (27, H, 30, 12, 15, 28, 16);
      (* Version 28 *)
      (28, L, 30, 3, 117, 10, 118);
      (28, M, 28, 3, 45, 23, 46);
      (28, Q, 30, 4, 24, 31, 25);
      (28, H, 30, 11, 15, 31, 16);
      (* Version 29 *)
      (29, L, 30, 7, 116, 7, 117);
      (29, M, 28, 21, 45, 7, 46);
      (29, Q, 30, 1, 23, 37, 24);
      (29, H, 30, 19, 15, 26, 16);
      (* Version 30 *)
      (30, L, 30, 5, 115, 10, 116);
      (30, M, 28, 19, 47, 10, 48);
      (30, Q, 30, 15, 24, 25, 25);
      (30, H, 30, 23, 15, 25, 16);
      (* Version 31 *)
      (31, L, 30, 13, 115, 3, 116);
      (31, M, 28, 2, 46, 29, 47);
      (31, Q, 30, 42, 24, 1, 25);
      (31, H, 30, 23, 15, 28, 16);
      (* Version 32 *)
      (32, L, 30, 17, 115, 0, 0);
      (32, M, 28, 10, 46, 23, 47);
      (32, Q, 30, 10, 24, 35, 25);
      (32, H, 30, 19, 15, 35, 16);
      (* Version 33 *)
      (33, L, 30, 17, 115, 1, 116);
      (33, M, 28, 14, 46, 21, 47);
      (33, Q, 30, 29, 24, 19, 25);
      (33, H, 30, 11, 15, 46, 16);
      (* Version 34 *)
      (34, L, 30, 13, 115, 6, 116);
      (34, M, 28, 14, 46, 23, 47);
      (34, Q, 30, 44, 24, 7, 25);
      (34, H, 30, 59, 16, 1, 17);
      (* Version 35 *)
      (35, L, 30, 12, 121, 7, 122);
      (35, M, 28, 12, 47, 26, 48);
      (35, Q, 30, 39, 24, 14, 25);
      (35, H, 30, 22, 15, 41, 16);
      (* Version 36 *)
      (36, L, 30, 6, 121, 14, 122);
      (36, M, 28, 6, 47, 34, 48);
      (36, Q, 30, 46, 24, 10, 25);
      (36, H, 30, 2, 15, 64, 16);
      (* Version 37 *)
      (37, L, 30, 17, 122, 4, 123);
      (37, M, 28, 29, 46, 14, 47);
      (37, Q, 30, 49, 24, 10, 25);
      (37, H, 30, 24, 15, 46, 16);
      (* Version 38 *)
      (38, L, 30, 4, 122, 18, 123);
      (38, M, 28, 13, 46, 32, 47);
      (38, Q, 30, 48, 24, 14, 25);
      (38, H, 30, 42, 15, 32, 16);
      (* Version 39 *)
      (39, L, 30, 20, 117, 4, 118);
      (39, M, 28, 40, 47, 7, 48);
      (39, Q, 30, 43, 24, 22, 25);
      (39, H, 30, 10, 15, 67, 16);
      (* Version 40 *)
      (40, L, 30, 19, 118, 6, 119);
      (40, M, 28, 18, 47, 31, 48);
      (40, Q, 30, 34, 24, 34, 25);
      (40, H, 30, 20, 15, 61, 16);
    ]
  in
  List.iter entries ~f:add_entry;
  hash_table

let get_capacity config = Hashtbl.find_exn capacity_table config
let get_ec_info config = Hashtbl.find_exn ec_table config
let mode_indicator_length = 4
let mode_indicator = 0b0010

let rec find_version (v @ local) (ecl @ local) (length @ local) =
  if v > 40 then raise (Invalid_argument "Data too long to encode in QR code")
  else
    let config = make ~version:v ~ecl in
    let capacity = get_capacity config in
    if length <= capacity then config else find_version (v + 1) ecl length

let get_config s (ecl @ local) =
  let length = String.length s in
  find_version 1 ecl length
