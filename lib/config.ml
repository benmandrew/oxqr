open Base

module ECL = struct
  type t = L | M | Q | H [@@deriving sexp_of, compare, equal, hash]

  let equal__local (a @ local) (b @ local) =
    match (a, b) with
    | L, L -> true
    | M, M -> true
    | Q, Q -> true
    | H, H -> true
    | _, _ -> false
end

(* Unboxed config type as specified in the interface *)
type t = #{ version : int; ecl : ECL.t }

(* Manual equality and compare for unboxed records *)
let equal a b = a.#version = b.#version && ECL.equal a.#ecl b.#ecl

let compare a b =
  let cmp_version = Int.compare a.#version b.#version in
  if cmp_version <> 0 then cmp_version else ECL.compare a.#ecl b.#ecl

(* Functions for creating configs *)
let make ~version ~ecl =
  assert (version >= 1 && version <= 40);
  #{ version; ecl }

let[@zero_alloc] char_count_indicator_length (t @ local) =
  match t.#version with
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

(* Parallel arrays: unboxed configs and corresponding values for capacity lookup *)
let capacity_configs =
  let open ECL in
  [|
    #{ version = 1; ecl = L };
    #{ version = 1; ecl = M };
    #{ version = 1; ecl = Q };
    #{ version = 1; ecl = H };
    #{ version = 2; ecl = L };
    #{ version = 2; ecl = M };
    #{ version = 2; ecl = Q };
    #{ version = 2; ecl = H };
    #{ version = 3; ecl = L };
    #{ version = 3; ecl = M };
    #{ version = 3; ecl = Q };
    #{ version = 3; ecl = H };
    #{ version = 4; ecl = L };
    #{ version = 4; ecl = M };
    #{ version = 4; ecl = Q };
    #{ version = 4; ecl = H };
    #{ version = 5; ecl = L };
    #{ version = 5; ecl = M };
    #{ version = 5; ecl = Q };
    #{ version = 5; ecl = H };
    #{ version = 6; ecl = L };
    #{ version = 6; ecl = M };
    #{ version = 6; ecl = Q };
    #{ version = 6; ecl = H };
    #{ version = 7; ecl = L };
    #{ version = 7; ecl = M };
    #{ version = 7; ecl = Q };
    #{ version = 7; ecl = H };
    #{ version = 8; ecl = L };
    #{ version = 8; ecl = M };
    #{ version = 8; ecl = Q };
    #{ version = 8; ecl = H };
    #{ version = 9; ecl = L };
    #{ version = 9; ecl = M };
    #{ version = 9; ecl = Q };
    #{ version = 9; ecl = H };
    #{ version = 10; ecl = L };
    #{ version = 10; ecl = M };
    #{ version = 10; ecl = Q };
    #{ version = 10; ecl = H };
    #{ version = 11; ecl = L };
    #{ version = 11; ecl = M };
    #{ version = 11; ecl = Q };
    #{ version = 11; ecl = H };
    #{ version = 12; ecl = L };
    #{ version = 12; ecl = M };
    #{ version = 12; ecl = Q };
    #{ version = 12; ecl = H };
    #{ version = 13; ecl = L };
    #{ version = 13; ecl = M };
    #{ version = 13; ecl = Q };
    #{ version = 13; ecl = H };
    #{ version = 14; ecl = L };
    #{ version = 14; ecl = M };
    #{ version = 14; ecl = Q };
    #{ version = 14; ecl = H };
    #{ version = 15; ecl = L };
    #{ version = 15; ecl = M };
    #{ version = 15; ecl = Q };
    #{ version = 15; ecl = H };
    #{ version = 16; ecl = L };
    #{ version = 16; ecl = M };
    #{ version = 16; ecl = Q };
    #{ version = 16; ecl = H };
    #{ version = 17; ecl = L };
    #{ version = 17; ecl = M };
    #{ version = 17; ecl = Q };
    #{ version = 17; ecl = H };
    #{ version = 18; ecl = L };
    #{ version = 18; ecl = M };
    #{ version = 18; ecl = Q };
    #{ version = 18; ecl = H };
    #{ version = 19; ecl = L };
    #{ version = 19; ecl = M };
    #{ version = 19; ecl = Q };
    #{ version = 19; ecl = H };
    #{ version = 20; ecl = L };
    #{ version = 20; ecl = M };
    #{ version = 20; ecl = Q };
    #{ version = 20; ecl = H };
    #{ version = 21; ecl = L };
    #{ version = 21; ecl = M };
    #{ version = 21; ecl = Q };
    #{ version = 21; ecl = H };
    #{ version = 22; ecl = L };
    #{ version = 22; ecl = M };
    #{ version = 22; ecl = Q };
    #{ version = 22; ecl = H };
    #{ version = 23; ecl = L };
    #{ version = 23; ecl = M };
    #{ version = 23; ecl = Q };
    #{ version = 23; ecl = H };
    #{ version = 24; ecl = L };
    #{ version = 24; ecl = M };
    #{ version = 24; ecl = Q };
    #{ version = 24; ecl = H };
    #{ version = 25; ecl = L };
    #{ version = 25; ecl = M };
    #{ version = 25; ecl = Q };
    #{ version = 25; ecl = H };
    #{ version = 26; ecl = L };
    #{ version = 26; ecl = M };
    #{ version = 26; ecl = Q };
    #{ version = 26; ecl = H };
    #{ version = 27; ecl = L };
    #{ version = 27; ecl = M };
    #{ version = 27; ecl = Q };
    #{ version = 27; ecl = H };
    #{ version = 28; ecl = L };
    #{ version = 28; ecl = M };
    #{ version = 28; ecl = Q };
    #{ version = 28; ecl = H };
    #{ version = 29; ecl = L };
    #{ version = 29; ecl = M };
    #{ version = 29; ecl = Q };
    #{ version = 29; ecl = H };
    #{ version = 30; ecl = L };
    #{ version = 30; ecl = M };
    #{ version = 30; ecl = Q };
    #{ version = 30; ecl = H };
    #{ version = 31; ecl = L };
    #{ version = 31; ecl = M };
    #{ version = 31; ecl = Q };
    #{ version = 31; ecl = H };
    #{ version = 32; ecl = L };
    #{ version = 32; ecl = M };
    #{ version = 32; ecl = Q };
    #{ version = 32; ecl = H };
    #{ version = 33; ecl = L };
    #{ version = 33; ecl = M };
    #{ version = 33; ecl = Q };
    #{ version = 33; ecl = H };
    #{ version = 34; ecl = L };
    #{ version = 34; ecl = M };
    #{ version = 34; ecl = Q };
    #{ version = 34; ecl = H };
    #{ version = 35; ecl = L };
    #{ version = 35; ecl = M };
    #{ version = 35; ecl = Q };
    #{ version = 35; ecl = H };
    #{ version = 36; ecl = L };
    #{ version = 36; ecl = M };
    #{ version = 36; ecl = Q };
    #{ version = 36; ecl = H };
    #{ version = 37; ecl = L };
    #{ version = 37; ecl = M };
    #{ version = 37; ecl = Q };
    #{ version = 37; ecl = H };
    #{ version = 38; ecl = L };
    #{ version = 38; ecl = M };
    #{ version = 38; ecl = Q };
    #{ version = 38; ecl = H };
    #{ version = 39; ecl = L };
    #{ version = 39; ecl = M };
    #{ version = 39; ecl = Q };
    #{ version = 39; ecl = H };
    #{ version = 40; ecl = L };
    #{ version = 40; ecl = M };
    #{ version = 40; ecl = Q };
    #{ version = 40; ecl = H };
  |]

let capacity_values =
  [|
    25;
    20;
    16;
    10;
    47;
    38;
    29;
    20;
    77;
    61;
    47;
    35;
    114;
    90;
    67;
    50;
    154;
    122;
    87;
    64;
    195;
    154;
    108;
    84;
    224;
    178;
    125;
    93;
    279;
    221;
    157;
    122;
    335;
    262;
    189;
    143;
    395;
    311;
    221;
    174;
    468;
    366;
    259;
    200;
    535;
    419;
    296;
    227;
    619;
    483;
    352;
    259;
    667;
    528;
    376;
    283;
    758;
    600;
    426;
    321;
    854;
    656;
    470;
    365;
    938;
    734;
    531;
    408;
    1046;
    816;
    574;
    452;
    1153;
    909;
    644;
    493;
    1249;
    970;
    702;
    557;
    1352;
    1035;
    742;
    587;
    1460;
    1134;
    823;
    640;
    1588;
    1248;
    890;
    672;
    1704;
    1326;
    963;
    744;
    1853;
    1451;
    1041;
    779;
    1990;
    1542;
    1094;
    864;
    2132;
    1637;
    1172;
    910;
    2223;
    1732;
    1263;
    958;
    2369;
    1839;
    1322;
    1016;
    2520;
    1994;
    1429;
    1080;
    2677;
    2113;
    1499;
    1150;
    2840;
    2238;
    1618;
    1226;
    3009;
    2369;
    1700;
    1307;
    3183;
    2506;
    1787;
    1394;
    3351;
    2632;
    1867;
    1431;
    3537;
    2780;
    1966;
    1530;
    3729;
    2894;
    2071;
    1591;
    3927;
    3054;
    2181;
    1658;
    4087;
    3220;
    2298;
    1774;
    4296;
    3391;
    2420;
    1852;
  |]

let ec_configs =
  let open ECL in
  [|
    #{ version = 1; ecl = L };
    #{ version = 1; ecl = M };
    #{ version = 1; ecl = Q };
    #{ version = 1; ecl = H };
    #{ version = 2; ecl = L };
    #{ version = 2; ecl = M };
    #{ version = 2; ecl = Q };
    #{ version = 2; ecl = H };
    #{ version = 3; ecl = L };
    #{ version = 3; ecl = M };
    #{ version = 3; ecl = Q };
    #{ version = 3; ecl = H };
    #{ version = 4; ecl = L };
    #{ version = 4; ecl = M };
    #{ version = 4; ecl = Q };
    #{ version = 4; ecl = H };
    #{ version = 5; ecl = L };
    #{ version = 5; ecl = M };
    #{ version = 5; ecl = Q };
    #{ version = 5; ecl = H };
    #{ version = 6; ecl = L };
    #{ version = 6; ecl = M };
    #{ version = 6; ecl = Q };
    #{ version = 6; ecl = H };
    #{ version = 7; ecl = L };
    #{ version = 7; ecl = M };
    #{ version = 7; ecl = Q };
    #{ version = 7; ecl = H };
    #{ version = 8; ecl = L };
    #{ version = 8; ecl = M };
    #{ version = 8; ecl = Q };
    #{ version = 8; ecl = H };
    #{ version = 9; ecl = L };
    #{ version = 9; ecl = M };
    #{ version = 9; ecl = Q };
    #{ version = 9; ecl = H };
    #{ version = 10; ecl = L };
    #{ version = 10; ecl = M };
    #{ version = 10; ecl = Q };
    #{ version = 10; ecl = H };
    #{ version = 11; ecl = L };
    #{ version = 11; ecl = M };
    #{ version = 11; ecl = Q };
    #{ version = 11; ecl = H };
    #{ version = 12; ecl = L };
    #{ version = 12; ecl = M };
    #{ version = 12; ecl = Q };
    #{ version = 12; ecl = H };
    #{ version = 13; ecl = L };
    #{ version = 13; ecl = M };
    #{ version = 13; ecl = Q };
    #{ version = 13; ecl = H };
    #{ version = 14; ecl = L };
    #{ version = 14; ecl = M };
    #{ version = 14; ecl = Q };
    #{ version = 14; ecl = H };
    #{ version = 15; ecl = L };
    #{ version = 15; ecl = M };
    #{ version = 15; ecl = Q };
    #{ version = 15; ecl = H };
    #{ version = 16; ecl = L };
    #{ version = 16; ecl = M };
    #{ version = 16; ecl = Q };
    #{ version = 16; ecl = H };
    #{ version = 17; ecl = L };
    #{ version = 17; ecl = M };
    #{ version = 17; ecl = Q };
    #{ version = 17; ecl = H };
    #{ version = 18; ecl = L };
    #{ version = 18; ecl = M };
    #{ version = 18; ecl = Q };
    #{ version = 18; ecl = H };
    #{ version = 19; ecl = L };
    #{ version = 19; ecl = M };
    #{ version = 19; ecl = Q };
    #{ version = 19; ecl = H };
    #{ version = 20; ecl = L };
    #{ version = 20; ecl = M };
    #{ version = 20; ecl = Q };
    #{ version = 20; ecl = H };
    #{ version = 21; ecl = L };
    #{ version = 21; ecl = M };
    #{ version = 21; ecl = Q };
    #{ version = 21; ecl = H };
    #{ version = 22; ecl = L };
    #{ version = 22; ecl = M };
    #{ version = 22; ecl = Q };
    #{ version = 22; ecl = H };
    #{ version = 23; ecl = L };
    #{ version = 23; ecl = M };
    #{ version = 23; ecl = Q };
    #{ version = 23; ecl = H };
    #{ version = 24; ecl = L };
    #{ version = 24; ecl = M };
    #{ version = 24; ecl = Q };
    #{ version = 24; ecl = H };
    #{ version = 25; ecl = L };
    #{ version = 25; ecl = M };
    #{ version = 25; ecl = Q };
    #{ version = 25; ecl = H };
    #{ version = 26; ecl = L };
    #{ version = 26; ecl = M };
    #{ version = 26; ecl = Q };
    #{ version = 26; ecl = H };
    #{ version = 27; ecl = L };
    #{ version = 27; ecl = M };
    #{ version = 27; ecl = Q };
    #{ version = 27; ecl = H };
    #{ version = 28; ecl = L };
    #{ version = 28; ecl = M };
    #{ version = 28; ecl = Q };
    #{ version = 28; ecl = H };
    #{ version = 29; ecl = L };
    #{ version = 29; ecl = M };
    #{ version = 29; ecl = Q };
    #{ version = 29; ecl = H };
    #{ version = 30; ecl = L };
    #{ version = 30; ecl = M };
    #{ version = 30; ecl = Q };
    #{ version = 30; ecl = H };
    #{ version = 31; ecl = L };
    #{ version = 31; ecl = M };
    #{ version = 31; ecl = Q };
    #{ version = 31; ecl = H };
    #{ version = 32; ecl = L };
    #{ version = 32; ecl = M };
    #{ version = 32; ecl = Q };
    #{ version = 32; ecl = H };
    #{ version = 33; ecl = L };
    #{ version = 33; ecl = M };
    #{ version = 33; ecl = Q };
    #{ version = 33; ecl = H };
    #{ version = 34; ecl = L };
    #{ version = 34; ecl = M };
    #{ version = 34; ecl = Q };
    #{ version = 34; ecl = H };
    #{ version = 35; ecl = L };
    #{ version = 35; ecl = M };
    #{ version = 35; ecl = Q };
    #{ version = 35; ecl = H };
    #{ version = 36; ecl = L };
    #{ version = 36; ecl = M };
    #{ version = 36; ecl = Q };
    #{ version = 36; ecl = H };
    #{ version = 37; ecl = L };
    #{ version = 37; ecl = M };
    #{ version = 37; ecl = Q };
    #{ version = 37; ecl = H };
    #{ version = 38; ecl = L };
    #{ version = 38; ecl = M };
    #{ version = 38; ecl = Q };
    #{ version = 38; ecl = H };
    #{ version = 39; ecl = L };
    #{ version = 39; ecl = M };
    #{ version = 39; ecl = Q };
    #{ version = 39; ecl = H };
    #{ version = 40; ecl = L };
    #{ version = 40; ecl = M };
    #{ version = 40; ecl = Q };
    #{ version = 40; ecl = H };
  |]

let ec_values =
  [|
    {
      ec_codewords_per_block = 7;
      group1_blocks = 1;
      group1_data_codewords = 19;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 10;
      group1_blocks = 1;
      group1_data_codewords = 16;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 13;
      group1_blocks = 1;
      group1_data_codewords = 13;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 17;
      group1_blocks = 1;
      group1_data_codewords = 9;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 10;
      group1_blocks = 1;
      group1_data_codewords = 34;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 16;
      group1_blocks = 1;
      group1_data_codewords = 28;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 22;
      group1_blocks = 1;
      group1_data_codewords = 22;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 1;
      group1_data_codewords = 16;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 15;
      group1_blocks = 1;
      group1_data_codewords = 55;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 1;
      group1_data_codewords = 44;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 18;
      group1_blocks = 2;
      group1_data_codewords = 17;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 22;
      group1_blocks = 2;
      group1_data_codewords = 13;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 20;
      group1_blocks = 1;
      group1_data_codewords = 80;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 18;
      group1_blocks = 2;
      group1_data_codewords = 32;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 2;
      group1_data_codewords = 24;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 16;
      group1_blocks = 4;
      group1_data_codewords = 9;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 1;
      group1_data_codewords = 108;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 2;
      group1_data_codewords = 43;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 18;
      group1_blocks = 2;
      group1_data_codewords = 15;
      group2_blocks = 2;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 22;
      group1_blocks = 2;
      group1_data_codewords = 11;
      group2_blocks = 2;
      group2_data_codewords = 12;
    };
    {
      ec_codewords_per_block = 18;
      group1_blocks = 2;
      group1_data_codewords = 68;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 16;
      group1_blocks = 4;
      group1_data_codewords = 27;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 4;
      group1_data_codewords = 19;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 4;
      group1_data_codewords = 15;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 20;
      group1_blocks = 2;
      group1_data_codewords = 78;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 18;
      group1_blocks = 4;
      group1_data_codewords = 31;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 18;
      group1_blocks = 2;
      group1_data_codewords = 14;
      group2_blocks = 4;
      group2_data_codewords = 15;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 4;
      group1_data_codewords = 13;
      group2_blocks = 1;
      group2_data_codewords = 14;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 2;
      group1_data_codewords = 97;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 22;
      group1_blocks = 2;
      group1_data_codewords = 38;
      group2_blocks = 2;
      group2_data_codewords = 39;
    };
    {
      ec_codewords_per_block = 22;
      group1_blocks = 4;
      group1_data_codewords = 18;
      group2_blocks = 2;
      group2_data_codewords = 19;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 4;
      group1_data_codewords = 14;
      group2_blocks = 2;
      group2_data_codewords = 15;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 2;
      group1_data_codewords = 116;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 22;
      group1_blocks = 3;
      group1_data_codewords = 36;
      group2_blocks = 2;
      group2_data_codewords = 37;
    };
    {
      ec_codewords_per_block = 20;
      group1_blocks = 4;
      group1_data_codewords = 16;
      group2_blocks = 4;
      group2_data_codewords = 17;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 4;
      group1_data_codewords = 12;
      group2_blocks = 4;
      group2_data_codewords = 13;
    };
    {
      ec_codewords_per_block = 18;
      group1_blocks = 2;
      group1_data_codewords = 68;
      group2_blocks = 2;
      group2_data_codewords = 69;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 4;
      group1_data_codewords = 43;
      group2_blocks = 1;
      group2_data_codewords = 44;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 6;
      group1_data_codewords = 19;
      group2_blocks = 2;
      group2_data_codewords = 20;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 6;
      group1_data_codewords = 15;
      group2_blocks = 2;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 20;
      group1_blocks = 4;
      group1_data_codewords = 81;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 1;
      group1_data_codewords = 50;
      group2_blocks = 4;
      group2_data_codewords = 51;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 4;
      group1_data_codewords = 22;
      group2_blocks = 4;
      group2_data_codewords = 23;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 3;
      group1_data_codewords = 12;
      group2_blocks = 8;
      group2_data_codewords = 13;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 2;
      group1_data_codewords = 92;
      group2_blocks = 2;
      group2_data_codewords = 93;
    };
    {
      ec_codewords_per_block = 22;
      group1_blocks = 6;
      group1_data_codewords = 36;
      group2_blocks = 2;
      group2_data_codewords = 37;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 4;
      group1_data_codewords = 20;
      group2_blocks = 6;
      group2_data_codewords = 21;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 7;
      group1_data_codewords = 14;
      group2_blocks = 4;
      group2_data_codewords = 15;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 4;
      group1_data_codewords = 107;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 22;
      group1_blocks = 8;
      group1_data_codewords = 37;
      group2_blocks = 1;
      group2_data_codewords = 38;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 8;
      group1_data_codewords = 20;
      group2_blocks = 4;
      group2_data_codewords = 21;
    };
    {
      ec_codewords_per_block = 22;
      group1_blocks = 12;
      group1_data_codewords = 11;
      group2_blocks = 4;
      group2_data_codewords = 12;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 3;
      group1_data_codewords = 115;
      group2_blocks = 1;
      group2_data_codewords = 116;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 4;
      group1_data_codewords = 40;
      group2_blocks = 5;
      group2_data_codewords = 41;
    };
    {
      ec_codewords_per_block = 20;
      group1_blocks = 11;
      group1_data_codewords = 16;
      group2_blocks = 5;
      group2_data_codewords = 17;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 11;
      group1_data_codewords = 12;
      group2_blocks = 5;
      group2_data_codewords = 13;
    };
    {
      ec_codewords_per_block = 22;
      group1_blocks = 5;
      group1_data_codewords = 87;
      group2_blocks = 1;
      group2_data_codewords = 88;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 5;
      group1_data_codewords = 41;
      group2_blocks = 5;
      group2_data_codewords = 42;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 5;
      group1_data_codewords = 24;
      group2_blocks = 7;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 11;
      group1_data_codewords = 12;
      group2_blocks = 7;
      group2_data_codewords = 13;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 5;
      group1_data_codewords = 98;
      group2_blocks = 1;
      group2_data_codewords = 99;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 7;
      group1_data_codewords = 45;
      group2_blocks = 3;
      group2_data_codewords = 46;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 15;
      group1_data_codewords = 19;
      group2_blocks = 2;
      group2_data_codewords = 20;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 3;
      group1_data_codewords = 15;
      group2_blocks = 13;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 1;
      group1_data_codewords = 107;
      group2_blocks = 5;
      group2_data_codewords = 108;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 10;
      group1_data_codewords = 46;
      group2_blocks = 1;
      group2_data_codewords = 47;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 1;
      group1_data_codewords = 22;
      group2_blocks = 15;
      group2_data_codewords = 23;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 2;
      group1_data_codewords = 14;
      group2_blocks = 17;
      group2_data_codewords = 15;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 5;
      group1_data_codewords = 120;
      group2_blocks = 1;
      group2_data_codewords = 121;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 9;
      group1_data_codewords = 43;
      group2_blocks = 4;
      group2_data_codewords = 44;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 17;
      group1_data_codewords = 22;
      group2_blocks = 1;
      group2_data_codewords = 23;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 2;
      group1_data_codewords = 14;
      group2_blocks = 19;
      group2_data_codewords = 15;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 3;
      group1_data_codewords = 113;
      group2_blocks = 4;
      group2_data_codewords = 114;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 3;
      group1_data_codewords = 44;
      group2_blocks = 11;
      group2_data_codewords = 45;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 17;
      group1_data_codewords = 21;
      group2_blocks = 4;
      group2_data_codewords = 22;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 9;
      group1_data_codewords = 13;
      group2_blocks = 16;
      group2_data_codewords = 14;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 3;
      group1_data_codewords = 107;
      group2_blocks = 5;
      group2_data_codewords = 108;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 3;
      group1_data_codewords = 41;
      group2_blocks = 13;
      group2_data_codewords = 42;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 15;
      group1_data_codewords = 24;
      group2_blocks = 5;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 15;
      group1_data_codewords = 15;
      group2_blocks = 10;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 4;
      group1_data_codewords = 116;
      group2_blocks = 4;
      group2_data_codewords = 117;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 17;
      group1_data_codewords = 42;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 17;
      group1_data_codewords = 22;
      group2_blocks = 6;
      group2_data_codewords = 23;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 19;
      group1_data_codewords = 16;
      group2_blocks = 6;
      group2_data_codewords = 17;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 2;
      group1_data_codewords = 111;
      group2_blocks = 7;
      group2_data_codewords = 112;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 17;
      group1_data_codewords = 46;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 7;
      group1_data_codewords = 24;
      group2_blocks = 16;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 24;
      group1_blocks = 34;
      group1_data_codewords = 13;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 4;
      group1_data_codewords = 121;
      group2_blocks = 5;
      group2_data_codewords = 122;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 4;
      group1_data_codewords = 47;
      group2_blocks = 14;
      group2_data_codewords = 48;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 11;
      group1_data_codewords = 24;
      group2_blocks = 14;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 16;
      group1_data_codewords = 15;
      group2_blocks = 14;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 6;
      group1_data_codewords = 117;
      group2_blocks = 4;
      group2_data_codewords = 118;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 6;
      group1_data_codewords = 45;
      group2_blocks = 14;
      group2_data_codewords = 46;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 11;
      group1_data_codewords = 24;
      group2_blocks = 16;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 30;
      group1_data_codewords = 16;
      group2_blocks = 2;
      group2_data_codewords = 17;
    };
    {
      ec_codewords_per_block = 26;
      group1_blocks = 8;
      group1_data_codewords = 106;
      group2_blocks = 4;
      group2_data_codewords = 107;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 8;
      group1_data_codewords = 47;
      group2_blocks = 13;
      group2_data_codewords = 48;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 7;
      group1_data_codewords = 24;
      group2_blocks = 22;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 22;
      group1_data_codewords = 15;
      group2_blocks = 13;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 10;
      group1_data_codewords = 114;
      group2_blocks = 2;
      group2_data_codewords = 115;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 19;
      group1_data_codewords = 46;
      group2_blocks = 4;
      group2_data_codewords = 47;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 28;
      group1_data_codewords = 22;
      group2_blocks = 6;
      group2_data_codewords = 23;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 33;
      group1_data_codewords = 16;
      group2_blocks = 4;
      group2_data_codewords = 17;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 8;
      group1_data_codewords = 122;
      group2_blocks = 4;
      group2_data_codewords = 123;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 22;
      group1_data_codewords = 45;
      group2_blocks = 3;
      group2_data_codewords = 46;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 8;
      group1_data_codewords = 23;
      group2_blocks = 26;
      group2_data_codewords = 24;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 12;
      group1_data_codewords = 15;
      group2_blocks = 28;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 3;
      group1_data_codewords = 117;
      group2_blocks = 10;
      group2_data_codewords = 118;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 3;
      group1_data_codewords = 45;
      group2_blocks = 23;
      group2_data_codewords = 46;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 4;
      group1_data_codewords = 24;
      group2_blocks = 31;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 11;
      group1_data_codewords = 15;
      group2_blocks = 31;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 7;
      group1_data_codewords = 116;
      group2_blocks = 7;
      group2_data_codewords = 117;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 21;
      group1_data_codewords = 45;
      group2_blocks = 7;
      group2_data_codewords = 46;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 1;
      group1_data_codewords = 23;
      group2_blocks = 37;
      group2_data_codewords = 24;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 19;
      group1_data_codewords = 15;
      group2_blocks = 26;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 5;
      group1_data_codewords = 115;
      group2_blocks = 10;
      group2_data_codewords = 116;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 19;
      group1_data_codewords = 47;
      group2_blocks = 10;
      group2_data_codewords = 48;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 15;
      group1_data_codewords = 24;
      group2_blocks = 25;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 23;
      group1_data_codewords = 15;
      group2_blocks = 25;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 13;
      group1_data_codewords = 115;
      group2_blocks = 3;
      group2_data_codewords = 116;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 2;
      group1_data_codewords = 46;
      group2_blocks = 29;
      group2_data_codewords = 47;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 42;
      group1_data_codewords = 24;
      group2_blocks = 1;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 23;
      group1_data_codewords = 15;
      group2_blocks = 28;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 17;
      group1_data_codewords = 115;
      group2_blocks = 0;
      group2_data_codewords = 0;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 10;
      group1_data_codewords = 46;
      group2_blocks = 23;
      group2_data_codewords = 47;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 10;
      group1_data_codewords = 24;
      group2_blocks = 35;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 19;
      group1_data_codewords = 15;
      group2_blocks = 35;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 17;
      group1_data_codewords = 115;
      group2_blocks = 1;
      group2_data_codewords = 116;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 14;
      group1_data_codewords = 46;
      group2_blocks = 21;
      group2_data_codewords = 47;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 29;
      group1_data_codewords = 24;
      group2_blocks = 19;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 11;
      group1_data_codewords = 15;
      group2_blocks = 46;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 13;
      group1_data_codewords = 115;
      group2_blocks = 6;
      group2_data_codewords = 116;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 14;
      group1_data_codewords = 46;
      group2_blocks = 23;
      group2_data_codewords = 47;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 44;
      group1_data_codewords = 24;
      group2_blocks = 7;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 59;
      group1_data_codewords = 16;
      group2_blocks = 1;
      group2_data_codewords = 17;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 12;
      group1_data_codewords = 121;
      group2_blocks = 7;
      group2_data_codewords = 122;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 12;
      group1_data_codewords = 47;
      group2_blocks = 26;
      group2_data_codewords = 48;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 39;
      group1_data_codewords = 24;
      group2_blocks = 14;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 22;
      group1_data_codewords = 15;
      group2_blocks = 41;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 6;
      group1_data_codewords = 121;
      group2_blocks = 14;
      group2_data_codewords = 122;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 6;
      group1_data_codewords = 47;
      group2_blocks = 34;
      group2_data_codewords = 48;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 46;
      group1_data_codewords = 24;
      group2_blocks = 10;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 2;
      group1_data_codewords = 15;
      group2_blocks = 64;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 17;
      group1_data_codewords = 122;
      group2_blocks = 4;
      group2_data_codewords = 123;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 29;
      group1_data_codewords = 46;
      group2_blocks = 14;
      group2_data_codewords = 47;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 49;
      group1_data_codewords = 24;
      group2_blocks = 10;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 24;
      group1_data_codewords = 15;
      group2_blocks = 46;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 4;
      group1_data_codewords = 122;
      group2_blocks = 18;
      group2_data_codewords = 123;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 13;
      group1_data_codewords = 46;
      group2_blocks = 32;
      group2_data_codewords = 47;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 48;
      group1_data_codewords = 24;
      group2_blocks = 14;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 42;
      group1_data_codewords = 15;
      group2_blocks = 32;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 20;
      group1_data_codewords = 117;
      group2_blocks = 4;
      group2_data_codewords = 118;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 40;
      group1_data_codewords = 47;
      group2_blocks = 7;
      group2_data_codewords = 48;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 43;
      group1_data_codewords = 24;
      group2_blocks = 22;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 10;
      group1_data_codewords = 15;
      group2_blocks = 67;
      group2_data_codewords = 16;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 19;
      group1_data_codewords = 118;
      group2_blocks = 6;
      group2_data_codewords = 119;
    };
    {
      ec_codewords_per_block = 28;
      group1_blocks = 18;
      group1_data_codewords = 47;
      group2_blocks = 31;
      group2_data_codewords = 48;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 34;
      group1_data_codewords = 24;
      group2_blocks = 34;
      group2_data_codewords = 25;
    };
    {
      ec_codewords_per_block = 30;
      group1_blocks = 20;
      group1_data_codewords = 15;
      group2_blocks = 61;
      group2_data_codewords = 16;
    };
  |]

(* Custom exception for lookup failures *)
exception Config_not_found

(* Find config index using tail recursion - parameter passing avoids closure capture *)
let rec find_config_index config configs len i =
  if i >= len then raise Config_not_found
  else if equal config configs.(i) then i
  else find_config_index config configs len (i + 1)

(* Zero-alloc array search for capacity *)
let[@zero_alloc] get_capacity (config @ local) : int =
  let idx =
    find_config_index config capacity_configs (Array.length capacity_configs) 0
  in
  capacity_values.(idx)

(* Zero-alloc array search for ec_info *)
let[@zero_alloc] get_ec_info (config @ local) : ec_info =
  let idx = find_config_index config ec_configs (Array.length ec_configs) 0 in
  ec_values.(idx)

let mode_indicator_length = 4
let mode_indicator = 0b0010

let rec find_version v (ecl @ local) length : t =
  let config = make ~version:v ~ecl in
  let capacity = get_capacity config in
  if v >= 40 || length <= capacity then config
  else find_version (v + 1) ecl length

let get_config s (ecl @ local) =
  let length = String.length s in
  let result = find_version 1 ecl length in
  if result.#version > 40 then
    raise (Invalid_argument "Data too long to encode in QR code");
  result
