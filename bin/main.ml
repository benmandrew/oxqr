open Oxqr
open Base

type output_format = Svg | Ascii

let run data ecl format =
  let arena = Encoding.Arena.create ~qr_version:1 in
  let qr = Encoding.generate_qr arena data ecl in
  let output_string =
    match format with
    | Svg -> Svg.qr_to_svg qr
    | Ascii -> Qr.to_unicode_string qr
  in
  Out_channel.output_string Out_channel.stdout output_string

let string_of_ecl = function
  | Config.ECL.L -> "L"
  | M -> "M"
  | Q -> "Q"
  | H -> "H"

let parse_ecl s =
  match String.uppercase s with
  | "L" -> Config.ECL.L
  | "M" -> Config.ECL.M
  | "Q" -> Config.ECL.Q
  | "H" -> Config.ECL.H
  | _ ->
      Out_channel.output_string Out_channel.stderr
        (Printf.sprintf "Error: ECL must be one of L, M, Q, H (got '%s')\n" s);
      Stdlib.exit 1

let parse_format s =
  match String.lowercase s with
  | "svg" -> Svg
  | "ascii" -> Ascii
  | _ ->
      Out_channel.output_string Out_channel.stderr
        (Printf.sprintf "Error: FORMAT must be one of ascii, svg (got '%s')\n" s);
      Stdlib.exit 1

let parse_data s =
  let upper = String.uppercase s in
  String.findi upper ~f:(fun _ -> function
    | '0' .. '9'
    | 'A' .. 'Z'
    | ' ' | '$' | '%' | '*' | '+' | '-' | '.' | '/' | ':' ->
        false
    | _ -> true)
  |> function
  | None -> upper
  | Some (idx, invalid_char) ->
      Out_channel.output_string Out_channel.stderr
        (Printf.sprintf
           "Error: Invalid character '%c' at position %d in input data\n"
           invalid_char (idx + 1));
      Stdlib.exit 1

let print_help () =
  Out_channel.output_string Out_channel.stdout
    {|Usage: oxqr [OPTIONS] DATA
Generate a QR code from alphanumeric input.
OPTIONS:
  --ecl ECL       Error correction level (L|M|Q|H). Default: M.
  --format FORMAT Output format (ascii|svg). Default: ascii.
  -h, --help      Show this help message.|}

let rec parse_args argv idx data ecl format =
  if idx >= Array.length argv then (data, ecl, format)
  else
    match argv.(idx) with
    | "-h" | "--help" ->
        print_help ();
        Stdlib.exit 0
    | "--ecl" ->
        if idx + 1 >= Array.length argv then (
          Out_channel.output_string Out_channel.stderr
            "Error: --ecl requires an argument\n";
          Stdlib.exit 1)
        else
          let new_ecl = parse_ecl argv.(idx + 1) in
          parse_args argv (idx + 2) data new_ecl format
    | "--format" ->
        if idx + 1 >= Array.length argv then (
          Out_channel.output_string Out_channel.stderr
            "Error: --format requires an argument\n";
          Stdlib.exit 1)
        else
          let new_format = parse_format argv.(idx + 1) in
          parse_args argv (idx + 2) data ecl new_format
    | arg when String.is_prefix arg ~prefix:"-" ->
        Out_channel.output_string Out_channel.stderr
          (Printf.sprintf "Error: Unknown option '%s'\n" arg);
        Stdlib.exit 1
    | arg ->
        if Option.is_some data then (
          Out_channel.output_string Out_channel.stderr
            "Error: Too many positional arguments\n";
          Stdlib.exit 1)
        else parse_args argv (idx + 1) (Some (parse_data arg)) ecl format

let () =
  let data, ecl, format =
    parse_args (Sys.get_argv ()) 1 None Config.ECL.M Ascii
  in
  match data with
  | Some d -> run d ecl format
  | None ->
      Out_channel.output_string Out_channel.stderr
        "Error: DATA argument is required\n";
      print_help ();
      Stdlib.exit 1
