open Oxqr

open Base

let run data ecl =
  let qr = Encoding.generate_qr data ecl in
  let qr_string = Qr.to_unicode_string qr in
  Out_channel.output_string Out_channel.stdout qr_string

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
      Stdlib.exit 2

let print_help () =
  Out_channel.output_string Out_channel.stdout "Usage: oxqr [OPTIONS] DATA\n";
  Out_channel.output_string Out_channel.stdout "\nGenerate a QR code from alphanumeric input.\n";
  Out_channel.output_string Out_channel.stdout "\nOPTIONS:\n";
  Out_channel.output_string Out_channel.stdout "  --ecl ECL       Error correction level (L|M|Q|H). Default: M.\n";
  Out_channel.output_string Out_channel.stdout "  -h, --help      Show this help message.\n"

let rec parse_args argv idx data ecl =
  if idx >= Array.length argv then (data, ecl)
  else
    match argv.(idx) with
    | "-h" | "--help" ->
        print_help ();
        Stdlib.exit 0
    | "--ecl" ->
        if idx + 1 >= Array.length argv then (
          Out_channel.output_string Out_channel.stderr "Error: --ecl requires an argument\n";
          Stdlib.exit 2)
        else
          let new_ecl = parse_ecl argv.(idx + 1) in
          parse_args argv (idx + 2) data new_ecl
    | arg when String.is_prefix arg ~prefix:"-" ->
        Out_channel.output_string Out_channel.stderr
          (Printf.sprintf "Error: Unknown option '%s'\n" arg);
        Stdlib.exit 2
    | arg ->
        if Option.is_some data then (
          Out_channel.output_string Out_channel.stderr
            "Error: Too many positional arguments\n";
          Stdlib.exit 2)
        else parse_args argv (idx + 1) (Some arg) ecl

let () =
  let data, ecl = parse_args (Sys.get_argv ()) 1 None Config.ECL.M in
  match data with
  | Some d -> run d ecl
  | None ->
      Out_channel.output_string Out_channel.stderr
        "Error: DATA argument is required\n";
      print_help ();
      Stdlib.exit 2
