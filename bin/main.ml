open Oxqr

let run data ecl =
  let qr = Encoding.generate_qr data ecl in
  let qr_string = Qr.to_unicode_string qr in
  print_string qr_string

let data_arg =
  let doc = "Alphanumeric data to encode as a QR code." in
  let info = Cmdliner.Arg.info [] ~docv:"DATA" ~doc in
  Cmdliner.Arg.required
    (Cmdliner.Arg.pos 0 (Cmdliner.Arg.some Cmdliner.Arg.string) None info)

let string_of_ecl = function
  | Config.ECL.L -> "L"
  | M -> "M"
  | Q -> "Q"
  | H -> "H"

let parse_ecl =
  let open Config.ECL in
  fun s ->
    match String.uppercase_ascii s with
    | "L" -> Ok L
    | "M" -> Ok M
    | "Q" -> Ok Q
    | "H" -> Ok H
    | _ -> Error (`Msg "ECL must be one of L, M, Q, H")

let pp_ecl fmt ecl = Format.pp_print_string fmt (string_of_ecl ecl)
let ecl_conv = Cmdliner.Arg.conv (parse_ecl, pp_ecl)

let ecl_arg =
  let doc = "Error correction level (L|M|Q|H). Default: M." in
  let info = Cmdliner.Arg.info [ "ecl" ] ~docv:"ECL" ~doc in
  Cmdliner.Arg.value (Cmdliner.Arg.opt ecl_conv Config.ECL.M info)

let term =
  let open Cmdliner.Term in
  app (app (const run) data_arg) ecl_arg

let cmd =
  let doc = "Generate a QR code from alphanumeric input" in
  let exits =
    Cmdliner.Cmd.Exit.info ~doc:"on invalid input or execution error" 2
  in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "oxqr" ~doc ~exits:[ exits ]) term

let () = exit (Cmdliner.Cmd.eval cmd)
