open Oxqr

let generate s ecl =
  let config, _capacity = Config.get_config_and_capacity s ecl in
  (* TODO: Implement QR code encoding *)
  Printf.sprintf "Config: version=%d" config.version

let () =
  let qr_code = generate "HELLO WORLD" Config.ECL.M in
  Printf.printf "Generated QR Code Data: %s\n" qr_code
