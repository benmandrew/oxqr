open Oxqr

let generate s ecl =
  let config, capacity = Config.get_config_and_capacity s ecl in
  let buf = Buf.create ~bytes:(capacity + 10) in
  (* Add mode indicator for Alphanumeric mode (4 bits). *)
  Buf.add_bits buf Config.mode_indicator Config.mode_indicator_length;
  (* Add character count indicator. *)
  let cci_len = Config.char_count_indicator_length config in
  Buf.add_bits buf (String.length s) cci_len;
  "Hello, World!" (* Placeholder implementation *)

let () =
  let qr_code = generate "HELLO WORLD" Config.ECL.M in
  Printf.printf "Generated QR Code Data: %s\n" qr_code
