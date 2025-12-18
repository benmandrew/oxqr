open Oxqr

let () =
  let config = Config.make ~version:2 ~ecl:Config.ECL.M in
  let qr = Qr.make ~version:config.version in
  Qr.place_pattern_modules qr config.version;
  let qr_str = Qr.to_unicode_string qr in
  print_endline qr_str
