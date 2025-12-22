open Oxqr

let () =
  let qr = Encoding.generate "HELLO WORLD" Config.ECL.M in
  let qr_string = Qr.to_unicode_string qr in
  print_string qr_string
