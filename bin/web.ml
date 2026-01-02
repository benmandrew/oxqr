open Oxqr
open Base
open Js_of_ocaml

(* Generate QR code as SVG *)
let generate_qr_svg data_js ecl_str_js =
  let data = Js.to_string data_js in
  let ecl_str = Js.to_string ecl_str_js in
  let arena = Encoding.Arena.create None in
  let ecl =
    match String.uppercase ecl_str with
    | "L" -> Config.ECL.L
    | "M" -> Config.ECL.M
    | "Q" -> Config.ECL.Q
    | "H" -> Config.ECL.H
    | _ -> Config.ECL.M (* Default to M if invalid *)
  in
  let qr = Encoding.generate_qr arena data ecl in
  let result = Svg.qr_to_svg qr in
  Js.string result

(* Export both functions for use in js_of_ocaml *)
let () = Js.export "generateQR" (Js.wrap_callback generate_qr_svg)
