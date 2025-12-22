open Oxqr

(* Example from https://www.thonky.com/qr-code-tutorial/error-correction-coding#use-the-terms-of-the-remainder-as-the-error-correction-codewords *)
let%expect_test "test_error_correction_generation" =
  let message = Encoding.encode "HELLO WORLD" Config.ECL.M in
  let message_bytes = Bitbuf.to_bytes message in
  Bytes.iter (fun c -> Printf.printf "%d " (Char.code c)) message_bytes;
  Printf.printf "\n";
  [%expect "32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17"];
  let ec_codewords = Reed_solomon.generate_error_correction message_bytes 10 in
  Bytes.iter (fun c -> Printf.printf "%d " (Char.code c)) ec_codewords;
  Printf.printf "\n";
  [%expect "196 35 39 119 235 215 231 226 93 23"]
