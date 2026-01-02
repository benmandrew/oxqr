open Oxqr
open Base

(* Example from https://www.thonky.com/qr-code-tutorial/error-correction-coding#use-the-terms-of-the-remainder-as-the-error-correction-codewords *)
let%expect_test "test_error_correction_generation" =
  let arena = Encoding.Arena.create None in
  let total_data_codewords = Encoding.encode arena "HELLO WORLD" Config.ECL.M in
  let message_bytes = Encoding.Arena.get_qr_buffer arena in
  for i = 0 to total_data_codewords - 1 do
    Stdlib.Printf.printf "%d " (Char.to_int (Bytes.get message_bytes i))
  done;
  Stdlib.Printf.printf "\n";
  [%expect "32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17"];
  let ec_data = Bytes.create 10 in
  Reed_solomon.generate_error_correction
    (Encoding.Arena.get_remainder_scratch arena)
    message_bytes ~pos:0 ~len:total_data_codewords 10 ec_data ~out_pos:0;
  for i = 0 to Bytes.length ec_data - 1 do
    Stdlib.Printf.printf "%d " (Char.to_int (Bytes.get ec_data i))
  done;
  Stdlib.Printf.printf "\n";
  [%expect "196 35 39 119 235 215 231 226 93 23"]
