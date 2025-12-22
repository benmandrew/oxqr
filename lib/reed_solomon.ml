(* Reed-Solomon error correction for QR codes *)

(* x⁸ + x⁴ + x³ + x + 1 *)
let reduction_polynomial = 0x11D

let exp_table, log_table =
  let exp = Array.make 512 0 in
  let log = Array.make 256 0 in
  let x = ref 1 in
  for i = 0 to 254 do
    exp.(i) <- !x;
    log.(!x) <- i;
    x := !x lsl 1;
    if !x land 0x100 <> 0 then x := !x lxor reduction_polynomial
  done;
  (* duplicate to avoid mod 255 *)
  for i = 255 to 511 do
    exp.(i) <- exp.(i - 255)
  done;
  (exp, log)

let polynomial_mult a b =
  if a = 0 || b = 0 then 0
  else
    let log_a = log_table.(a) in
    let log_b = log_table.(b) in
    exp_table.(log_a + log_b)

let generate_generator_polynomial n =
  (* Generate the generator polynomial for n error correction codewords *)
  let poly = Array.make (n + 1) 0 in
  poly.(0) <- 1;
  for i = 0 to n - 1 do
    (* Multiply by (x - α^i) *)
    for j = i + 1 downto 1 do
      poly.(j) <- polynomial_mult poly.(j) exp_table.(i) lxor poly.(j - 1)
    done;
    poly.(0) <- polynomial_mult poly.(0) exp_table.(i)
  done;
  poly

let generate_error_correction data ec_count =
  let data_len = Bytes.length data in
  let generator = generate_generator_polynomial ec_count in
  (* Initialize remainder with message padded by ec_count zeros *)
  let remainder = Array.make (data_len + ec_count) 0 in
  (* Copy data into the beginning of remainder *)
  for i = 0 to data_len - 1 do
    remainder.(i) <- Char.code (Bytes.get data i)
  done;
  (* Polynomial division *)
  for i = 0 to data_len - 1 do
    let coef = remainder.(i) in
    if coef <> 0 then
      for j = 0 to ec_count do
        remainder.(i + j) <-
          remainder.(i + j) lxor polynomial_mult generator.(ec_count - j) coef
      done
  done;
  (* Extract the last ec_count bytes as the error correction codewords *)
  let result = Bytes.create ec_count in
  for i = 0 to ec_count - 1 do
    Bytes.set result i (Char.chr remainder.(data_len + i))
  done;
  result
