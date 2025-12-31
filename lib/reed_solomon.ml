(* Reed-Solomon error correction for QR codes *)

open Base

(* x⁸ + x⁴ + x³ + x + 1 *)
let reduction_polynomial = 0x11D

let exp_table, log_table =
  let exp = Array.create ~len:512 0 in
  let log = Array.create ~len:256 0 in
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

let[@zero_alloc] polynomial_mult a b =
  if a = 0 || b = 0 then 0
  else
    let log_a = log_table.(a) in
    let log_b = log_table.(b) in
    exp_table.(log_a + log_b)

(* Precompute generator polynomials once so the lookup below is allocation-free. *)
let generator_polynomials =
  let max_n = 255 in
  let table = Array.create ~len:(max_n + 1) [||] in
  table.(0) <- [| 1 |];
  let poly = Array.create ~len:(max_n + 1) 0 in
  poly.(0) <- 1;
  let current_len = ref 1 in
  for i = 0 to max_n - 1 do
    (* Multiply by (x - α^i) *)
    for j = !current_len downto 1 do
      poly.(j) <- polynomial_mult poly.(j) exp_table.(i) lxor poly.(j - 1)
    done;
    poly.(0) <- polynomial_mult poly.(0) exp_table.(i);
    Int.incr current_len;
    table.(i + 1) <- Array.sub poly ~pos:0 ~len:!current_len
  done;
  table

(** Generate the generator polynomial for n error correction codewords *)
let[@zero_alloc] generate_generator_polynomial n = generator_polynomials.(n)

(* Scratch space sized for the worst QR block (version 40, max data 119, max
   ec 30) with headroom. *)
let remainder_scratch = Array.create ~len:512 0

(** Generate error correction codewords for given data slice and number of ec
    codewords, writing into [out] at [out_pos]. *)
let[@zero_alloc] generate_error_correction (data @ local) ~pos ~len ec_count
    (out @ local) ~out_pos =
  let generator = generate_generator_polynomial ec_count in
  (* Copy data slice into the beginning of remainder and clear the EC tail. *)
  for i = 0 to len - 1 do
    remainder_scratch.(i) <- Char.to_int (Bytes.get data (pos + i))
  done;
  for i = 0 to ec_count - 1 do
    remainder_scratch.(len + i) <- 0
  done;
  (* Polynomial division *)
  for i = 0 to len - 1 do
    let coef = remainder_scratch.(i) in
    if coef <> 0 then
      for j = 0 to ec_count do
        remainder_scratch.(i + j) <-
          remainder_scratch.(i + j)
          lxor polynomial_mult generator.(ec_count - j) coef
      done
  done;
  (* Extract the last ec_count bytes as the error correction codewords *)
  for i = 0 to ec_count - 1 do
    Bytes.set out (out_pos + i) (Char.of_int_exn remainder_scratch.(len + i))
  done
