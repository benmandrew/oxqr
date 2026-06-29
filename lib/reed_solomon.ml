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

(* Precomputed log values of each generator polynomial coefficient.
   Sentinel -1 means the coefficient is zero (GF(256) has no log of 0). *)
let generator_log_polynomials =
  Array.init (Array.length generator_polynomials) ~f:(fun i ->
    Array.map generator_polynomials.(i) ~f:(fun x ->
      if x = 0 then -1 else log_table.(x)))

(** Generate error correction codewords for given data slice and number of ec
    codewords, writing into [out] at [out_pos]. *)
let[@zero_alloc] generate_error_correction (remainder_scratch @ local)
    (data @ local) ~pos ~len ec_count (out @ local) ~out_pos =
  let log_generator = generator_log_polynomials.(ec_count) in
  (* Copy data slice into the beginning of remainder and clear the EC tail. *)
  for i = 0 to len - 1 do
    remainder_scratch.(i) <- Char.to_int (Bytes.get data (pos + i))
  done;
  for i = 0 to ec_count - 1 do
    remainder_scratch.(len + i) <- 0
  done;
  (* Polynomial division: hoist log_table.(coef) out of the inner loop *)
  for i = 0 to len - 1 do
    let coef = remainder_scratch.(i) in
    if coef <> 0 then begin
      let log_coef = log_table.(coef) in
      for j = 0 to ec_count do
        let log_gen = log_generator.(ec_count - j) in
        if log_gen >= 0 then
          remainder_scratch.(i + j) <-
            remainder_scratch.(i + j) lxor exp_table.(log_gen + log_coef)
      done
    end
  done;
  (* Extract the last ec_count bytes as the error correction codewords *)
  for i = 0 to ec_count - 1 do
    Bytes.set out (out_pos + i) (Char.of_int_exn remainder_scratch.(len + i))
  done
