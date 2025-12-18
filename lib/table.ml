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
