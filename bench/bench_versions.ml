open Oxqr

(* Target ~300ms of wall time per measurement regardless of version size.
   Small versions (~5 μs/call) get ~60k iterations; large ones (~500 μs/call)
   get ~600.  Both give consistent sub-1% noise. *)
let target_s = 0.3
let min_iters = 300
let max_iters = 100_000

(* Scan increasing string lengths to find the minimum-length alphanumeric input
   that forces exactly each version 1..40.  Config.get_config is pure and fast
   (table lookup), so the linear scan is negligible. *)
let version_inputs =
  let arr = Array.make 41 "" in
  let highest = ref 0 in
  let len = ref 0 in
  while !highest < 40 do
    incr len;
    let s = String.make !len 'A' in
    let v = (Config.get_config s Config.ECL.L).version in
    if v > !highest then begin
      for vv = !highest + 1 to v do arr.(vv) <- s done;
      highest := v
    end
  done;
  arr

(* Probe a single call to estimate cost, compute iteration count, then measure. *)
let measure f =
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  let single_s = max 1e-9 (t1 -. t0) in
  let n = max min_iters (min max_iters (int_of_float (target_s /. single_s))) in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n do f () done;
  let t1 = Unix.gettimeofday () in
  ((t1 -. t0) *. 1.0e9 /. float_of_int n, n)

let () =
  let ecl = Config.ECL.L in
  Printf.printf "version,input_chars,heap_ns,stack_ns,heap_iters,stack_iters\n%!";
  for v = 1 to 40 do
    let data = version_inputs.(v) in
    let heap_arena  = Encoding.Arena.create None in
    let stack_arena = Encoding.Arena.create (Some v) in
    (* Warmup: both paths, enough iterations to prime instruction caches *)
    for _ = 1 to 200 do
      let _qr = Encoding.generate_qr heap_arena data ecl in ();
      Encoding.generate_qr_stack stack_arena data ecl
    done;
    Gc.full_major ();
    let (heap_ns, hi) = measure (fun () ->
      let _qr = Encoding.generate_qr heap_arena data ecl in ()) in
    Gc.full_major ();
    let (stack_ns, si) = measure (fun () ->
      Encoding.generate_qr_stack stack_arena data ecl) in
    Printf.printf "%d,%d,%.1f,%.1f,%d,%d\n%!"
      v (String.length data) heap_ns stack_ns hi si
  done
