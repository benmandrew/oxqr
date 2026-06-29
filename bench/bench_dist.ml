open Oxqr

(* Nanosecond monotonic clock via C stub.  [@@noalloc] means:
   - the call is not a GC safe-point, so no collection can slip between
     t0 = time_ns() and the function under test, or between it returning
     and t1 = time_ns();
   - zero overhead: no OCaml register save/restore around the C call. *)
external time_ns : unit -> int = "bench_time_ns" [@@noalloc]

(* 5 000 samples gives stable p50/p90/p99 and directionally useful p99.9
   (5 data points in the top 0.1%). *)
let n_samples = 100_000

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

(* Collect n_samples individual call latencies, return them sorted ascending. *)
let collect f =
  let times = Array.make n_samples 0 in
  for i = 0 to n_samples - 1 do
    let t0 = time_ns () in
    f ();
    let t1 = time_ns () in
    times.(i) <- t1 - t0
  done;
  Array.sort compare times;
  times

(* p in [0.0, 1.0] *)
let pct arr p =
  arr.(int_of_float (float_of_int (Array.length arr - 1) *. p))

let () =
  let ecl = Config.ECL.L in
  Printf.printf
    "version,input_chars,\
     heap_p50,heap_p90,heap_p95,heap_p99,heap_p999,heap_max,\
     stack_p50,stack_p90,stack_p95,stack_p99,stack_p999,stack_max\n%!";
  for v = 1 to 40 do
    let data = version_inputs.(v) in
    let heap_arena  = Encoding.Arena.create None in
    let stack_arena = Encoding.Arena.create (Some v) in
    (* Warmup: prime instruction caches and branch predictors for both paths. *)
    for _ = 1 to 500 do
      let _qr = Encoding.generate_qr heap_arena data ecl in ();
      Encoding.generate_qr_stack stack_arena data ecl
    done;
    (* Clean heap before each measurement so one path's garbage doesn't
       affect the other's GC timing. *)
    Gc.full_major ();
    let ht = collect (fun () ->
      let _qr = Encoding.generate_qr heap_arena data ecl in ()) in
    Gc.full_major ();
    let st = collect (fun () ->
      Encoding.generate_qr_stack stack_arena data ecl) in
    Printf.printf "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n%!"
      v (String.length data)
      (pct ht 0.50) (pct ht 0.90) (pct ht 0.95) (pct ht 0.99) (pct ht 0.999) (pct ht 1.0)
      (pct st 0.50) (pct st 0.90) (pct st 0.95) (pct st 0.99) (pct st 0.999) (pct st 1.0)
  done
