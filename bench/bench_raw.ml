open Oxqr

external time_ns : unit -> int = "bench_time_ns" [@@noalloc]

(* Dump raw per-call heap-path latencies (ns) for a single version, one per
   line, so downstream tooling can bootstrap tail-percentile stability. *)

let () =
  let v = int_of_string Sys.argv.(1) in
  let n = int_of_string Sys.argv.(2) in
  let ecl = Config.ECL.L in
  (* Build an input string that lands on version v (ECL L). *)
  let data =
    let s = ref "" in
    let len = ref 0 in
    let found = ref None in
    while !found = None do
      incr len;
      let cand = String.make !len 'A' in
      if (Config.get_config cand ecl).version >= v then found := Some cand;
      s := cand
    done;
    Option.get !found
  in
  let heap_arena = Encoding.Arena.create None in
  for _ = 1 to 500 do
    let _qr = Encoding.generate_qr heap_arena data ecl in ()
  done;
  Gc.full_major ();
  let times = Array.make n 0 in
  for i = 0 to n - 1 do
    let t0 = time_ns () in
    let _qr = Encoding.generate_qr heap_arena data ecl in ();
    let t1 = time_ns () in
    times.(i) <- t1 - t0
  done;
  let b = Buffer.create (n * 6) in
  for i = 0 to n - 1 do
    Buffer.add_string b (string_of_int times.(i));
    Buffer.add_char b '\n'
  done;
  print_string (Buffer.contents b)
