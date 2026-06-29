open Oxqr

let () =
  Landmark.start_profiling
    ~profiling_options:{ Landmark.default_options with
        allocated_bytes = true;
        output = Silent }
    ()

let bench_generate_qr =
  Landmark.register "generate_qr"

let bench_generate_qr_stack =
  Landmark.register "generate_qr_stack (landmark)"

(* Direct call to the [@zero_alloc] function — no inner Landmark calls.
   The only Landmark overhead is one enter+exit wrapping each call site. *)
let bench_generate_qr_stack_direct =
  Landmark.register "generate_qr_stack (direct)"

let inputs = [
  "HELLO WORLD";
  "HTTPS://EXAMPLE.COM/PATH/TO/PAGE";
  "HTTPS://EXAMPLE.COM/VERY/LONG/PATH/WITH/MANY/SEGMENTS/AND/MORE/DATA";
]

let ecls = Config.ECL.[L; M; Q; H]

let iters = 10_000

let () =
  List.iter (fun data ->
    List.iter (fun ecl ->
      let config = Config.get_config data ecl in
      let heap_arena = Encoding.Arena.create None in
      let stack_arena = Encoding.Arena.create (Some config.version) in
      let stack_arena_direct = Encoding.Arena.create (Some config.version) in
      for _ = 1 to iters do
        Landmark.enter bench_generate_qr;
        let _qr = Encoding.generate_qr heap_arena data ecl in
        Landmark.exit bench_generate_qr
      done;
      for _ = 1 to iters do
        Landmark.enter bench_generate_qr_stack;
        Encoding.generate_qr_stack_bench stack_arena data ecl;
        Landmark.exit bench_generate_qr_stack
      done;
      for _ = 1 to iters do
        Landmark.enter bench_generate_qr_stack_direct;
        Encoding.generate_qr_stack stack_arena_direct data ecl;
        Landmark.exit bench_generate_qr_stack_direct
      done
    ) ecls
  ) inputs;
  Landmark.stop_profiling ();
  let graph = Landmark.export () in
  let agg = Landmark.Graph.aggregate_landmarks graph in
  Landmark.Graph.output ~threshold:0.0 stderr agg

(* ------------------------------------------------------------------ *)
(* GC investigation: allocation pressure across QR versions            *)
(* ------------------------------------------------------------------ *)

let gc_iters = 1000

let gc_row label s0 s1 =
  Printf.eprintf "  %-6s minor_gc=%-5d major_gc=%-3d  minor_words=%8.0f  promoted=%8.0f  major_direct=%8.0f\n"
    label
    (s1.Gc.minor_collections - s0.Gc.minor_collections)
    (s1.Gc.major_collections - s0.Gc.major_collections)
    (s1.Gc.minor_words -. s0.Gc.minor_words)
    (s1.Gc.promoted_words -. s0.Gc.promoted_words)
    (* major_words tracks direct-to-major allocations (large objects that
       bypass the minor heap); promoted_words is NOT included in major_words *)
    (s1.Gc.major_words -. s0.Gc.major_words)

let () =
  Printf.eprintf "\n=== GC configuration ===\n";
  let g = Gc.get () in
  Printf.eprintf "Minor heap: %d words  (~%d KB on 64-bit)\n"
    g.minor_heap_size (g.minor_heap_size * 8 / 1024);

  Printf.eprintf "\n=== Qr.t allocation sizes ===\n";
  Printf.eprintf "  (buf + reserved = 2 * width^2 bytes; both Bytes.make calls)\n";
  List.iter (fun v ->
    let w = (v - 1) * 4 + 21 in
    Printf.eprintf "  v%-2d  width=%-3d  buf+reserved = %6d bytes  (%5.1f KB)\n"
      v w (2 * w * w) (float_of_int (2 * w * w) /. 1024.0)
  ) [1; 5; 10; 20; 30; 40];

  (* Inputs chosen to land near specific versions under ECL L *)
  Printf.eprintf "\n=== GC pressure: heap vs zero-alloc stack path (%d iterations) ===\n" gc_iters;
  let test_cases = [
    ("HELLO WORLD",          Config.ECL.L);  (* v1/2 *)
    (String.make 174 'A',   Config.ECL.L);  (* ~v10 *)
    (String.make 395 'A',   Config.ECL.L);  (* ~v20 *)
    (String.make 706 'A',   Config.ECL.L);  (* ~v30 *)
    (String.make 1268 'A',  Config.ECL.L);  (* ~v40 *)
  ] in
  List.iter (fun (data, ecl) ->
    let config = Config.get_config data ecl in
    let v = config.version in
    let w = (v - 1) * 4 + 21 in
    Printf.eprintf "\nv%d (%d-char input, buf+reserved=%d bytes):\n"
      v (String.length data) (2 * w * w);

    let heap_arena  = Encoding.Arena.create None in
    let stack_arena = Encoding.Arena.create (Some v) in

    Gc.full_major ();
    let s0 = Gc.stat () in
    for _ = 1 to gc_iters do
      let _qr = Encoding.generate_qr heap_arena data ecl in
      ()
    done;
    let s1 = Gc.stat () in
    gc_row "heap:" s0 s1;

    Gc.full_major ();
    let s0 = Gc.stat () in
    for _ = 1 to gc_iters do
      Encoding.generate_qr_stack stack_arena data ecl
    done;
    let s1 = Gc.stat () in
    gc_row "stack:" s0 s1
  ) test_cases
