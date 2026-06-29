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
  Landmark.register "generate_qr_stack"

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
      for _ = 1 to iters do
        Landmark.enter bench_generate_qr;
        let _qr = Encoding.generate_qr heap_arena data ecl in
        Landmark.exit bench_generate_qr
      done;
      for _ = 1 to iters do
        Landmark.enter bench_generate_qr_stack;
        Encoding.generate_qr_stack_bench stack_arena data ecl;
        Landmark.exit bench_generate_qr_stack
      done
    ) ecls
  ) inputs;
  Landmark.stop_profiling ();
  let graph = Landmark.export () in
  let agg = Landmark.Graph.aggregate_landmarks graph in
  Landmark.Graph.output ~threshold:0.0 stderr agg
