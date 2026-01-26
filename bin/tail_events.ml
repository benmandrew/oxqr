open Oxqr
open Base

[@@@warning "-69"] (* Disable unused field warnings *)

let random_alphanumeric_string len =
  let chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:" in
  let chars_len = String.length chars in
  let rec build_string acc n =
    if n <= 0 then acc
    else
      let idx = Random.int chars_len in
      let c = chars.[idx] in
      build_string (String.of_char c ^ acc) (n - 1)
  in
  build_string "" len

(* Store timing results *)
let timing_results = ref []

(* High-resolution timing function with warmup and multiple samples *)
let time_function_precise f ~warmup_iterations ~samples =
  (* Warmup phase to stabilize cache/branch predictor *)
  for _ = 1 to warmup_iterations do
    let _ = f () in
    ()
  done;
  (* Take multiple samples and return all measurements *)
  let measurements = ref [] in
  for _ = 1 to samples do
    let start_time = Core_unix.gettimeofday () in
    let _ = f () in
    let end_time = Core_unix.gettimeofday () in
    let duration = end_time -. start_time in
    measurements := duration :: !measurements
  done;
  !measurements

(* Simple timing function (backwards compatibility) *)
let time_function f =
  let start_time = Core_unix.gettimeofday () in
  let result = f () in
  let end_time = Core_unix.gettimeofday () in
  let duration = end_time -. start_time in
  timing_results := duration :: !timing_results;
  result

let gc_count = ref 0
let minor_gc_count = ref 0
let major_gc_count = ref 0

let runtime_begin _device _ts phase =
  match phase with
  | Runtime_events.EV_MINOR ->
      Int.incr minor_gc_count;
      Int.incr gc_count
  | Runtime_events.EV_MAJOR ->
      Int.incr major_gc_count;
      Int.incr gc_count
  | _ -> ()

let runtime_end _device _ts _phase = ()
let runtime_counter _device _ts _counter _n = ()

let calculate_percentiles sorted_timings count =
  let p50_idx = count / 2 in
  let p90_idx = count * 9 / 10 in
  let p95_idx = count * 95 / 100 in
  let p99_idx = count * 99 / 100 in
  let p99_9_idx = count * 999 / 1000 in
  let p50 = List.nth_exn sorted_timings p50_idx in
  let p90 = List.nth_exn sorted_timings p90_idx in
  let p95 = List.nth_exn sorted_timings p95_idx in
  let p99 = List.nth_exn sorted_timings p99_idx in
  let p99_9 = List.nth_exn sorted_timings p99_9_idx in
  (p50, p90, p95, p99, p99_9)

let calculate_basic_stats timings =
  let count = List.length timings in
  let sum = List.fold timings ~init:0.0 ~f:( +. ) in
  let mean = sum /. Float.of_int count in
  let sorted_timings = List.sort timings ~compare:Float.compare in
  let min_time = List.hd_exn sorted_timings in
  let max_time = List.last_exn sorted_timings in
  let variance =
    List.fold timings ~init:0.0 ~f:(fun acc t ->
        let diff = t -. mean in
        acc +. (diff *. diff))
    /. Float.of_int count
  in
  let std_dev = Float.sqrt variance in
  (count, mean, min_time, max_time, std_dev, sorted_timings)

(* Advanced variability metrics for statistical comparison *)
type variability_stats = {
  mean : float;
  std_dev : float;
  variance : float;
  min_time : float;
  max_time : float;
  range : float;
  coeff_variation : float;
  iqr : float;
  iqr_coeff : float;
  p50 : float;
  p90 : float;
  p95 : float;
  p99 : float;
  p99_9 : float;
  p99_p50_ratio : float;
  p99_9_p50_ratio : float;
  p90_p10_ratio : float;
  mad : float;
  mad_coeff : float;
  rel_variance : float;
}

let calculate_variability_metrics timings =
  let count, mean, min_time, max_time, std_dev, sorted_timings =
    calculate_basic_stats timings
  in
  let variance = std_dev *. std_dev in
  let range = max_time -. min_time in
  let coeff_variation = std_dev /. mean in
  let p50, p90, p95, p99, p99_9 = calculate_percentiles sorted_timings count in

  (* Interquartile range *)
  let p25_idx = count / 4 in
  let p75_idx = count * 3 / 4 in
  let p25 = List.nth_exn sorted_timings p25_idx in
  let p75 = List.nth_exn sorted_timings p75_idx in
  let iqr = p75 -. p25 in
  let iqr_coeff = iqr /. p50 in

  (* IQR relative to median *)

  (* Tail heaviness metrics *)
  let p99_p50_ratio = p99 /. p50 in
  let p99_9_p50_ratio = p99_9 /. p50 in
  let p90_p10_ratio =
    let p10_idx = count / 10 in
    let p10 = List.nth_exn sorted_timings p10_idx in
    p90 /. p10
  in

  (* Mean absolute deviation from median (robust measure) *)
  let mad =
    List.fold timings ~init:0.0 ~f:(fun acc x -> acc +. Float.abs (x -. p50))
    /. Float.of_int count
  in
  let mad_coeff = mad /. p50 in

  (* Relative variance (variance normalized by square of mean) *)
  let rel_variance = variance /. (mean *. mean) in

  {
    mean;
    std_dev;
    variance;
    min_time;
    max_time;
    range;
    coeff_variation;
    iqr;
    iqr_coeff;
    p50;
    p90;
    p95;
    p99;
    p99_9;
    p99_p50_ratio;
    p99_9_p50_ratio;
    p90_p10_ratio;
    mad;
    mad_coeff;
    rel_variance;
  }

(* Statistical tests for comparing variability *)
let variance_ratio_test stack_var heap_var stack_n heap_n =
  let f_stat = heap_var /. stack_var in
  let df1 = heap_n - 1 in
  let df2 = stack_n - 1 in
  (* Simple critical value approximation *)
  let critical_05 =
    if Float.(f_stat > 1.2) then "significant at p<0.05" else "not significant"
  in
  let critical_01 =
    if Float.(f_stat > 1.5) then "significant at p<0.01" else "not significant"
  in
  (f_stat, df1, df2, critical_05, critical_01)

(* Levene's test for equal variances (more robust than F-test) *)
let levenes_test stack_timings heap_timings =
  let stack_median =
    let sorted = List.sort stack_timings ~compare:Float.compare in
    List.nth_exn sorted (List.length sorted / 2)
  in
  let heap_median =
    let sorted = List.sort heap_timings ~compare:Float.compare in
    List.nth_exn sorted (List.length sorted / 2)
  in

  (* Calculate absolute deviations from median *)
  let stack_devs =
    List.map stack_timings ~f:(fun x -> Float.abs (x -. stack_median))
  in
  let heap_devs =
    List.map heap_timings ~f:(fun x -> Float.abs (x -. heap_median))
  in

  let stack_mean_dev =
    List.fold stack_devs ~init:0.0 ~f:( +. )
    /. Float.of_int (List.length stack_devs)
  in
  let heap_mean_dev =
    List.fold heap_devs ~init:0.0 ~f:( +. )
    /. Float.of_int (List.length heap_devs)
  in

  (* Test: difference in mean absolute deviations *)
  let dev_ratio = heap_mean_dev /. stack_mean_dev in
  let interpretation =
    if Float.(dev_ratio > 1.15) then "Heap significantly more variable"
    else if Float.(dev_ratio < 0.85) then "Stack significantly more variable"
    else "No significant difference"
  in
  (dev_ratio, stack_mean_dev, heap_mean_dev, interpretation)

let analyze_tail_events timings p95 =
  let tail_threshold = p95 *. 2.0 in
  let moderate_threshold = p95 *. 1.5 in
  (* Lower threshold for detection *)
  let mild_threshold = p95 *. 1.2 in

  (* Even lower threshold *)
  let tail_events =
    List.filter timings ~f:(fun t -> Float.(t > tail_threshold))
  in
  let moderate_events =
    List.filter timings ~f:(fun t -> Float.(t > moderate_threshold))
  in
  let mild_events =
    List.filter timings ~f:(fun t -> Float.(t > mild_threshold))
  in

  let tail_count = List.length tail_events in
  let moderate_count = List.length moderate_events in
  let mild_count = List.length mild_events in

  let tail_mean =
    if tail_count > 0 then
      List.fold tail_events ~init:0.0 ~f:( +. ) /. Float.of_int tail_count
    else 0.0
  in
  ( tail_threshold,
    tail_count,
    tail_mean,
    moderate_threshold,
    moderate_count,
    mild_threshold,
    mild_count )

let print_variability_analysis stack_results heap_results =
  let stack_stats = calculate_variability_metrics stack_results in
  let heap_stats = calculate_variability_metrics heap_results in

  Stdlib.Printf.printf "\n=== Statistical Variability Analysis ===\n";

  (* Basic comparison *)
  Stdlib.Printf.printf "\nBasic Variability Metrics:\n";
  Stdlib.Printf.printf
    "                    Stack        Heap         Heap/Stack\n";
  Stdlib.Printf.printf "Std Deviation:      %.6f     %.6f     %.3f\n"
    stack_stats.std_dev heap_stats.std_dev
    (heap_stats.std_dev /. stack_stats.std_dev);
  Stdlib.Printf.printf "Coeff of Variation: %.6f     %.6f     %.3f\n"
    stack_stats.coeff_variation heap_stats.coeff_variation
    (heap_stats.coeff_variation /. stack_stats.coeff_variation);
  Stdlib.Printf.printf "Range (max-min):    %.6f     %.6f     %.3f\n"
    stack_stats.range heap_stats.range
    (heap_stats.range /. stack_stats.range);
  Stdlib.Printf.printf "IQR Coefficient:    %.6f     %.6f     %.3f\n"
    stack_stats.iqr_coeff heap_stats.iqr_coeff
    (heap_stats.iqr_coeff /. stack_stats.iqr_coeff);

  (* Tail behavior *)
  Stdlib.Printf.printf "\nTail Behavior (Percentile Ratios):\n";
  Stdlib.Printf.printf "P99/P50:            %.3f        %.3f        %.3f\n"
    stack_stats.p99_p50_ratio heap_stats.p99_p50_ratio
    (heap_stats.p99_p50_ratio /. stack_stats.p99_p50_ratio);
  Stdlib.Printf.printf "P99.9/P50:          %.3f        %.3f        %.3f\n"
    stack_stats.p99_9_p50_ratio heap_stats.p99_9_p50_ratio
    (heap_stats.p99_9_p50_ratio /. stack_stats.p99_9_p50_ratio);
  Stdlib.Printf.printf "P90/P10:            %.3f        %.3f        %.3f\n"
    stack_stats.p90_p10_ratio heap_stats.p90_p10_ratio
    (heap_stats.p90_p10_ratio /. stack_stats.p90_p10_ratio);

  (* Robust measures *)
  Stdlib.Printf.printf "\nRobust Variability Measures:\n";
  Stdlib.Printf.printf "MAD Coefficient:    %.6f     %.6f     %.3f\n"
    stack_stats.mad_coeff heap_stats.mad_coeff
    (heap_stats.mad_coeff /. stack_stats.mad_coeff);
  Stdlib.Printf.printf "Relative Variance:  %.6f     %.6f     %.3f\n"
    stack_stats.rel_variance heap_stats.rel_variance
    (heap_stats.rel_variance /. stack_stats.rel_variance);

  (* Statistical tests *)
  Stdlib.Printf.printf "\nStatistical Tests:\n";
  let f_stat, _df1, _df2, crit_05, crit_01 =
    variance_ratio_test stack_stats.variance heap_stats.variance
      (List.length stack_results)
      (List.length heap_results)
  in
  Stdlib.Printf.printf "F-test (variance ratio): %.3f (%s, %s)\n" f_stat crit_05
    crit_01;

  let dev_ratio, _stack_dev, _heap_dev, interpretation =
    levenes_test stack_results heap_results
  in
  Stdlib.Printf.printf "Levene's test (MAD ratio): %.3f (%s)\n" dev_ratio
    interpretation;

  (* Overall assessment *)
  Stdlib.Printf.printf "\nOverall Assessment:\n";
  let variability_indicators =
    [
      ("Standard deviation", heap_stats.std_dev /. stack_stats.std_dev);
      ( "Coefficient of variation",
        heap_stats.coeff_variation /. stack_stats.coeff_variation );
      ("IQR coefficient", heap_stats.iqr_coeff /. stack_stats.iqr_coeff);
      ("P99/P50 ratio", heap_stats.p99_p50_ratio /. stack_stats.p99_p50_ratio);
      ("MAD coefficient", heap_stats.mad_coeff /. stack_stats.mad_coeff);
    ]
  in

  let higher_variability =
    List.count variability_indicators ~f:(fun (_, ratio) -> Float.(ratio > 1.1))
  in
  let total_indicators = List.length variability_indicators in

  Stdlib.Printf.printf "Indicators showing heap more variable: %d/%d\n"
    higher_variability total_indicators;
  if higher_variability >= total_indicators / 2 then
    Stdlib.Printf.printf
      "✓ Statistical evidence: Heap allocation has higher timing variability\n"
  else
    Stdlib.Printf.printf
      "⚠ Insufficient evidence: Timing variability appears similar\n"

let analyze_tail_events timings p95 =
  let tail_threshold = p95 *. 2.0 in
  let moderate_threshold = p95 *. 1.5 in
  let mild_threshold = p95 *. 1.2 in

  (* Even lower threshold *)
  let tail_events =
    List.filter timings ~f:(fun t -> Float.(t > tail_threshold))
  in
  let moderate_events =
    List.filter timings ~f:(fun t -> Float.(t > moderate_threshold))
  in
  let mild_events =
    List.filter timings ~f:(fun t -> Float.(t > mild_threshold))
  in

  let tail_count = List.length tail_events in
  let moderate_count = List.length moderate_events in
  let mild_count = List.length mild_events in

  let tail_mean =
    if tail_count > 0 then
      List.fold tail_events ~init:0.0 ~f:( +. ) /. Float.of_int tail_count
    else 0.0
  in
  ( tail_threshold,
    tail_count,
    tail_mean,
    moderate_threshold,
    moderate_count,
    mild_threshold,
    mild_count )

let print_timing_results function_name =
  let timings = List.rev !timing_results in
  let count = List.length timings in
  let printf = Stdlib.Printf.printf in
  if count > 0 then (
    let count, mean, min_time, max_time, std_dev, sorted_timings =
      calculate_basic_stats timings
    in
    let p50, p90, p95, p99, p99_9 =
      calculate_percentiles sorted_timings count
    in
    let ( tail_threshold,
          tail_count,
          tail_mean,
          moderate_threshold,
          moderate_count,
          mild_threshold,
          mild_count ) =
      analyze_tail_events timings p95
    in
    let worst_time, worst_idx =
      List.foldi timings ~init:(Float.neg_infinity, -1)
        ~f:(fun idx (cur_max, cur_idx) t ->
          if Float.compare t cur_max > 0 then (t, idx) else (cur_max, cur_idx))
    in
    printf "\n=== %s Timing Analysis ===\n" function_name;
    printf "Total QR codes generated: %d\n" count;
    printf "Total GC events: %d (minor: %d, major: %d)\n" !gc_count
      !minor_gc_count !major_gc_count;
    printf "\nTiming Statistics (seconds):\n";
    printf "  Mean:    %.6f\n" mean;
    printf "  Min:     %.6f\n" min_time;
    printf "  Max:     %.6f\n" max_time;
    printf "  Std Dev: %.6f\n" std_dev;
    printf "  Worst-case: %.6f (iteration %d)\n" worst_time (worst_idx + 1);
    printf "\nPercentiles (seconds):\n";
    printf "  P50:     %.6f\n" p50;
    printf "  P90:     %.6f\n" p90;
    printf "  P95:     %.6f\n" p95;
    printf "  P99:     %.6f\n" p99;
    printf "  P99.9:   %.6f\n" p99_9;
    printf "\n=== Tail Event Analysis ===\n";
    printf "Events exceeding 2x P95 threshold (%.6fs): %d (%.2f%%)\n"
      tail_threshold tail_count
      (Float.of_int tail_count /. Float.of_int count *. 100.0);
    printf "Events exceeding 1.5x P95 threshold (%.6fs): %d (%.2f%%)\n"
      moderate_threshold moderate_count
      (Float.of_int moderate_count /. Float.of_int count *. 100.0);
    printf "Events exceeding 1.2x P95 threshold (%.6fs): %d (%.2f%%)\n"
      mild_threshold mild_count
      (Float.of_int mild_count /. Float.of_int count *. 100.0);
    if tail_count > 0 then
      printf "Tail events mean duration: %.6f seconds\n" tail_mean;
    printf "\nGC correlation:\n";
    printf "  GC events per QR generation: %.3f\n"
      (Float.of_int !gc_count /. Float.of_int count);

    (* Additional diagnostic info *)
    printf "\nTiming Distribution Analysis:\n";
    let range = max_time -. min_time in
    let cv = std_dev /. mean in
    (* Coefficient of variation *)
    printf "  Range (max-min): %.6f seconds (%.3f ms)\n" range (range *. 1000.0);
    printf "  Coefficient of variation: %.3f\n" cv;
    printf "  99.9th percentile / median ratio: %.2fx\n" (p99_9 /. p50))
  else printf "No timing data collected for %s.\n" function_name

let reset_counters () =
  timing_results := [];
  gc_count := 0;
  minor_gc_count := 0;
  major_gc_count := 0

let setup_runtime_events () =
  Runtime_events.start ();
  let cursor = Runtime_events.create_cursor None in
  let callbacks =
    Runtime_events.Callbacks.create ~runtime_begin ~runtime_end ~runtime_counter
      ()
  in
  (cursor, callbacks)

let write_results_to_csv function_name =
  let timings = List.rev !timing_results in
  let filename = "generate_qr_" ^ function_name ^ "_timings.csv" in
  let oc = Out_channel.open_text filename in
  Out_channel.output_string oc
    "iteration,duration_seconds,tail_event,moderate_event,mild_event\n";
  let count = List.length timings in
  let sorted_timings = List.sort timings ~compare:Float.compare in
  let p95_idx = count * 95 / 100 in
  let p95 = List.nth_exn sorted_timings p95_idx in
  let tail_threshold = p95 *. 2.0 in
  let moderate_threshold = p95 *. 1.5 in
  let mild_threshold = p95 *. 1.2 in
  List.iteri timings ~f:(fun i duration ->
      let tail_flag = if Float.(duration > tail_threshold) then "1" else "0" in
      let moderate_flag =
        if Float.(duration > moderate_threshold) then "1" else "0"
      in
      let mild_flag = if Float.(duration > mild_threshold) then "1" else "0" in
      Out_channel.output_string oc
        (Stdlib.Printf.sprintf "%d,%.9f,%s,%s,%s\n" (i + 1) duration tail_flag
           moderate_flag mild_flag));
  Out_channel.close oc;
  Stdlib.Printf.printf "Timing data written to %s\n" filename

let run_qr_experiment ~test_data ~arena ~num_iterations ~warmup_iterations =
  let experiments =
    [
      ("stack", Encoding.generate_qr_stack);
      ( "heap",
        fun arena data ecl ->
          let _ = Encoding.generate_qr arena data ecl in
          () );
    ]
  in
  let results =
    List.map experiments ~f:(fun (label, qr_function) ->
        reset_counters ();
        Stdlib.Printf.printf "\n=== Timing: %s ===\n" label;
        Stdlib.Printf.printf "Warming up...\n";
        for _ = 1 to warmup_iterations do
          ignore (qr_function arena test_data Config.ECL.M)
        done;
        Stdlib.Gc.full_major ();
        Stdlib.Gc.compact ();
        let cursor, callbacks = setup_runtime_events () in
        Stdlib.Printf.printf "Measuring...\n";
        for i = 1 to num_iterations do
          if Int.rem i 100 = 0 then
            Stdlib.Printf.printf "Progress: %d/%d\r%!" i num_iterations;
          let measurements =
            time_function_precise
              (fun () -> qr_function arena test_data Config.ECL.M)
              ~warmup_iterations:1 ~samples:3
          in
          let min_measurement =
            List.fold measurements ~init:Float.max_value ~f:Float.min
          in
          timing_results := min_measurement :: !timing_results;
          if Int.rem i 50 = 0 then
            ignore (Runtime_events.read_poll cursor callbacks None)
        done;
        ignore (Runtime_events.read_poll cursor callbacks None);
        Runtime_events.free_cursor cursor;
        Stdlib.Printf.printf "\nDone!\n";
        print_timing_results label;
        write_results_to_csv label;
        (label, List.rev !timing_results))
  in
  (* Optional: print a simple comparison summary *)
  match results with
  | [ (_, stack); (_, heap) ] -> print_variability_analysis stack heap
  | _ -> ()

let () =
  Stdlib.Printf.printf "=== QR Code Generation Timing Analysis ===\n";
  let str_len = 10 in
  let num_iterations = 200000 in
  let warmup_iterations = num_iterations / 10 in
  let test_data = random_alphanumeric_string str_len in
  let config = Config.get_config test_data Config.ECL.M in
  let qr_version = config.version in
  Stdlib.Printf.printf "Test data: %d characters, QR version: %d\n" str_len
    qr_version;
  let arena = Encoding.Arena.create (Some qr_version) in
  Stdlib.Printf.printf "Starting experiment...\n";
  run_qr_experiment ~test_data ~arena ~num_iterations ~warmup_iterations
