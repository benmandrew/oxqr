open Oxqr

(* Nanosecond monotonic clock via the same C stub bench_dist uses.  [@@noalloc]
   keeps the call off the GC safe-point path so nothing collects between the two
   time_ns reads. *)
external time_ns : unit -> int = "bench_time_ns" [@@noalloc]

let ecl = Config.ECL.L

let n_samples =
  match Sys.getenv_opt "BENCH_GC_SAMPLES" with
  | Some s -> ( try int_of_string s with _ -> 100_000 )
  | None -> 100_000

(* version -> a representative input string, same construction as bench_dist. *)
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

let width v = ((v - 1) * 4) + 21

(* ------------------------------------------------------------------ *)
(* 1. Threshold probe (REPORT section 3): minor/major word growth per   *)
(*    generate_qr call across v6-v9.  Qr.t holds two Bytes.make(width^2) *)
(*    buffers; once each exceeds Max_young_wosize (256 words / 2048 B on *)
(*    64-bit) it is allocated straight on the major heap, which shows up *)
(*    as major_words. The story: ~0 major words at v6/v7, a jump at v8.  *)
(* ------------------------------------------------------------------ *)
let threshold_report path =
  let oc = open_out path in
  let iters = 2000 in
  Printf.fprintf oc
    "# Minor/major-heap word growth per generate_qr call (heap path, %d iters/version).\n"
    iters;
  Printf.fprintf oc
    "# major_words>0 => a buffer spilled directly to the major heap.\n";
  Printf.fprintf oc
    "# Qr.t = two Bytes.make(width^2); Max_young_wosize=256 words (2048 bytes).\n";
  Printf.fprintf oc
    "version,width,buf_bytes,buf_words,major_words_per_call,minor_words_per_call,major_collections\n";
  for v = 6 to 9 do
    let data = version_inputs.(v) in
    let w = width v in
    let arena = Encoding.Arena.create None in
    for _ = 1 to 300 do
      let _qr = Encoding.generate_qr arena data ecl in ()
    done;
    Gc.full_major ();
    let s0 = Gc.stat () in
    for _ = 1 to iters do
      let _qr = Encoding.generate_qr arena data ecl in ()
    done;
    let s1 = Gc.stat () in
    let fi = float_of_int iters in
    let major = (s1.Gc.major_words -. s0.Gc.major_words) /. fi in
    let minor = (s1.Gc.minor_words -. s0.Gc.minor_words) /. fi in
    let mgc = s1.Gc.major_collections - s0.Gc.major_collections in
    let buf_bytes = w * w in
    let buf_words = (buf_bytes + 7) / 8 in
    Printf.fprintf oc "%d,%d,%d,%d,%.1f,%.1f,%d\n"
      v w buf_bytes buf_words major minor mgc
  done;
  close_out oc

(* ------------------------------------------------------------------ *)
(* 2. Per-sample GC attribution (REPORT section 3, strengthened):       *)
(*    for each timed generate_qr call, record whether a major collection *)
(*    completed during it, then show that the slow tail is dominated by  *)
(*    those calls.  If the top-0.1% slowest calls have a far higher major *)
(*    -GC rate than the population, the heap tail *is* GC, measured.      *)
(*                                                                        *)
(*    The Gc.quick_stat reads sit OUTSIDE the t0..t1 window, so they add  *)
(*    no latency to the sample; they do add minor-heap churn, which is    *)
(*    why we attribute on major_collections (robust to that) rather than  *)
(*    minor GC.                                                           *)
(*                                                                        *)
(*    major_collections only increments on whichever call happens to     *)
(*    finish the LAST slice of an in-progress cycle -- it marks a         *)
(*    boundary, not "how much GC work this call did". gc_rate (fraction  *)
(*    of ALL calls that complete a cycle) actually RISES with version     *)
(*    (v8 0.29% -> v40 3.7%: bigger buffers spill onto the major heap     *)
(*    every call, so slices -- and completions -- happen more often).     *)
(*    What breaks down at high version is the correlation between        *)
(*    "this call completed a cycle" and "this call was slow": at v8-v20   *)
(*    the major heap is tiny, so one call's allocation is often enough to *)
(*    finish most/all of a cycle's remaining work in a single slice --    *)
(*    slice-work and completion coincide, so the flagged call really did  *)
(*    pay for the whole cycle (p999_gc/p999_nogc ~17x at v8). At v40 the   *)
(*    heap is bigger, each call still only contributes one incremental    *)
(*    slice, and most of a cycle's cost is paid by earlier calls that did *)
(*    a partial slice without completing it (gc=false, untagged) -- the   *)
(*    call that finally tips the counter over only pays the last sliver,  *)
(*    so p999_gc/p999_nogc compresses to ~2x and tail_gc_rate stops being *)
(*    a reliable proxy for "this was the slow one", even though GC is     *)
(*    still going on and still costing something.                        *)
(* ------------------------------------------------------------------ *)
let pct sorted p =
  sorted.(int_of_float (float_of_int (Array.length sorted - 1) *. p))

let attribution_report path =
  let oc = open_out path in
  Printf.fprintf oc
    "# Per-sample GC attribution, heap path, %d samples/version.\n" n_samples;
  Printf.fprintf oc
    "# 'major-GC sample' = a major collection completed during the timed call.\n";
  Printf.fprintf oc
    "# If tail_gc_rate >> gc_rate, the latency tail is caused by major GC.\n";
  Printf.fprintf oc
    "# CAVEAT: the signal is 'a major cycle COMPLETED during the call', which\n";
  Printf.fprintf oc
    "# marks a boundary, not 'how much GC work this call did'. gc_rate actually\n";
  Printf.fprintf oc
    "# RISES with version (v8 0.29%% -> v40 3.7%%: bigger buffers spill onto the\n";
  Printf.fprintf oc
    "# major heap every call, so completions get MORE frequent, not rarer).\n";
  Printf.fprintf oc
    "# What breaks down at high version is the correlation, not the frequency:\n";
  Printf.fprintf oc
    "# at v8-v20 the heap is tiny, so one call's allocation often finishes most\n";
  Printf.fprintf oc
    "# of a cycle in one slice -- slice-work and completion coincide, so the\n";
  Printf.fprintf oc
    "# flagged call really paid for the whole cycle (p999_gc/p999_nogc ~17x at\n";
  Printf.fprintf oc
    "# v8). At v40 the heap is bigger, each call still only contributes one\n";
  Printf.fprintf oc
    "# incremental slice, and most of a cycle's cost is paid by earlier calls\n";
  Printf.fprintf oc
    "# that did a partial slice without completing it (untagged, gc=false) --\n";
  Printf.fprintf oc
    "# the call that tips the counter over only pays the last sliver, so the\n";
  Printf.fprintf oc
    "# ratio compresses to ~2x and tail_gc_rate stops proxying for 'the slow\n";
  Printf.fprintf oc
    "# one', even though GC is still happening and still costing something.\n";
  Printf.fprintf oc
    "# Latencies in ns.  p50/p999 split by whether the call saw a major GC.\n";
  Printf.fprintf oc
    "version,gc_rate,tail_gc_rate,p50_all,p999_all,p50_nogc,p999_nogc,p50_gc,p999_gc\n";
  let versions = [ 7; 8; 11; 20; 40 ] in
  List.iter
    (fun v ->
      let data = version_inputs.(v) in
      let arena = Encoding.Arena.create None in
      for _ = 1 to 500 do
        let _qr = Encoding.generate_qr arena data ecl in ()
      done;
      Gc.full_major ();
      (* pack (latency, saw_major_gc) so a sort by latency keeps the flag. *)
      let lat = Array.make n_samples 0 in
      let gc = Array.make n_samples false in
      for i = 0 to n_samples - 1 do
        let c0 = (Gc.quick_stat ()).Gc.major_collections in
        let t0 = time_ns () in
        let _qr = Encoding.generate_qr arena data ecl in ();
        let t1 = time_ns () in
        let c1 = (Gc.quick_stat ()).Gc.major_collections in
        lat.(i) <- t1 - t0;
        gc.(i) <- c1 > c0
      done;
      let n_gc = Array.fold_left (fun a b -> if b then a + 1 else a) 0 gc in
      let gc_rate = float_of_int n_gc /. float_of_int n_samples in
      (* overall percentiles *)
      let all = Array.copy lat in
      Array.sort compare all;
      (* percentiles of the two subsets *)
      let sub keep =
        let xs =
          Array.to_list
            (Array.of_seq
               (Seq.filter_map
                  (fun i -> if gc.(i) = keep then Some lat.(i) else None)
                  (Array.to_seq (Array.init n_samples (fun i -> i)))))
        in
        let a = Array.of_list xs in
        Array.sort compare a;
        a
      in
      let nogc = sub false in
      let withgc = sub true in
      (* tail GC rate: among the slowest 0.1% by latency, how many saw GC. *)
      let idx = Array.init n_samples (fun i -> i) in
      Array.sort (fun i j -> compare lat.(i) lat.(j)) idx;
      let tail_n = max 1 (n_samples / 1000) in
      let tail_gc = ref 0 in
      for k = n_samples - tail_n to n_samples - 1 do
        if gc.(idx.(k)) then incr tail_gc
      done;
      let tail_gc_rate = float_of_int !tail_gc /. float_of_int tail_n in
      let p50p a = if Array.length a = 0 then -1 else pct a 0.50 in
      let p999p a = if Array.length a = 0 then -1 else pct a 0.999 in
      Printf.fprintf oc "%d,%.4f,%.4f,%d,%d,%d,%d,%d,%d\n"
        v gc_rate tail_gc_rate
        (pct all 0.50) (pct all 0.999)
        (p50p nogc) (p999p nogc)
        (p50p withgc) (p999p withgc);
      Printf.printf
        "v%-2d  gc_rate=%.4f  tail_gc_rate=%.4f  p999_all=%d  p999_nogc=%d  p999_gc=%d\n%!"
        v gc_rate tail_gc_rate (pct all 0.999)
        (p999p nogc) (p999p withgc))
    versions;
  close_out oc

let () =
  let dir =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else Filename.concat (Filename.dirname Sys.executable_name) "."
  in
  let threshold = Filename.concat dir "threshold.txt" in
  let attribution = Filename.concat dir "gc_attribution.txt" in
  Printf.printf "== threshold probe (v6-v9) -> %s ==\n%!" threshold;
  threshold_report threshold;
  Printf.printf "== per-sample GC attribution -> %s ==\n%!" attribution;
  attribution_report attribution;
  Printf.printf "done.\n%!"
