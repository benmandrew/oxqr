open Oxqr
open Base
open Core_bench

let random_alphanumeric_string len =
  let chars =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:" |> String.to_list
  in
  let chars_len = List.length chars in
  let rec build_string acc n =
    if n <= 0 then acc
    else
      let idx = Random.int chars_len in
      let c = List.nth_exn chars idx in
      build_string (acc ^ String.of_char c) (n - 1)
  in
  build_string "" len

let alloc_count = ref 0

let runtime_begin _device _ts phase =
  Stdlib.Printf.printf "Begin event: %s\n%!"
    (Runtime_events.runtime_phase_name phase)

let runtime_end _device _ts phase =
  Stdlib.Printf.printf "End event: %s\n%!"
    (Runtime_events.runtime_phase_name phase)

let runtime_counter _device _ts counter n =
  Stdlib.Printf.printf "Counter event: %s with n=%d\n%!"
    (Runtime_events.runtime_counter_name counter)
    n;
  match counter with
  | Runtime_events.EV_C_MINOR_ALLOCATED -> Int.incr alloc_count
  | _ -> ()

let stack test_data str_len =
  let arena = Encoding.Arena.create (Some str_len) in
  let i = ref 0 in
  while !i < 1000 do
    Encoding.generate_qr_stack arena test_data Config.ECL.M;
    Int.incr i
  done

let heap test_data =
  let arena = Encoding.Arena.create None in
  let i = ref 0 in
  while !i < 1000 do
    let _ = Encoding.generate_qr arena test_data Config.ECL.M in
    Int.incr i
  done

let runtime_events test_data str_len =
  let arena = Encoding.Arena.create (Some str_len) in
  Runtime_events.start ();
  let cursor = Runtime_events.create_cursor None in
  let callbacks =
    Runtime_events.Callbacks.create ~runtime_begin ~runtime_end ~runtime_counter
      ()
  in
  let i = ref 0 in
  while !i < 10000 do
    Encoding.generate_qr_stack arena test_data Config.ECL.M;
    Int.incr i
  done;
  let n_events = Runtime_events.read_poll cursor callbacks None in
  Runtime_events.free_cursor cursor;
  Stdlib.Printf.printf "Number of runtime events collected: %d\n" n_events;
  Stdlib.Printf.printf "Total allocations during QR generation: %d\n"
    !alloc_count

let () =
  let str_len = 100 in
  let test_data = random_alphanumeric_string str_len in
  let tests =
    [
      Bench.Test.create ~name:"stack" (fun () -> stack test_data str_len);
      Bench.Test.create ~name:"immutable" (fun () -> heap test_data);
    ]
  in
  Bench.make_command tests |> Command_unix.run
