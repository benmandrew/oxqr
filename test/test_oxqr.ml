let test_version_2_pattern_modules () =
  let qr = Oxqr.Qr.make ~version:2 in
  Oxqr.Qr.place_pattern_modules qr 2;
  let output = String.trim @@ Oxqr.Qr.to_unicode_string qr in
  let expected =
    String.trim
      {|
████████████████                    ██████████████
██          ████                    ██          ██
██  ██████  ████                    ██  ██████  ██
██  ██████  ████                    ██  ██████  ██
██  ██████  ████                    ██  ██████  ██
██          ████                    ██          ██
██████████████████  ██  ██  ██  ██  ██████████████
████████████████                  ████████████████
            ██

            ██

            ██

            ██

            ██                  ██████████
              ████              ██      ██
████████████████                ██  ██  ██
██          ████                ██      ██
██  ██████  ████                ██████████
██  ██████  ████
██  ██████  ████
██          ████
████████████████|}
  in
  if String.equal output expected then
    print_endline "✓ version_2_pattern_modules: PASS"
  else (
    print_endline "✗ version_2_pattern_modules: FAIL";
    print_endline "\n--- Expected ---";
    print_endline expected;
    print_endline "\n--- Actual ---";
    print_endline output;
    print_endline "\n--- Line-by-line diff ---";
    let exp_lines = String.split_on_char '\n' expected in
    let act_lines = String.split_on_char '\n' output in
    let max_lines = max (List.length exp_lines) (List.length act_lines) in
    for i = 0 to max_lines - 1 do
      let exp =
        if i < List.length exp_lines then List.nth exp_lines i else "<missing>"
      in
      let act =
        if i < List.length act_lines then List.nth act_lines i else "<missing>"
      in
      if not (String.equal exp act) then
        Printf.printf "Line %d differs:\n  Expected: '%s'\n  Actual: '%s'\n" i
          exp act
    done;
    failwith "Test failed")

let () = test_version_2_pattern_modules ()
