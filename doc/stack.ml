let f ((x, y) @ local) = exclave_ (x + 1, y + 1)

let () =
  let _ = f (3, 4) in
  ()
