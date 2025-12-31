open Base

let qr_to_xml (t : Qr.t) =
  let buf = Buffer.create 1024 in
  let width = t.width + 8 in
  Buffer.add_string buf
    (Printf.sprintf
       "<svg version=\"1.1\" width=\"%d\" height=\"%d\" \
        xmlns=\"http://www.w3.org/2000/svg\">\n\
        <rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"white\" />\n"
       width width width width);
  for y = 0 to t.width - 1 do
    for x = 0 to t.width - 1 do
      let cell = Bytes.get t.buf ((y * t.width) + x) in
      if not (Char.equal__local cell '\000') then
        Buffer.add_string buf
          (Printf.sprintf
             "<rect x=\"%d\" y=\"%d\" width=\"1\" height=\"1\" fill=\"black\" />\n"
             (x + 4) (y + 4))
    done
  done;
  Buffer.add_string buf "</svg>\n";
  Stdlib.Printf.printf "Buffer length: %d\n" (Buffer.length buf);
  Buffer.contents buf
