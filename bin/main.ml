open Oxqr

let () =
  let _config = Config.make ~version:2 ~ecl:Config.ECL.M in
  (* Example usage of a fixed-size bit buffer. *)
  let b = Bitbuf.create 8 in
  (* 8 bytes = 64 bits *)
  Bitbuf.write_bits_msb b ~value:0b1011 ~width:4;
  Bitbuf.write_bit b true;
  Bitbuf.write_bits_msb b ~value:0x2A ~width:6;
  let _bytes = Bitbuf.to_bytes b in
  ()
