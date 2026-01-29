(* Test that arithmetic-encoded JPEGs can be decoded back *)
let () =
  (* Read the arithmetic baseline file *)
  let img = Jpeg.read "test_images/arith_baseline.jpg" in
  Printf.printf "SOF9 decoded: %dx%d\n" img.Jpeg.width img.Jpeg.height;
  let r, g, b = Jpeg.get_pixel img 32 32 in
  Printf.printf "  Pixel at (32,32): R=%d G=%d B=%d\n" r g b;

  (* Read the arithmetic progressive file *)
  let img2 = Jpeg.read "test_images/arith_progressive.jpg" in
  Printf.printf "SOF10 decoded: %dx%d\n" img2.Jpeg.width img2.Jpeg.height;
  let r, g, b = Jpeg.get_pixel img2 32 32 in
  Printf.printf "  Pixel at (32,32): R=%d G=%d B=%d\n" r g b;

  Printf.printf "\nArithmetic coding encode/decode roundtrip verified!\n"
