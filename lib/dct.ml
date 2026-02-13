(** DCT and IDCT transforms for JPEG *)

let pi = 4.0 *. atan 1.0

(** Cosine lookup table for 8x8 DCT *)
let cos_table =
  Array.init 8 (fun u ->
      Array.init 8 (fun x ->
          cos (Float.of_int ((2 * x) + 1) *. Float.of_int u *. pi /. 16.0)))

(** Scaling factors for DCT *)
let c = function 0 -> 1.0 /. sqrt 2.0 | _ -> 1.0

(** 1D IDCT on 8 values *)
let idct_1d input =
  Array.init 8 (fun x ->
    let sum = ref 0.0 in
    for u = 0 to 7 do
      sum := !sum +. (c u *. input.(u) *. cos_table.(u).(x))
    done;
    !sum *. 0.5)

(** 1D FDCT on 8 values *)
let fdct_1d input =
  Array.init 8 (fun u ->
    let sum = ref 0.0 in
    for x = 0 to 7 do
      sum := !sum +. (input.(x) *. cos_table.(u).(x))
    done;
    c u *. !sum *. 0.5)

(** 2D IDCT on 8x8 block (input in row-major, 64 elements) *)
let idct block =
  let result = Array.make 64 0.0 in

  (* First pass: IDCT on rows *)
  let temp = Array.make 64 0.0 in
  for row = 0 to 7 do
    let row_data = Array.init 8 (fun col -> block.((row * 8) + col)) in
    let row_result = idct_1d row_data in
    for col = 0 to 7 do
      temp.((row * 8) + col) <- row_result.(col)
    done
  done;

  (* Second pass: IDCT on columns *)
  for col = 0 to 7 do
    let col_data = Array.init 8 (fun row -> temp.((row * 8) + col)) in
    let col_result = idct_1d col_data in
    for row = 0 to 7 do
      result.((row * 8) + col) <- col_result.(row)
    done
  done;

  result

(** 2D FDCT on 8x8 block (input in row-major, 64 elements) *)
let fdct block =
  let result = Array.make 64 0.0 in

  (* First pass: FDCT on rows *)
  let temp = Array.make 64 0.0 in
  for row = 0 to 7 do
    let row_data = Array.init 8 (fun col -> block.((row * 8) + col)) in
    let row_result = fdct_1d row_data in
    for col = 0 to 7 do
      temp.((row * 8) + col) <- row_result.(col)
    done
  done;

  (* Second pass: FDCT on columns *)
  for col = 0 to 7 do
    let col_data = Array.init 8 (fun row -> temp.((row * 8) + col)) in
    let col_result = fdct_1d col_data in
    for row = 0 to 7 do
      result.((row * 8) + col) <- col_result.(row)
    done
  done;

  result

(** AA&N (Arai, Agui, Nakajima) algorithm constants *)
let aan_scale_factors =
  let s = Array.make 8 0.0 in
  s.(0) <- 1.0;
  for k = 1 to 7 do
    s.(k) <- 1.0 /. (2.0 *. cos (Float.of_int k *. pi /. 16.0))
  done;
  (* Combined 2D scale factors *)
  Array.init 64 (fun i ->
      let row = i / 8 in
      let col = i mod 8 in
      1.0 /. (s.(row) *. s.(col) *. 8.0))

(** Core 1D AA&N IDCT transform - shared by row and column passes *)
let aan_1d_transform get set =
  let v0 = get 0 and v1 = get 1 and v2 = get 2 and v3 = get 3 in
  let v4 = get 4 and v5 = get 5 and v6 = get 6 and v7 = get 7 in

  (* Even part *)
  let t0 = v0 +. v4 in
  let t1 = v0 -. v4 in
  let t2 = (v2 *. 1.414213562) -. v6 in
  let t3 = v2 +. (v6 *. 1.414213562) in

  let t4 = t0 +. t3 in
  let t5 = t0 -. t3 in
  let t6 = t1 +. t2 in
  let t7 = t1 -. t2 in

  (* Odd part *)
  let z1 = v1 +. v7 in
  let z2 = v3 +. v5 in
  let z3 = v1 +. v3 in
  let z4 = v5 +. v7 in
  let z5 = (z3 +. z4) *. 1.175875602 in

  let t10 = v1 *. 1.501321110 in
  let t11 = v3 *. 3.072711026 in
  let t12 = v5 *. 0.298631336 in
  let t13 = v7 *. 0.899976223 in

  let z1' = z1 *. -0.899976223 in
  let z2' = z2 *. -2.562915447 in
  let z3' = (z3 *. -1.961570560) +. z5 in
  let z4' = (z4 *. -0.390180644) +. z5 in

  let r4 = t10 +. z1' +. z3' in
  let r5 = t11 +. z2' +. z4' in
  let r6 = t12 +. z2' +. z3' in
  let r7 = t13 +. z1' +. z4' in

  set 0 (t4 +. r4);
  set 7 (t4 -. r4);
  set 1 (t6 +. r5);
  set 6 (t6 -. r5);
  set 2 (t7 +. r6);
  set 5 (t7 -. r6);
  set 3 (t5 +. r7);
  set 4 (t5 -. r7)

(** Fast IDCT using AA&N algorithm (optimized version) *)
let idct_aan block =
  (* Scale input *)
  let scaled = Array.mapi (fun i v -> v *. aan_scale_factors.(i)) block in

  (* Apply separable 1D transforms *)
  let temp = Array.make 64 0.0 in

  (* Row transforms *)
  for row = 0 to 7 do
    let offset = row * 8 in
    aan_1d_transform
      (fun i -> scaled.(offset + i))
      (fun i v -> temp.(offset + i) <- v)
  done;

  (* Column transforms *)
  let result = Array.make 64 0.0 in
  for col = 0 to 7 do
    aan_1d_transform
      (fun i -> temp.((i * 8) + col))
      (fun i v -> result.((i * 8) + col) <- v)
  done;

  result
