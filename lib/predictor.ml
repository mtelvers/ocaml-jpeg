(** Lossless JPEG predictors (ITU-T T.81 Table H.1) *)

(** Predictor selection values per ITU-T T.81 Table H.1 *)
type predictor =
  | No_prediction  (** Selection 0: Px = 0 *)
  | Ra             (** Selection 1: Px = Ra (left pixel) *)
  | Rb             (** Selection 2: Px = Rb (above pixel) *)
  | Rc             (** Selection 3: Px = Rc (above-left pixel) *)
  | Ra_Rb_Rc       (** Selection 4: Px = Ra + Rb - Rc *)
  | Ra_half_diff   (** Selection 5: Px = Ra + (Rb - Rc) / 2 *)
  | Rb_half_diff   (** Selection 6: Px = Rb + (Ra - Rc) / 2 *)
  | Average        (** Selection 7: Px = (Ra + Rb) / 2 *)

(** Convert predictor selection value (0-7) to predictor type *)
let predictor_of_int = function
  | 0 -> No_prediction
  | 1 -> Ra
  | 2 -> Rb
  | 3 -> Rc
  | 4 -> Ra_Rb_Rc
  | 5 -> Ra_half_diff
  | 6 -> Rb_half_diff
  | 7 -> Average
  | n -> failwith (Printf.sprintf "Invalid predictor selection value: %d" n)

(** Convert predictor type to selection value *)
let int_of_predictor = function
  | No_prediction -> 0
  | Ra -> 1
  | Rb -> 2
  | Rc -> 3
  | Ra_Rb_Rc -> 4
  | Ra_half_diff -> 5
  | Rb_half_diff -> 6
  | Average -> 7

(** Calculate the prediction value.

    @param predictor The predictor to use
    @param ra The left pixel value (sample at x-1, y)
    @param rb The above pixel value (sample at x, y-1)
    @param rc The above-left pixel value (sample at x-1, y-1)
    @param x The current pixel x coordinate
    @param y The current pixel y coordinate
    @param precision The sample precision (P) in bits
    @param point_transform The point transform value (Pt)
    @return The predicted sample value

    Boundary handling per ITU-T T.81 Section H.1.1:
    - First pixel (0,0): Use default value 2^(P-Pt-1)
    - First row (y=0, x>0): Use Ra (left pixel) regardless of predictor
    - First column (x=0, y>0): Use Rb (above pixel) for all predictors *)
let predict predictor ~ra ~rb ~rc ~x ~y ~precision ~point_transform =
  (* Default value for first pixel: 2^(P-Pt-1) where P = precision, Pt = point_transform *)
  let default_value = 1 lsl (precision - point_transform - 1) in

  if x = 0 && y = 0 then
    (* First pixel: use default value *)
    default_value
  else if y = 0 then
    (* First row (x>0): use Ra (left pixel) regardless of predictor selection *)
    ra
  else if x = 0 then
    (* First column (y>0): use Rb (above pixel) for all predictors *)
    rb
  else
    (* Normal case: apply the selected predictor *)
    match predictor with
    | No_prediction -> 0
    | Ra -> ra
    | Rb -> rb
    | Rc -> rc
    | Ra_Rb_Rc -> ra + rb - rc
    | Ra_half_diff -> ra + ((rb - rc) / 2)
    | Rb_half_diff -> rb + ((ra - rc) / 2)
    | Average -> (ra + rb) / 2

(** Apply modular reduction to keep value in valid range.
    Lossless JPEG uses modulo 2^precision arithmetic.

    @param precision The sample precision in bits
    @param value The value to reduce
    @return The value reduced to [0, 2^precision - 1] *)
let modular_reduction ~precision value =
  let max_val = 1 lsl precision in
  let v = value mod max_val in
  if v < 0 then v + max_val else v

(** Reconstruct original sample from prediction and difference.

    @param predicted The predicted value
    @param diff The decoded difference value
    @param precision The sample precision in bits
    @param point_transform The point transform value
    @return The reconstructed sample value *)
let reconstruct ~predicted ~diff ~precision ~point_transform =
  (* Apply point transform to difference and add to prediction *)
  let diff_scaled = diff lsl point_transform in
  modular_reduction ~precision (predicted + diff_scaled)

(** Calculate the difference value for encoding.

    @param sample The actual sample value
    @param predicted The predicted value
    @param precision The sample precision in bits
    @param point_transform The point transform value (for future use)
    @return The difference value to encode *)
let compute_diff ~sample ~predicted ~precision ~point_transform:_ =
  (* The difference is computed before point transform is applied *)
  let max_val = 1 lsl precision in
  let half = max_val / 2 in
  let diff = sample - predicted in
  (* Map difference to the range [-half, half-1] for optimal encoding *)
  if diff > half - 1 then diff - max_val
  else if diff < -half then diff + max_val
  else diff
