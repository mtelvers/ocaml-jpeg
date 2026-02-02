(** Pure OCaml JPEG library for reading and writing JPEG/JFIF files.

    Supported features:
    - Baseline sequential DCT JPEG (SOF0)
    - Progressive DCT JPEG (SOF2)
    - Lossless JPEG (SOF3, SOF11) - pixel-perfect encoding/decoding
    - Arithmetic coding JPEG (SOF9, SOF10) - full encode/decode support
    - 8-bit and 12-bit precision
    - Grayscale, YCbCr, CMYK, and YCCK color (1, 3, or 4 components)
    - Standard Huffman coding and arithmetic coding
    - All common sampling factors (4:4:4, 4:2:2, 4:2:0)
    - EXIF metadata parsing and preservation
    - Restart markers *)

(** {1 Internal Modules}
    These modules are exposed for advanced use cases. *)

module Bitstream = Bitstream
module Markers = Markers
module Huffman = Huffman
module Dct = Dct
module Quantization = Quantization
module Color = Color
module Exif = Exif
module Arithmetic = Arithmetic
module Icc = Icc
module Predictor = Predictor

(** {1 Types} *)

(** Pixel format for image data *)
type pixel_format =
  | RGB24  (** 3 bytes per pixel: R, G, B *)
  | CMYK32  (** 4 bytes per pixel: C, M, Y, K *)

type pixel_data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Pixel data stored as RGB24 or CMYK32 format in a bigarray. *)

type image = {
  width : int;
  height : int;
  pixels : pixel_data;
  pixel_format : pixel_format;
  exif : Exif.t option;
  icc_profile : Icc.t option;
}
(** JPEG image with pixel data and optional EXIF metadata and ICC profile. *)

(** {1 Encoding Options} *)

type subsampling =
  | Sub_444
  | Sub_422
  | Sub_420
      (** Chroma subsampling modes:
          - [Sub_444]: No subsampling (highest quality, largest files)
          - [Sub_422]: Horizontal subsampling only (2:1 horizontal ratio)
          - [Sub_420]: Both horizontal and vertical subsampling (2:1 both
            directions) *)

type color_mode =
  | Color
  | Grayscale
  | CMYK
  | YCCK
      (** Color modes:
          - [Color]: Full YCbCr color encoding (3 components)
          - [Grayscale]: Single luminance component only
          - [CMYK]: 4 components, no conversion (inverted CMYK as used in JPEG)
          - [YCCK]: CMYK via YCbCr + K, 4 components *)

type encoding_mode =
  | Baseline
  | Progressive
  | Lossless
      (** Encoding modes:
          - [Baseline]: Standard sequential DCT encoding (SOF0)
          - [Progressive]: Progressive DCT encoding (SOF2) for incremental
            display
          - [Lossless]: Lossless JPEG encoding (SOF3/SOF11) for pixel-perfect
            compression *)

type precision =
  | Precision_8
  | Precision_12
      (** Sample precision:
          - [Precision_8]: 8-bit samples (0-255, standard)
          - [Precision_12]: 12-bit samples (0-4095, extended range) *)

type entropy_coding =
  | Huffman
  | Arithmetic
      (** Entropy coding method:
          - [Huffman]: Standard Huffman coding (most compatible)
          - [Arithmetic]: Arithmetic coding (smaller files, less compatible) *)

type encode_options = {
  quality : int;  (** Compression quality from 1 (worst) to 100 (best) *)
  subsampling : subsampling;  (** Chroma subsampling mode *)
  color_mode : color_mode;  (** Color or grayscale *)
  encoding_mode : encoding_mode;  (** Baseline, progressive, or lossless *)
  restart_interval : int;  (** MCUs between RST markers, 0 = disabled *)
  precision : precision;  (** Sample precision: 8-bit or 12-bit *)
  entropy_coding : entropy_coding;  (** Huffman or arithmetic coding *)
  predictor : int;  (** Predictor selection for lossless mode (1-7), 0 = auto *)
  point_transform : int;  (** Point transform for lossless mode (0 = none) *)
}
(** Encoding options for JPEG output. *)

val default_encode_options : encode_options
(** Default encoding options: quality=75, Sub_420, Color, Baseline,
    restart_interval=0, Precision_8, Huffman *)

(** {1 Reading JPEG} *)

val read : string -> image
(** [read filename] reads a JPEG image from the given file path.
    @raise Failure if the file cannot be read or is not a valid baseline JPEG.
*)

val read_bytes : bytes -> image
(** [read_bytes data] decodes a JPEG image from bytes in memory.
    @raise Failure if the data is not a valid baseline JPEG. *)

(** {1 Writing JPEG} *)

val write : ?quality:int -> string -> image -> unit
(** [write filename image ~quality] writes an image to the given file path as
    JPEG using default options (baseline, 4:2:0, color).
    @param quality
      Compression quality from 1 (worst) to 100 (best). Default is 75.
    @raise Failure if the file cannot be written. *)

val write_bytes : ?quality:int -> image -> bytes
(** [write_bytes image ~quality] encodes an image to JPEG bytes in memory using
    default options (baseline, 4:2:0, color).
    @param quality
      Compression quality from 1 (worst) to 100 (best). Default is 75. *)

val write_with_options : encode_options -> string -> image -> unit
(** [write_with_options options filename image] writes an image to the given
    file path with the specified encoding options.
    @raise Failure if the file cannot be written. *)

val write_bytes_with_options : encode_options -> image -> bytes
(** [write_bytes_with_options options image] encodes an image to JPEG bytes with
    the specified encoding options. Supports:
    - Baseline and progressive encoding modes
    - 4:4:4, 4:2:2, and 4:2:0 chroma subsampling
    - Color and grayscale output *)

(** {1 Image Creation} *)

val create_image : int -> int -> pixel_data -> image
(** [create_image width height pixels] creates a new image from raw RGB24 pixel
    data. The [pixels] bigarray must have length [width * height * 3]. *)

val create_image_with_exif : int -> int -> pixel_data -> Exif.t -> image
(** [create_image_with_exif width height pixels exif] creates a new image with
    EXIF metadata. *)

val create_image_with_icc : int -> int -> pixel_data -> Icc.t -> image
(** [create_image_with_icc width height pixels icc] creates a new image with
    ICC color profile. *)

val create_image_with_metadata : int -> int -> pixel_data -> Exif.t -> Icc.t -> image
(** [create_image_with_metadata width height pixels exif icc] creates a new image
    with both EXIF metadata and ICC color profile. *)

val create_cmyk_image : int -> int -> pixel_data -> image
(** [create_cmyk_image width height pixels] creates a new CMYK image from raw
    CMYK32 pixel data. The [pixels] bigarray must have length
    [width * height * 4]. *)

(** {1 Pixel Access} *)

val get_pixel : image -> int -> int -> int * int * int
(** [get_pixel image x y] returns the (r, g, b) values at position (x, y). For
    CMYK images, converts to RGB on the fly. *)

val get_cmyk_pixel : image -> int -> int -> int * int * int * int
(** [get_cmyk_pixel image x y] returns the (c, m, y, k) values at position (x,
    y). Only valid for CMYK32 images. *)

val set_pixel : image -> int -> int -> int -> int -> int -> unit
(** [set_pixel image x y r g b] sets the pixel at position (x, y) to (r, g, b).
    Only valid for RGB24 images. *)

val set_cmyk_pixel : image -> int -> int -> int -> int -> int -> int -> unit
(** [set_cmyk_pixel image x y c m yy k] sets the CMYK pixel at position (x, y).
    Only valid for CMYK32 images. *)
