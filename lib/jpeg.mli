(** Pure OCaml JPEG library for reading and writing JPEG/JFIF files.

    Supported features:
    - Baseline sequential DCT JPEG (SOF0)
    - Progressive DCT JPEG (SOF2)
    - 8-bit precision
    - Grayscale and YCbCr color (1 or 3 components)
    - Standard Huffman coding
    - All common sampling factors (4:4:4, 4:2:2, 4:2:0)
    - EXIF metadata parsing and preservation *)

(** {1 Internal Modules}
    These modules are exposed for advanced use cases. *)

module Bitstream = Bitstream
module Markers = Markers
module Huffman = Huffman
module Dct = Dct
module Quantization = Quantization
module Color = Color
module Exif = Exif

(** {1 Types} *)

type pixel_data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Pixel data stored as RGB24 format in a bigarray. The data is laid out as
    [R,G,B,R,G,B,...] with one byte per channel. *)

type image = {
  width : int;
  height : int;
  pixels : pixel_data;  (** RGB24 format: R,G,B,R,G,B,... *)
  exif : Exif.t option;
}
(** JPEG image with pixel data and optional EXIF metadata. *)

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
      (** Color modes:
          - [Color]: Full YCbCr color encoding (3 components)
          - [Grayscale]: Single luminance component only *)

type encoding_mode =
  | Baseline
  | Progressive
      (** Encoding modes:
          - [Baseline]: Standard sequential DCT encoding (SOF0)
          - [Progressive]: Progressive DCT encoding (SOF2) for incremental
            display *)

type precision =
  | Precision_8
  | Precision_12
      (** Sample precision:
          - [Precision_8]: 8-bit samples (0-255, standard)
          - [Precision_12]: 12-bit samples (0-4095, extended range) *)

type encode_options = {
  quality : int;  (** Compression quality from 1 (worst) to 100 (best) *)
  subsampling : subsampling;  (** Chroma subsampling mode *)
  color_mode : color_mode;  (** Color or grayscale *)
  encoding_mode : encoding_mode;  (** Baseline or progressive *)
  restart_interval : int;  (** MCUs between RST markers, 0 = disabled *)
  precision : precision;  (** Sample precision: 8-bit or 12-bit *)
}
(** Encoding options for JPEG output. *)

val default_encode_options : encode_options
(** Default encoding options: quality=75, Sub_420, Color, Baseline,
    restart_interval=0, Precision_8 *)

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

(** {1 Pixel Access} *)

val get_pixel : image -> int -> int -> int * int * int
(** [get_pixel image x y] returns the (r, g, b) values at position (x, y). *)

val set_pixel : image -> int -> int -> int -> int -> int -> unit
(** [set_pixel image x y r g b] sets the pixel at position (x, y) to (r, g, b).
*)
