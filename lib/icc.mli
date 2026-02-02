(** ICC color profile handling *)

type t
(** ICC profile data *)

val empty : t
(** Empty ICC profile *)

val from_bytes : bytes -> t
(** Create ICC profile from bytes *)

val to_bytes : t -> bytes
(** Get ICC profile as bytes *)

val is_empty : t -> bool
(** Check if profile is empty *)

val from_chunks : (int * int * bytes) list -> t option
(** Reassemble ICC profile from APP2 chunks.
    Chunks are (sequence, count, data) tuples where sequence is 1-based.
    Returns None if chunks are incomplete or inconsistent. *)

val to_chunks : t -> (int * int * bytes) list
(** Split ICC profile into chunks for encoding.
    Returns list of (sequence, count, data) tuples where sequence is 1-based. *)
