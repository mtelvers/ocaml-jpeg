(** ICC color profile handling *)

type t = { profile_data : bytes }
(** ICC profile data *)

(** Empty ICC profile *)
let empty = { profile_data = Bytes.empty }

(** Create ICC profile from bytes *)
let from_bytes data = { profile_data = data }

(** Get ICC profile as bytes *)
let to_bytes icc = icc.profile_data

(** Check if profile is empty *)
let is_empty icc = Bytes.length icc.profile_data = 0

(** Maximum payload size per APP2 chunk (65535 - 2 length - 12 signature - 2 seq/count) *)
let max_chunk_size = 65519

(** Reassemble ICC profile from APP2 chunks.
    Chunks are (sequence, count, data) tuples where sequence is 1-based.
    Returns None if chunks are incomplete or inconsistent. *)
let from_chunks chunks =
  match chunks with
  | [] -> None
  | _ ->
      (* Sort by sequence number *)
      let sorted =
        List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) chunks
      in
      (* Verify we have all chunks and they agree on count *)
      let _, expected_count, _ = List.hd sorted in
      let num_chunks = List.length sorted in
      if num_chunks <> expected_count then None
      else
        (* Verify sequence numbers are 1..count *)
        let valid_sequences =
          List.mapi
            (fun i (seq, count, _) -> seq = i + 1 && count = expected_count)
            sorted
          |> List.for_all Fun.id
        in
        if not valid_sequences then None
        else
          (* Calculate total size and concatenate *)
          let total_size =
            List.fold_left (fun acc (_, _, data) -> acc + Bytes.length data) 0 sorted
          in
          let result = Bytes.create total_size in
          let _ =
            List.fold_left
              (fun offset (_, _, data) ->
                let len = Bytes.length data in
                Bytes.blit data 0 result offset len;
                offset + len)
              0 sorted
          in
          Some { profile_data = result }

(** Split ICC profile into chunks for encoding.
    Returns list of (sequence, count, data) tuples where sequence is 1-based. *)
let to_chunks icc =
  let data = icc.profile_data in
  let total_len = Bytes.length data in
  if total_len = 0 then []
  else
    let num_chunks = (total_len + max_chunk_size - 1) / max_chunk_size in
    List.init num_chunks (fun i ->
        let offset = i * max_chunk_size in
        let len = min max_chunk_size (total_len - offset) in
        let chunk_data = Bytes.sub data offset len in
        (i + 1, num_chunks, chunk_data))
