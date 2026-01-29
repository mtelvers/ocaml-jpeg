(** Simple JPEG viewer using OCaml Graphics library

    Usage: jpeg_viewer <file.jpg> [file2.jpg ...]

    Controls:
    - Space/Enter/Right: Next image
    - Left/Backspace: Previous image
    - Q/Escape: Quit
    - R: Reload current image
    - +/-: Zoom in/out (TODO) *)

let display_image image =
  let width = image.Jpeg.width in
  let height = image.Jpeg.height in

  (* Create color array for Graphics.make_image *)
  let colors =
    Array.init height (fun y ->
        Array.init width (fun x ->
            let r, g, b = Jpeg.get_pixel image x y in
            Graphics.rgb r g b))
  in

  (* Graphics uses bottom-left origin, so flip vertically *)
  let flipped = Array.init height (fun y -> colors.(height - 1 - y)) in

  let gimage = Graphics.make_image flipped in

  (* Center the image in the window *)
  let win_w = Graphics.size_x () in
  let win_h = Graphics.size_y () in
  let x = (win_w - width) / 2 in
  let y = (win_h - height) / 2 in

  Graphics.clear_graph ();
  Graphics.draw_image gimage x y

let show_info filename image =
  let info =
    Printf.sprintf "%s - %dx%d"
      (Filename.basename filename)
      image.Jpeg.width image.Jpeg.height
  in
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 0 (Graphics.size_x ()) 20;
  Graphics.set_color Graphics.white;
  Graphics.moveto 5 5;
  Graphics.draw_string info

let load_and_display filename =
  Printf.printf "Loading: %s\n%!" filename;
  try
    let image = Jpeg.read filename in
    Printf.printf "  Size: %dx%d\n%!" image.Jpeg.width image.Jpeg.height;
    display_image image;
    show_info filename image;
    Some image
  with e ->
    Printf.eprintf "Error loading %s: %s\n%!" filename (Printexc.to_string e);
    Graphics.clear_graph ();
    Graphics.set_color Graphics.red;
    Graphics.moveto 10 (Graphics.size_y () / 2);
    Graphics.draw_string (Printf.sprintf "Error: %s" (Printexc.to_string e));
    None

let rec event_loop files current_idx =
  let num_files = Array.length files in
  if num_files = 0 then ()
  else begin
    let ev = Graphics.wait_next_event [ Graphics.Key_pressed ] in
    if ev.Graphics.keypressed then begin
      let next_idx =
        match ev.Graphics.key with
        | 'q' | '\027' ->
            (* Q or Escape *)
            -1
            (* Signal quit *)
        | ' ' | '\r' | '\n' ->
            (* Space, Enter *)
            (current_idx + 1) mod num_files
        | 'p' | '\b' ->
            (* P or Backspace for previous *)
            (current_idx - 1 + num_files) mod num_files
        | 'r' ->
            (* Reload *)
            current_idx
        | _ -> current_idx (* No change *)
      in
      if next_idx < 0 then () (* Quit *)
      else if next_idx <> current_idx || ev.Graphics.key = 'r' then begin
        ignore (load_and_display files.(next_idx));
        event_loop files next_idx
      end
      else event_loop files current_idx
    end
    else event_loop files current_idx
  end

let () =
  let files = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in

  if Array.length files = 0 then begin
    Printf.printf "Usage: %s <file.jpg> [file2.jpg ...]\n" Sys.argv.(0);
    Printf.printf "\nControls:\n";
    Printf.printf "  Space/Enter: Next image\n";
    Printf.printf "  P/Backspace: Previous image\n";
    Printf.printf "  R: Reload current image\n";
    Printf.printf "  Q/Escape: Quit\n";
    exit 1
  end;

  (* Determine initial window size from first image *)
  let first_image = try Some (Jpeg.read files.(0)) with _ -> None in

  let init_w, init_h =
    match first_image with
    | Some img ->
        (* Add some padding, max 800x600 for initial size *)
        (min 800 (img.Jpeg.width + 40), min 600 (img.Jpeg.height + 60))
    | None -> (640, 480)
  in

  (* Try to open graphics window *)
  begin try
    Graphics.open_graph (Printf.sprintf " %dx%d" init_w init_h);
    Graphics.set_window_title "JPEG Viewer";
    Graphics.auto_synchronize true
  with e ->
    Printf.eprintf "Cannot open graphics window: %s\n" (Printexc.to_string e);
    Printf.eprintf "Make sure you have a display available (X11 on Linux).\n";
    exit 1
  end;

  (* Display first image *)
  ignore (load_and_display files.(0));

  (* Event loop *)
  begin try event_loop files 0
  with Graphics.Graphic_failure _ -> () (* Window was closed *)
  end;

  Graphics.close_graph ()
