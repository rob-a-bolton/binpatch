let buffer_size = 1024 * 8

let range_validator file_size =
  fun (off, len) -> off + len <= file_size

let range_valid file_size (off, len) =
  (range_validator file_size) (off, len)

let can_patch file_size ranges =
  let valid_ranges =
    List.filter (range_validator file_size) ranges
  in
  List.length valid_ranges <> List.length ranges

let create_patch src dst ranges =
  if not (Sys.file_exists src) then
    Error "Src file does not exist"
  else if (Sys.is_directory src) then
    Error "Src is not a file"
  else if can_patch (Unix.stat src).st_size ranges then
    Error "File not large enough for specified patch[es]"
  else
  let in_channel = open_in src in
  let out_channel = open_out dst in
  let buffer = Bytes.create buffer_size in
  let rec copy_loop bytes_read len =
    match input in_channel buffer 0 (min (len - bytes_read) buffer_size) with
    | 0 -> ()
    | r -> ignore (output out_channel buffer 0 r); copy_loop (bytes_read + r) len
  in
  let stream_patch (off, len) =
    output_binary_int out_channel off;
    output_binary_int out_channel len;
    seek_in in_channel off;
    copy_loop 0 len
  in
  try
    List.iter stream_patch ranges;
    flush out_channel;
    close_in in_channel;
    close_out out_channel;
    Ok "Created patch"
  with _ ->
    close_in_noerr in_channel;
    close_out_noerr out_channel;
    Unix.unlink dst;
    Error "Write error"

let get_patch_header in_chan =
  try
    let off = input_binary_int in_chan in
    let len = input_binary_int in_chan in
    Some (off, len)
  with End_of_file -> None

let apply_patch src dst =
  if not (Sys.file_exists src) then
    Error "Patch file does not exist"
  else if (Sys.is_directory src) then
    Error "Patch is not a file"
  else if not (Sys.file_exists dst) then
    Error "Dst file does not exist"
  else if (Sys.is_directory dst) then
    Error "Dst is not a file"
  else
    let in_channel = open_in src in
    let out_channel = open_out_gen [Open_wronly; Open_binary] 0 dst in
    let buffer = Bytes.create buffer_size in
    let rec copy_loop bytes_read len =
      match input in_channel buffer 0 (min (len - bytes_read) buffer_size) with
      | 0 -> ()
      | r -> ignore (output out_channel buffer 0 r); copy_loop (bytes_read + r) len
    in
    let rec apply_loop () =
      match get_patch_header in_channel with
      | None -> ()
      | Some (off, len)
        -> begin
          seek_out out_channel off;
          copy_loop 0 len;
          apply_loop ()
        end
    in
    try
      apply_loop ();
      flush out_channel;
      close_in in_channel;
      close_out out_channel;
      Ok "Patched file"
    with _ ->
      close_in_noerr in_channel;
      close_out_noerr out_channel;
      Unix.unlink dst;
      Error "Write error"
