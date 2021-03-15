let buffer_size = 1024 * 8

let create_patch src dst off len =
  if not (Sys.file_exists src) then
    Error "Src file does not exist"
  else if (Sys.is_directory src) then
    Error "Src is not a file"
  else if (Unix.stat src).st_size < (off + len) then
    Error "File not large enough for specified patch"
  else
  let in_channel = open_in src in
  let out_channel = open_out dst in
  let buffer = Bytes.create buffer_size in
  ignore (seek_in in_channel off);
  output_binary_int out_channel off;
  output_binary_int out_channel len;
  let rec copy_loop bytes_read =
    match input in_channel buffer 0 (min (len - bytes_read) buffer_size) with
    | 0 -> ()
    | r -> ignore (output out_channel buffer 0 r); copy_loop (bytes_read + r)
  in
  try
    copy_loop 0;
    flush out_channel;
    close_in in_channel;
    close_out out_channel;
    Ok "Created patch"
  with _ ->
    close_in_noerr in_channel;
    close_out_noerr out_channel;
    Unix.unlink dst;
    Error "Write error"

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
    let off = input_binary_int in_channel in
    let len = input_binary_int in_channel in
    if (Unix.stat dst).st_size < (off + len) then
      Error "File not large enough for specified patch"
    else
      let out_channel = open_out_gen [Open_wronly; Open_binary] 0 dst in
      let buffer = Bytes.create buffer_size in
      ignore (seek_out out_channel off);
      let rec copy_loop bytes_read =
        match input in_channel buffer 0 (min (len - bytes_read) buffer_size) with
        | 0 -> ()
        | r -> ignore (output out_channel buffer 0 r); copy_loop (bytes_read + r)
      in
      try
        copy_loop 0;
        flush out_channel;
        close_in in_channel;
        close_out out_channel;
        Ok "Patched file"
      with _ ->
        close_in_noerr in_channel;
        close_out_noerr out_channel;
        Unix.unlink dst;
        Error "Write error"
