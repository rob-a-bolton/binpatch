open Cmdliner;;
open Binpatch.Patch;;

let range_scanner arg_str =
  try
    Ok (Scanf.sscanf arg_str "%d:%d" (fun x y -> (x, y)))
  with Scanf.Scan_failure e -> Error (`Msg e)

let range_printer formatter (x, y) =
  Format.fprintf formatter "%d:%d" x y

let range_conv =
  Arg.(conv (range_scanner, range_printer))

type cmd_mode =
  | Create
  | Apply;;

let c_create =
  let doc = "Create a patch file" in
  Some Create, Arg.info ["c"; "create"] ~doc

let c_apply =
  let doc = "Apply a patch file" in
  Some Apply, Arg.info ["a"; "apply"] ~doc

let c_src =
  let doc = "Source file" in
  Arg.(value & opt (some string) None & info ["s"; "src"] ~docv:"SRC" ~doc)

let c_dst =
  let doc = "Destination file" in
  Arg.(value & opt (some string) None & info ["d"; "dst"] ~docv:"DST" ~doc)

let c_ranges =
  let doc = "Patch data range" in
  Arg.(value & (pos_all range_conv []) & info [] ~docv:"RANGE" ~doc)

let c_mode =
  Arg.(last & vflag_all [None] [c_create; c_apply])

let validate_create_flags src dst =
  match (src, dst) with
  | (Some src, Some dst) -> Ok (src, dst)
  | (None, _) -> Error "No_src"
  | (_, None) -> Error "No_dst"

let validate_apply_flags src dst =
  match (src, dst) with
  | (Some src, Some dst) -> Ok (src, dst)
  | (None, _) -> Error "No_src"
  | (_, None) -> Error "No_dst"

let try_create src dst ranges =
  match Result.bind (validate_create_flags src dst)
          (fun (src, dst) ->
            create_patch src dst ranges)
  with
  | Ok msg -> `Ok msg
  | Error msg -> `Error (false, msg)

let try_apply src dst =
  match Result.bind (validate_apply_flags src dst)
          (fun (src, dst) ->
            apply_patch src dst)
  with
  | Ok msg -> `Ok msg
  | Error msg -> `Error (false, msg)

let run_cmd (mode:cmd_mode option)
            (src:string option)
            (dst:string option)
            (ranges:(int * int) list) =
  match mode with
  | Some Create -> try_create src dst ranges
  | Some Apply -> try_apply src dst
  | None -> `Error (false, "Must supply a command")

let main =
  let doc = "Creates and applies binary patch files" in
  let man = [
      `S Manpage.s_description;
      `P "$(tname) can create binary patch files from a source file, offset, and length";
      `P "Use either --create or --apply to create or apply patches";]
  in
  Term.(ret (const run_cmd $ c_mode $ c_src $ c_dst $ c_ranges)),
  Term.info "binpatch" ~version:"v0.0.1" ~doc ~exits:Term.default_exits ~man;;

let () =
  Term.(exit @@ eval main)
