open Cmdliner;;
open Binpatch.Patch;;

type create_error_reason = No_src | No_dst | No_off | No_len
exception Create_error of create_error_reason

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

let c_off =
  let doc = "Offset in binary input file" in
  Arg.(value & opt (some int) None & info ["o"; "off"] ~docv:"OFF" ~doc)

let c_len =
  let doc = "Length" in
  Arg.(value & opt (some int) None & info ["l"; "len"] ~docv:"LEN" ~doc)

let c_mode =
  Arg.(last & vflag_all [None] [c_create; c_apply])

(*
  Check flags exist: src, dst, off, len
  Check src file exists, and is readable
  Check 0 < off < off + len < len(file)
 *)

let validate_create_flags src dst off len =
  match (src, dst, off, len) with
  | (Some src, Some dst, Some off, Some len) -> Ok (src, dst, off, len)
  | (None, _, _, _) -> Error "No_src"
  | (_, None, _, _) -> Error "No_dst"
  | (_, _, None, _) -> Error "No_off"
  | (_, _, _, None) -> Error "No_len"

let validate_apply_flags src dst =
  match (src, dst) with
  | (Some src, Some dst) -> Ok (src, dst)
  | (None, _) -> Error "No_src"
  | (_, None) -> Error "No_dst"
  (* | (None, _, _, _) -> Error No_src
   * | (_, None, _, _) -> Error No_dst
   * | (_, _, None, _) -> Error No_off
   * | (_, _, _, None) -> Error No_len *)

let try_create src dst off len =
  match Result.bind (validate_create_flags src dst off len)
          (fun (src, dst, off, len) ->
            create_patch src dst off len)
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
            (off:int option)
            (len:int option) =
  match mode with
  | Some Create -> try_create src dst off len
  | Some Apply -> try_apply src dst
  | None -> `Error (false, "Must supply a command")

let main =
  let doc = "Creates and applies binary patch files" in
  let man = [
      `S Manpage.s_description;
      `P "$(tname) can create binary patch files from a source file, offset, and length";
      `P "Use either --create or --apply to create or apply patches";
      `P "  $(b,create) - Creates a patch file. Requires: $(b,src), $(b,off), $(b,len)";
      `P "  $(b,apply) - Applies a patch file. Requires: $(b,src), $(b,dst)";]
  in
  Term.(ret (const run_cmd $ c_mode $ c_src $ c_dst $ c_off $ c_len)),
  Term.info "binpatch" ~version:"v0.0.1" ~doc ~exits:Term.default_exits ~man;;

let () =
  Term.(exit @@ eval main)
