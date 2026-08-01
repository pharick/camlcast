let variable = "CAMLCAST_ASSETS"

let roots ~exe ~override =
  match override with
  | Some dir -> [ dir ]
  | None ->
      let beside = Filename.dirname exe in
      let above = Filename.dirname beside in
      [
        (* A macOS bundle: Contents/MacOS/exe, files under Contents/Resources. *)
        Filename.concat above "Resources";
        (* Beside the executable: an AppImage, a tarball, a Windows folder. *)
        beside;
        (* A dune build, where every executable sits one directory below
           _build/default beside the copied source tree. *)
        above;
        (* An opam or system prefix: the binary in bin/, its art in share/
           under the binary's own name. One directory up and across, which is
           the bundle's shape again — and the name comes off the executable
           rather than being written here, because this module holds no
           content and knows no package. *)
        Filename.concat above
          (Filename.concat "share"
             (Filename.remove_extension (Filename.basename exe)));
      ]

let resolve ~exists ~exe ~override name =
  let tried = roots ~exe ~override in
  match
    List.find_opt (fun root -> exists (Filename.concat root name)) tried
  with
  | Some root -> Ok (Filename.concat root name)
  | None ->
      let only =
        match override with
        | Some _ -> Printf.sprintf " (only, because %s is set)" variable
        | None -> ""
      in
      Error
        (`Msg
           (Printf.sprintf "%s: no such asset. Looked in %s%s" name
              (String.concat ", " tried) only))

let path name =
  resolve ~exists:Sys.file_exists ~exe:Sys.executable_name
    ~override:(Sys.getenv_opt variable) name
