(** Where the files a game loads are, on a machine that has nothing else on it.

    A texture or a picture generated in code needs no path; one read off the
    disk does, and it has to be a path that still exists after the game has been
    copied to someone who has neither this source tree nor an opam switch. That
    rules out asking the build system: [dune-site] resolves into the opam
    prefix, and a player has no opam prefix.

    So every root here is {e relative to the executable}, which is the one thing
    a program always knows about itself. That is not a compromise — it is the
    shape both target layouts already have. A macOS [.app] puts the binary in
    [Contents/MacOS] and its files in [Contents/Resources]; an AppImage or a
    plain tarball puts them beside the binary. Each is a root below, and the
    same code finds them all.

    {!variable} is the way out of that for development, where the executable is
    somewhere in [_build] and the answer is wherever you happen to be working.
    When it is set it is used {e alone}, so pointing it at the wrong directory
    is an error that says so rather than a silent fall back to a stale copy.

    The engine holds no content, so nothing here names a directory: a caller
    asks for ["assets/brick.png"] and supplies its own vocabulary. *)

(** The environment variable that overrides the search, for development. *)
let variable = "CAMLCAST_ASSETS"

(** The directories an asset is looked for in, in order.

    Taking [exe] and [override] as arguments rather than reading the process is
    what makes the rule testable: {!resolve} below is a pure function of these
    two and an [exists], and the whole of {!path} is those three supplied from
    the world. *)
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
      ]

(** [resolve ~exists ~exe ~override name] is the first root under which [name]
    is present, joined to it — or an error naming every root tried, because the
    only useful thing to say about a missing asset is where it was not. *)
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

(** {!resolve}, asking the running program where it is and the environment
    whether it disagrees. *)
let path name =
  resolve ~exists:Sys.file_exists ~exe:Sys.executable_name
    ~override:(Sys.getenv_opt variable) name
