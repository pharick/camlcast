(** Locates the files a game loads, on a machine with nothing else installed.

    A texture or picture generated in code needs no path; one read from disk
    does, and the path must still exist after the game has been copied to
    someone with neither this source tree nor an opam switch. That rules out
    asking the build system: [dune-site] bakes in the prefix it was built
    against, and a player has no prefix at all.

    Every root here is therefore {e relative to the executable}, the one thing a
    program always knows about itself. This matches every target layout. A macOS
    [.app] puts the binary in [Contents/MacOS] and its files in
    [Contents/Resources]; an AppImage or a plain tarball puts them beside the
    binary; an opam or system prefix puts the binary in [bin/] and its files in
    [share/] under the binary's own name, which is again the bundle shape. Each
    is a root below, and the same code finds all of them, the prefix included,
    without being told where it is. The share directory is named after the
    executable and not after a package: a game called [foo] installs [foo] and
    reads [share/foo], and the engine needs to know neither word.

    {!variable} overrides the search for development, where the executable is
    somewhere in [_build] and the answer is the working directory. When set it
    is used {e alone}, so pointing it at the wrong directory produces an error
    that says so rather than silently falling back to a stale copy.

    The engine holds no content, so nothing here names a directory: a caller
    asks for ["assets/brick.png"] and supplies its own vocabulary. *)

val variable : string
(** [CAMLCAST_ASSETS], the environment variable that overrides the search, for
    development. *)

val roots : exe:string -> override:string option -> string list
(** [roots ~exe ~override] is the list of directories an asset is looked for in,
    in the order tried. [exe] is the path of the running executable, from which
    every root is derived. The four roots are the macOS bundle's
    [Contents/Resources], the directory beside the binary, the one above it, and
    [share/] under the binary's own name. [override] is the value of
    {!variable}: when given, it is the whole list, alone.

    Taking [exe] and [override] as arguments rather than reading the process
    makes the rule testable: {!resolve} below is a pure function of these two
    and an [exists], and {!path} is those three supplied from the environment.
*)

val resolve :
  exists:(string -> bool) ->
  exe:string ->
  override:string option ->
  string ->
  (string, [ `Msg of string ]) result
(** [resolve ~exists ~exe ~override name] is the first root of
    [roots ~exe ~override] under which [name] is present, joined to it — or an
    error naming every root tried, since the useful thing to report about a
    missing asset is where it was not found. [name] is a caller's own relative
    path, such as ["assets/brick.png"].

    [exists] is called on each candidate full path in turn and answers whether a
    file is there. In a program it is [Sys.file_exists]; a test passes its own
    to describe a disk that is not on this machine. *)

val path : string -> (string, [ `Msg of string ]) result
(** [path name] is {!resolve} with the real environment supplied:
    [Sys.executable_name], {!variable}, and [Sys.file_exists] on the real disk.
    This is the one a game calls; the error is the same list of roots. *)

val read :
  (string -> ('a, [ `Msg of string ]) result) ->
  string ->
  ('a, [ `Msg of string ]) result
(** [read load name] is {!path} followed by [load] on the found path, the first
    error winning. A missing asset reports where it was looked for and a corrupt
    one reports what was wrong with it; the caller distinguishes them by the
    message, not by which call failed.

    {!Texture.of_asset} and {!Image.of_asset} are exactly this and nothing else.
    The composition is named once here rather than spelled out in each of them,
    because providing it ready-made is the reason either function exists. *)
