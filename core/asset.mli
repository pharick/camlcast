(** Where the files a game loads are, on a machine that has nothing else on it.

    A texture or a picture generated in code needs no path; one read off the
    disk does, and it has to be a path that still exists after the game has been
    copied to someone who has neither this source tree nor an opam switch. That
    rules out asking the build system: [dune-site] bakes in the prefix it was
    built against, and a player has no prefix at all.

    So every root here is {e relative to the executable}, which is the one thing
    a program always knows about itself. That is not a compromise — it is the
    shape every target layout already has. A macOS [.app] puts the binary in
    [Contents/MacOS] and its files in [Contents/Resources]; an AppImage or a
    plain tarball puts them beside the binary; an opam or system prefix puts the
    binary in [bin/] and its files in [share/] under the binary's own name,
    which is the bundle's shape once more. Each is a root below, and the same
    code finds them all — the prefix included, without anything having had to be
    told where it is. The share directory is named after the executable and not
    after a package, for the same reason nothing else here names a directory: a
    game called [foo] installs [foo] and reads [share/foo], and the engine does
    not have to know either word.

    {!variable} is the way out of that for development, where the executable is
    somewhere in [_build] and the answer is wherever you happen to be working.
    When it is set it is used {e alone}, so pointing it at the wrong directory
    is an error that says so rather than a silent fall back to a stale copy.

    The engine holds no content, so nothing here names a directory: a caller
    asks for ["assets/brick.png"] and supplies its own vocabulary. *)

val variable : string
(** [CAMLCAST_ASSETS], the environment variable that overrides the search, for
    development. *)

val roots : exe:string -> override:string option -> string list
(** [roots ~exe ~override] is the directories an asset is looked for in, in the
    order they are tried. [exe] is the path of the running executable, which is
    what every root is derived from — the four are the macOS bundle's
    [Contents/Resources], the directory beside the binary, the one above it, and
    [share/] under the binary's own name. [override] is the value of
    {!variable}: given, it is the whole list, alone.

    Taking [exe] and [override] as arguments rather than reading the process is
    what makes the rule testable: {!resolve} below is a pure function of these
    two and an [exists], and the whole of {!path} is those three supplied from
    the world. *)

val resolve :
  exists:(string -> bool) ->
  exe:string ->
  override:string option ->
  string ->
  (string, [ `Msg of string ]) result
(** [resolve ~exists ~exe ~override name] is the first root of
    [roots ~exe ~override] under which [name] is present, joined to it — or an
    error naming every root tried, because the only useful thing to say about a
    missing asset is where it was not. [name] is a caller's own relative path,
    such as ["assets/brick.png"].

    [exists] is asked of each candidate full path in turn and answers whether
    there is a file there; in a program it is [Sys.file_exists], and a test
    passes its own to describe a disk that is not on this machine. *)

val path : string -> (string, [ `Msg of string ]) result
(** [path name] is {!resolve} for that asset, asking the running program where
    it is and the environment whether it disagrees — [Sys.executable_name],
    {!variable} and the real disk, supplied for you. This is the one a game
    calls; the error is the same list of roots. *)

val read :
  (string -> ('a, [ `Msg of string ]) result) ->
  string ->
  ('a, [ `Msg of string ]) result
(** [read load name] is {!path} and then [load] on what it found, the first
    error winning — so a missing asset says where it was looked for and a
    corrupt one says what was wrong with it, and the caller tells them apart by
    reading the message rather than by which call failed.

    {!Texture.of_asset} and {!Image.of_asset} are this and nothing else. They
    were the same two lines written twice, which is a thing worth naming once
    given that the whole reason either exists is that composing those two lines
    was worth doing for a game. *)
