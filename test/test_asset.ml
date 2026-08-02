(** Where a picture is looked for.

    Every case here drives {!Asset.resolve} with a made-up [exists] over a
    made-up tree, which is the whole point of it taking one: the rule is about
    {e which} directories are tried and in what order, and answering that
    against a real filesystem would test this machine rather than the rule. The
    only thing {!Asset.path} adds is the three real answers, and it has nowhere
    to hide a mistake. *)

open Camlcast_core
open Support

(** Joined the way {!Asset} joins, which is not the way a POSIX machine spells
    it: [Filename.concat] uses a backslash on Windows, so a case that wrote its
    expected path out by hand passed on a Mac and failed on a runner. What is
    under test is which directory is tried and in what order, and that rule is
    the same everywhere — so the cases below name the components and leave the
    separator to [Filename]. *)
let ( / ) = Filename.concat

(** An [exists] that says yes to exactly these paths. *)
let tree paths path = List.mem path paths

let resolve ?override ~exe present name =
  Asset.resolve ~exists:(tree present) ~exe ~override name

let found = function Ok path -> path | Error (`Msg m) -> Alcotest.fail m

let message = function
  | Ok path -> Alcotest.fail ("unexpectedly found " ^ path)
  | Error (`Msg m) -> m

(* A macOS bundle: the binary sits in Contents/MacOS and its files in
   Contents/Resources, so the first root is one level up and across. *)
let a_bundle_looks_beside_the_binary () =
  let bundled = "/A/Demo.app/Contents" / "Resources" / "art/wall.png" in
  Alcotest.(check string)
    "the Resources directory" bundled
    (found
       (resolve ~exe:"/A/Demo.app/Contents/MacOS/demo" [ bundled ]
          "art/wall.png"))

(* An AppImage, a tarball or a Windows folder puts the files beside the binary
   itself, which is the second root. *)
let files_may_sit_beside_the_binary () =
  let beside = "/opt/game" / "art/wall.png" in
  Alcotest.(check string)
    "the executable's own directory" beside
    (found (resolve ~exe:"/opt/game/demo" [ beside ] "art/wall.png"))

(* A dune build, which is the layout every developer actually runs: each
   executable is one directory below _build/default, beside the copied source
   tree. Without this root, nothing would find anything before it was packaged. *)
let a_dune_build_looks_one_directory_up () =
  let above = "/repo/_build/default" / "assets/wall.png" in
  Alcotest.(check string)
    "one above the executable" above
    (found
       (resolve ~exe:"/repo/_build/default/bin/demo.exe" [ above ]
          "assets/wall.png"))

(* An opam or system prefix, which is what `opam install camlcast-demo`
   produces: the binary in bin/ and the art the root dune installs in share/
   beside it. The same up-and-across shape as the bundle, which is why a prefix
   needs no dune-site and nothing had to be told where it is. *)
let a_prefix_looks_in_its_share_directory () =
  let installed =
    "/usr/local" / "share" / "camlcast-demo" / "assets/font.png"
  in
  Alcotest.(check string)
    "share, under the binary's own name" installed
    (found
       (resolve ~exe:"/usr/local/bin/camlcast-demo" [ installed ]
          "assets/font.png"))

(* The share directory is the executable's name and not this project's, so a
   game built on the engine finds its own — and on Windows the [.exe] comes off
   first, or an installer would have to spell the directory "foo.exe". *)
let the_share_directory_is_named_after_the_binary () =
  let theirs = "/usr" / "share" / "wanderer" / "art/wall.png" in
  Alcotest.(check string)
    "a game called wanderer reads share/wanderer" theirs
    (found (resolve ~exe:"/usr/bin/wanderer" [ theirs ] "art/wall.png"));
  Alcotest.(check string)
    "and the same one from wanderer.exe" theirs
    (found (resolve ~exe:"/usr/bin/wanderer.exe" [ theirs ] "art/wall.png"))

(* The roots are tried in order, so a bundle's own copy wins over anything that
   happens to be lying beside or above the binary. *)
let the_first_root_that_has_it_wins () =
  let bundled = "/A/Contents" / "Resources" / "art/wall.png" in
  (* Above the binary is /A/Contents, the third root — not /A, which is no root
     at all and so could never have lost to anything. *)
  let stale = "/A/Contents" / "art/wall.png" in
  Alcotest.(check string)
    "the bundle's copy, not the stale one above it" bundled
    (found
       (resolve ~exe:"/A/Contents/MacOS/demo" [ bundled; stale ] "art/wall.png"))

(* The override is used alone. Falling back from it would be worse than
   failing: a developer who points it at the wrong directory would get the
   copy they were trying to stop using, and nothing would say so. *)
let the_override_is_used_alone () =
  let chosen = "/scratch" / "art/wall.png" in
  let present = [ chosen; "/repo/_build/default" / "assets/wall.png" ] in
  Alcotest.(check string)
    "it is preferred" chosen
    (found
       (resolve ~override:"/scratch" ~exe:"/repo/_build/default/bin/demo.exe"
          present "art/wall.png"));
  let m =
    message
      (resolve ~override:"/nowhere" ~exe:"/repo/_build/default/bin/demo.exe"
         present "assets/wall.png")
  in
  Alcotest.(check bool)
    (Printf.sprintf "and nothing else is tried: %s" m)
    true
    (mentions m "/nowhere"
    && (not (mentions m "/repo/_build/default"))
    && mentions m Asset.variable)

(* The only useful thing to say about a missing picture is where it was not. *)
let nothing_found_names_every_root () =
  let m = message (resolve ~exe:"/opt/game/demo" [] "art/wall.png") in
  Alcotest.(check bool)
    (Printf.sprintf "the name is in it: %s" m)
    true
    (mentions m "art/wall.png");
  List.iter
    (fun root ->
      Alcotest.(check bool)
        (Printf.sprintf "%s is in it" root)
        true (mentions m root))
    (Asset.roots ~exe:"/opt/game/demo" ~override:None)

let roots_are_ordered_and_distinct () =
  Alcotest.(check (list string))
    "bundle, beside, above, share"
    [
      "/opt/game" / "Resources";
      "/opt/game/bin";
      "/opt/game";
      "/opt/game" / "share" / "demo";
    ]
    (Asset.roots ~exe:"/opt/game/bin/demo" ~override:None);
  Alcotest.(check (list string))
    "and an override replaces the lot" [ "/scratch" ]
    (Asset.roots ~exe:"/opt/game/bin/demo" ~override:(Some "/scratch"))

(* {!Asset.read} is {!Asset.path} and then a loader, and what it promises is
   that the first error wins. Texture.of_asset and Image.of_asset are this and
   nothing else, so a missing file has to come back saying where it was looked
   for rather than whatever a loader would have made of a path that is not
   there — which means the loader must not run at all. This is the only claim
   about it that can be made without a disk, and it is the one that matters. *)
let read_does_not_reach_the_loader_without_a_file () =
  let ran = ref false in
  let loader _ =
    ran := true;
    Ok "loaded"
  in
  match Asset.read loader "camlcast/no/such/asset.png" with
  | Ok _ -> Alcotest.fail "a missing asset came back as a success"
  | Error (`Msg message) ->
      Alcotest.(check bool) "the loader was never called" false !ran;
      Alcotest.(check bool)
        "and the error is the lookup's, naming the roots it tried" true
        (mentions message "no such asset")

let () =
  Alcotest.run "Asset"
    [
      ( "roots",
        [
          case "a bundle looks beside the binary"
            a_bundle_looks_beside_the_binary;
          case "files may sit beside the binary" files_may_sit_beside_the_binary;
          case "a dune build looks one directory up"
            a_dune_build_looks_one_directory_up;
          case "a prefix looks in its share directory"
            a_prefix_looks_in_its_share_directory;
          case "the share directory is named after the binary"
            the_share_directory_is_named_after_the_binary;
          case "the first root that has it wins" the_first_root_that_has_it_wins;
          case "roots are ordered and distinct" roots_are_ordered_and_distinct;
        ] );
      ( "failing",
        [
          case "the override is used alone" the_override_is_used_alone;
          case "nothing found names every root" nothing_found_names_every_root;
          case "read does not reach the loader without a file"
            read_does_not_reach_the_loader_without_a_file;
        ] );
    ]
