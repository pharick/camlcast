(** Where a picture is looked for.

    Every case here drives {!Asset.resolve} with a made-up [exists] over a
    made-up tree, which is the whole point of it taking one: the rule is about
    {e which} directories are tried and in what order, and answering that
    against a real filesystem would test this machine rather than the rule. The
    only thing {!Asset.path} adds is the three real answers, and it has nowhere
    to hide a mistake. *)

open Camlcast
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
    "bundle, beside, above"
    [ "/opt/game" / "Resources"; "/opt/game/bin"; "/opt/game" ]
    (Asset.roots ~exe:"/opt/game/bin/demo" ~override:None);
  Alcotest.(check (list string))
    "and an override replaces the lot" [ "/scratch" ]
    (Asset.roots ~exe:"/opt/game/bin/demo" ~override:(Some "/scratch"))

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
          case "the first root that has it wins" the_first_root_that_has_it_wins;
          case "roots are ordered and distinct" roots_are_ordered_and_distinct;
        ] );
      ( "failing",
        [
          case "the override is used alone" the_override_is_used_alone;
          case "nothing found names every root" nothing_found_names_every_root;
        ] );
    ]
