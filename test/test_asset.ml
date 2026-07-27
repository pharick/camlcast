(** Where a picture is looked for.

    Every case here drives {!Asset.resolve} with a made-up [exists] over a
    made-up tree, which is the whole point of it taking one: the rule is about
    {e which} directories are tried and in what order, and answering that
    against a real filesystem would test this machine rather than the rule. The
    only thing {!Asset.path} adds is the three real answers, and it has nowhere
    to hide a mistake. *)

open Raycaster
open Support

(** An [exists] that says yes to exactly these paths. *)
let tree paths path = List.mem path paths

let resolve ?override ~exe present name =
  Asset.resolve ~exists:(tree present) ~exe ~override name

let found = function
  | Ok path -> path
  | Error (`Msg m) -> Alcotest.fail m

let message = function
  | Ok path -> Alcotest.fail ("unexpectedly found " ^ path)
  | Error (`Msg m) -> m

(* A macOS bundle: the binary sits in Contents/MacOS and its files in
   Contents/Resources, so the first root is one level up and across. *)
let a_bundle_looks_beside_the_binary () =
  Alcotest.(check string)
    "the Resources directory" "/A/House.app/Contents/Resources/art/wall.png"
    (found
       (resolve ~exe:"/A/House.app/Contents/MacOS/house"
          [ "/A/House.app/Contents/Resources/art/wall.png" ]
          "art/wall.png"))

(* An AppImage, a tarball or a Windows folder puts the files beside the binary
   itself, which is the second root. *)
let files_may_sit_beside_the_binary () =
  Alcotest.(check string)
    "the executable's own directory" "/opt/game/art/wall.png"
    (found
       (resolve ~exe:"/opt/game/house" [ "/opt/game/art/wall.png" ]
          "art/wall.png"))

(* A dune build, which is the layout every developer actually runs: each
   executable is one directory below _build/default, beside the copied source
   tree. Without this root, nothing would find anything before it was packaged. *)
let a_dune_build_looks_one_directory_up () =
  Alcotest.(check string)
    "one above the executable" "/repo/_build/default/assets/wall.png"
    (found
       (resolve ~exe:"/repo/_build/default/bin/demo.exe"
          [ "/repo/_build/default/assets/wall.png" ]
          "assets/wall.png"))

(* The roots are tried in order, so a bundle's own copy wins over anything that
   happens to be lying beside or above the binary. *)
let the_first_root_that_has_it_wins () =
  Alcotest.(check string)
    "the bundle's copy, not the stale one above it"
    "/A/Contents/Resources/art/wall.png"
    (found
       (resolve ~exe:"/A/Contents/MacOS/house"
          [ "/A/Contents/Resources/art/wall.png"; "/A/art/wall.png" ]
          "art/wall.png"))

(* The override is used alone. Falling back from it would be worse than
   failing: a developer who points it at the wrong directory would get the
   copy they were trying to stop using, and nothing would say so. *)
let the_override_is_used_alone () =
  let present =
    [ "/scratch/art/wall.png"; "/repo/_build/default/assets/wall.png" ]
  in
  Alcotest.(check string)
    "it is preferred" "/scratch/art/wall.png"
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
  let m = message (resolve ~exe:"/opt/game/house" [] "art/wall.png") in
  Alcotest.(check bool)
    (Printf.sprintf "the name is in it: %s" m)
    true (mentions m "art/wall.png");
  List.iter
    (fun root ->
      Alcotest.(check bool)
        (Printf.sprintf "%s is in it" root)
        true (mentions m root))
    (Asset.roots ~exe:"/opt/game/house" ~override:None)

let roots_are_ordered_and_distinct () =
  Alcotest.(check (list string))
    "bundle, beside, above"
    [ "/opt/game/Resources"; "/opt/game/bin"; "/opt/game" ]
    (Asset.roots ~exe:"/opt/game/bin/house" ~override:None);
  Alcotest.(check (list string))
    "and an override replaces the lot" [ "/scratch" ]
    (Asset.roots ~exe:"/opt/game/bin/house" ~override:(Some "/scratch"))

let () =
  Alcotest.run "Asset"
    [
      ( "roots",
        [
          case "a bundle looks beside the binary" a_bundle_looks_beside_the_binary;
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
