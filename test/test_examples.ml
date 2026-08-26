(** Every step of {!page-"making-a-game"}, built and looked at without a window.

    The examples are the guide's, one complete program per step, and until now
    nothing built the worlds in them: they were checked by compiling, and
    {!Camlcast_demo.Catalogue} never listed them. Twenty-one of them carried a
    doorway wound against its own room for exactly that reason — every other
    check in the engine passes one, and the only thing that does not is looking.

    The sources are copied into a library by [test/levels/dune], which cuts off
    the [let () =] that opens a window and leaves the definitions above it. The
    entries below name what each step calls its description: [level] where a
    step has one, and the component it plays where the description is a function
    of state. Adding a step to the guide means adding a line here. *)

open Camlcast_core
open Camlcast
open Support

(* The steps whose description needs nothing off the disk. *)
let plain =
  [
    ("step01_room", Example_levels.Step01_room.level);
    ("step02_masonry", Example_levels.Step02_masonry.level);
    ("step03_pillars", Example_levels.Step03_pillars.level);
    ("step04_air", Example_levels.Step04_air.level);
    ("step05_dressing", Example_levels.Step05_dressing.level);
    ("step06_antechamber", Example_levels.Step06_antechamber.level);
    ("step07_component", Example_levels.Step07_component.level);
    ("step08_state", Example_levels.Step08_state.level);
    ("step09_gaze", Example_levels.Step09_gaze.level);
    ("step10_burning", Example_levels.Step10_burning.game ());
    ("step11_wisp", Example_levels.Step11_wisp.game ());
    ("step12_hud", Example_levels.Step12_hud.game ());
  ]

(* And those that read a font or a picture first. A missing file is this
   suite's failure as much as a crooked wall is: the guide ships the assets. *)
let needing_art =
  [
    ( "step13_words",
      Result.map Example_levels.Step13_words.game
        Example_levels.Step13_words.typeface );
    ( "step14_prompt",
      Result.map Example_levels.Step14_prompt.game
        Example_levels.Step14_prompt.typeface );
    ( "step15_gate",
      Result.map Example_levels.Step15_gate.game
        Example_levels.Step15_gate.typeface );
    ( "step16_winch",
      Result.map Example_levels.Step16_winch.game
        Example_levels.Step16_winch.typeface );
    ( "step17_crossings",
      Result.map Example_levels.Step17_crossings.game
        Example_levels.Step17_crossings.typeface );
    ( "step18_effect",
      Result.map Example_levels.Step18_effect.game
        Example_levels.Step18_effect.typeface );
    ( "step19_embers",
      Result.map Example_levels.Step19_embers.game
        Example_levels.Step19_embers.typeface );
    ( "step20_store",
      Result.map Example_levels.Step20_store.game
        Example_levels.Step20_store.typeface );
    ( "step21_context",
      Result.map Example_levels.Step21_context.game
        Example_levels.Step21_context.typeface );
    ( "step22_check",
      Result.map Example_levels.Step22_check.game
        Example_levels.Step22_check.typeface );
    ( "step23_controls",
      Result.map Example_levels.Step23_controls.game
        Example_levels.Step23_controls.typeface );
    ( "step24_escape",
      Result.map Example_levels.Step24_escape.game
        Example_levels.Step24_escape.typeface );
    ( "step25_launcher",
      Result.map Example_levels.Step25_launcher.game
        Example_levels.Step25_launcher.typeface );
    ( "step26_shipping",
      Result.map Example_levels.Step26_shipping.game
        (Example_levels.Step26_shipping.load_art ()) );
  ]

let world_of description = (Mount.build description).Scene.world

let is_sound name world =
  Alcotest.(check (list string))
    (name ^ ": every doorway is wound with its own room")
    [] (wound_wrong world);
  Alcotest.(check (list string))
    (name ^ ": Check has nothing to report")
    []
    (List.filter_map
       (fun (d : Check.t) ->
         match d.Check.severity with
         | Check.Error -> Some d.Check.summary
         | Check.Warning -> None)
       (Check.assembled world));
  List.iter
    (fun (room, _, (portal : World.portal)) ->
      Alcotest.check close
        (name ^ ": no step in the floor at " ^ portal.World.threshold.Room.name)
        0.
        (World.seam_gap world ~room portal))
    (joined world)

let each =
  List.map
    (fun (name, description) ->
      case name (fun () -> is_sound name (world_of description)))
    plain
  @ List.map
      (fun (name, loaded) ->
        case name (fun () ->
            match loaded with
            | Ok description -> is_sound name (world_of description)
            | Error (`Msg reason) ->
                Alcotest.failf "%s: could not read its art: %s" name reason))
      needing_art

let () = Alcotest.run "Examples" [ ("sound", each) ]
