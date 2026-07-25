(** Starting a run.

    {!Raycaster.Engine.run} asks for a function from a world and a player to a
    world, which is all the engine needs to know: it hands back whatever it was
    drawing and takes whatever it is given. The generator carries rather more
    than a world — the catalogue entry behind each room, which of its walls are
    already open, and the run's random state — so that lives in a reference
    here, in the game, and the engine stays a pure function of the world it is
    handed. *)

open Raycaster

(** Open a window on a fresh house and explore it until the player quits.

    [seed] fixes the house: the same seed is the same rooms in the same order,
    every time. Without one a run picks its own and is never seen again. *)
let play ?seed () =
  let seed =
    match seed with
    | Some s -> s
    | None ->
        Random.self_init ();
        Random.bits ()
  in
  let start = Generator.start ~seed in
  (* The first ring has to be standing before the first frame is drawn, or the
     player would spawn looking at three doorways onto nothing. *)
  let house =
    ref (Generator.horizon start (Player.spawn start.Generator.world))
  in
  let grow _ player =
    house := Generator.horizon !house player;
    !house.Generator.world
  in
  Engine.run ~grow !house.Generator.world
