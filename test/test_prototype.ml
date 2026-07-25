open Raycaster
open House
open Support

let named = List.map (fun (p : Prototype.t) -> (p.Prototype.name, p)) Catalogue.all

(* The winding rule is not cosmetic. Transform.between pairs the endpoints of
   two linked doorways in reverse, because the two rooms describe the same
   opening from opposite sides; a clockwise room would come out mirrored, with
   its walls inside out and the player facing backwards through every doorway
   into it. *)
let outlines_run_counter_clockwise () =
  List.iter
    (fun (name, p) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s winds the right way (%.1f)" name
           (Prototype.winding p))
        true
        (Prototype.winding p > 0.))
    named

let outlines_are_closed_and_simple () =
  List.iter
    (fun (name, (p : Prototype.t)) ->
      let n = Array.length p.Prototype.outline in
      Alcotest.(check bool)
        (Printf.sprintf "%s has at least three corners" name)
        true (n >= 3);
      (* No two corners in the same place, or the wall between them would have
         no length and no normal. *)
      let distinct =
        List.sort_uniq compare (Array.to_list p.Prototype.outline)
      in
      Alcotest.(check int)
        (name ^ " has no repeated corner")
        n (List.length distinct))
    named

(* A doorway is cut into the middle of a wall and leaves a jamb either side. A
   wall no longer than the doorway would leave none, and the two rooms' boundary
   and thresholds would stop agreeing about where the room ends. *)
let every_exit_is_wide_enough () =
  List.iter
    (fun (name, (p : Prototype.t)) ->
      Array.iter
        (fun k ->
          let a, b = Prototype.segment p k in
          let length = Vec.length (Vec.sub b a) in
          Alcotest.(check bool)
            (Printf.sprintf "%s exit %d is %.2f, needs more than %.2f" name k
               length Prototype.width)
            true
            (length > Prototype.width +. (2. *. Config.collision_padding)))
        p.Prototype.exits;
      Alcotest.(check bool)
        (name ^ " has somewhere to be entered")
        true
        (Array.length p.Prototype.exits >= 1);
      Array.iter
        (fun k ->
          Alcotest.(check bool)
            (Printf.sprintf "%s exit %d is a real segment" name k)
            true
            (k >= 0 && k < Array.length p.Prototype.outline))
        p.Prototype.exits)
    named

(* World.link will only join two thresholds that agree in length and height, so
   a house that wants to join any two doorways — including two it did not plan
   for when it built the rooms — can only do so if every doorway is identical.
   This is what makes a loop possible. *)
let every_door_is_the_same_door () =
  let openings =
    List.concat_map
      (fun (_, (p : Prototype.t)) ->
        Array.to_list p.Prototype.exits
        |> List.map (fun k ->
               let _, t = Prototype.cut p k ~name:"x" () in
               (t.Room.length, t.Room.height)))
      named
  in
  (* To within the tolerance World.link itself uses. Room.doorway computes the
     endpoints from the wall's own direction, so two doorways of nominally the
     same width come out a bit or two apart depending on which way their wall
     ran; that is far inside what a link will accept, and far outside what
     exact float comparison would. *)
  List.iter
    (fun (length, height) ->
      Alcotest.(check bool)
        (Printf.sprintf "%.17g x %.17g is the standard doorway" length height)
        true
        (Float.abs (length -. Prototype.width) < 1e-6
        && Float.abs (height -. Prototype.opening) < 1e-6))
    openings

(* Ceilings differ from one kind of room to the next while doorways do not, so
   walking through a door changes the height of the room around you without
   changing the door. *)
let ceilings_differ_but_doors_do_not () =
  let heights =
    List.map (fun (_, (p : Prototype.t)) -> p.Prototype.height) named
  in
  Alcotest.(check bool)
    "more than one ceiling height in the catalogue" true
    (List.length (List.sort_uniq compare heights) > 1);
  List.iter
    (fun (name, (p : Prototype.t)) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s is taller than its doorways" name)
        true
        (p.Prototype.height > Prototype.opening))
    named

(* Flat, and every one of them at the same elevation. The elevation around a
   loop of rooms need not come back to itself, so anything else would put a step
   in some doorway of a house that closes loops. *)
let every_floor_is_level () =
  List.iter
    (fun (name, p) ->
      let floor = (Prototype.floor p).Room.plane in
      Alcotest.check close
        (name ^ " is level")
        (Plane.elevation floor (Vec.make 0. 0.))
        (Plane.elevation floor (Vec.make 7. (-3.)));
      Alcotest.check close (name ^ " is at zero") 0.
        (Plane.elevation floor (Vec.make 2. 2.)))
    named

(* A prototype is a room with every exit still solid, which is the state the
   generator has to be able to hand a player: somewhere to stand and look at,
   whose neighbours need not exist. *)
let a_prototype_starts_closed () =
  List.iter
    (fun (name, p) ->
      let walls = Prototype.boundary p in
      Alcotest.(check int)
        (name ^ " is walled all the way round")
        (Array.length p.Prototype.outline)
        (Array.length walls);
      Array.iteri
        (fun k (w : Room.wall) ->
          let a, b = Prototype.segment p k in
          Alcotest.check vec (Printf.sprintf "%s wall %d starts right" name k) a
            w.Room.a;
          Alcotest.check vec (Printf.sprintf "%s wall %d ends right" name k) b
            w.Room.b)
        walls)
    named

(* Cutting a wall open replaces it with two jambs and a doorway that runs the
   same way, which is the winding rule the link depends on. *)
let cutting_preserves_the_wall () =
  List.iter
    (fun (name, p) ->
      Array.iter
        (fun k ->
          let a, b = Prototype.segment p k in
          let jambs, t = Prototype.cut p k ~name:"x" () in
          Alcotest.(check int) (name ^ " leaves two jambs") 2 (List.length jambs);
          let first = List.nth jambs 0 and second = List.nth jambs 1 in
          Alcotest.check vec "the first jamb starts where the wall did" a
            first.Room.a;
          Alcotest.check vec "the doorway carries on from it" first.Room.b
            t.Room.a;
          Alcotest.check vec "the second jamb carries on from the doorway"
            t.Room.b second.Room.a;
          Alcotest.check vec "and ends where the wall did" b second.Room.b;
          Alcotest.(check bool)
            (name ^ " keeps the wall above the opening")
            true
            (t.Room.lintel <> None);
          Alcotest.(check bool)
            (name ^ " leaves the opening open unless asked")
            true
            (t.Room.door = None);
          let _, shut =
            Prototype.cut p k ~name:"x" ~door:Assets.Surfaces.door ()
          in
          Alcotest.(check bool)
            (name ^ " can have a leaf hung across it")
            true
            (shut.Room.door <> None))
        p.Prototype.exits)
    named

let () =
  Alcotest.run "Prototype"
    [
      ( "shape",
        [
          case "outlines run counter-clockwise" outlines_run_counter_clockwise;
          case "outlines are closed and simple" outlines_are_closed_and_simple;
          case "a prototype starts closed" a_prototype_starts_closed;
          case "every floor is level" every_floor_is_level;
        ] );
      ( "doorways",
        [
          case "every exit is wide enough" every_exit_is_wide_enough;
          case "every door is the same door" every_door_is_the_same_door;
          case "ceilings differ but doors do not" ceilings_differ_but_doors_do_not;
          case "cutting preserves the wall" cutting_preserves_the_wall;
        ] );
    ]
