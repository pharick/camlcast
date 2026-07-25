(** A kind of room the house can be built out of: its shape, which of its walls
    a doorway may be cut into, and what it is made of.

    A prototype is not a room. It is a room with every one of its exits still a
    solid wall, because that is the state the generator needs to be able to
    hand a player: a room they can stand in and look at, whose doorways have not
    been decided yet and whose neighbours therefore need not exist. Cutting one
    of those walls open is {!cut}, and it is the only thing that ever adds a
    doorway.

    {1 Every door in the house is the same door}

    {!width} and {!opening} are constants, shared by every prototype, and that
    is load-bearing rather than tidy. {!Raycaster.World.link} will only join two
    thresholds that agree in length and height — otherwise the opening would not
    line up and the seam would show from both sides. A generator that wants to
    join {e any} two doorways, including two it did not plan for when it built
    the rooms, can only do so if all of them are identical. That is what makes a
    loop possible, and a loop is what makes the house bigger on the inside.

    It is also true of the book, which is a pleasant accident.

    {1 Winding}

    [outline] must run counter-clockwise. {!Raycaster.Transform.between} pairs
    the endpoints of two linked doorways {e in reverse}, because the two rooms
    describe the same opening from opposite sides; a clockwise room would come
    out mirrored, with its walls inside out and the player facing backwards
    through every doorway into it. {!Raycaster.Room.doorway}'s own docstring
    carries the argument. *)

open Raycaster

(** How wide every doorway in the house is, and how tall. Both whole numbers of
    cells, so that the leaf's pattern — which tiles once per cell — lands
    squarely on the door rather than being cut off partway through. *)
let width = 1.

let opening = 2.

type t = {
  name : string;  (** what its rooms are called, for diagnostics *)
  outline : Vec.t array;  (** the boundary, counter-clockwise *)
  exits : int array;
      (** which boundary segments may become doorways. Segment [k] runs from
          [outline.(k)] to the point after it. *)
  height : float;  (** how tall its walls are, and so where its ceiling sits *)
  walls : Material.t;
  floor : Material.t;
  ceiling : Material.t;
  fittings : Rng.t -> Room.wall list;
      (** whatever stands about inside it: a pillar, a low sill. Something has
          to, or one ashen box is indistinguishable from the next and there is
          no way to tell you have moved. *)
  weight : int;  (** how often it comes up, against the rest of the catalogue *)
}

(** The two ends of boundary segment [k]. *)
let segment t k =
  let n = Array.length t.outline in
  (t.outline.(k), t.outline.((k + 1) mod n))

(** The boundary as walls, in outline order, so segment [k] is wall [k]. *)
let boundary t =
  Array.init (Array.length t.outline) (fun k ->
      let a, b = segment t k in
      Room.wall ~height:t.height ~material:t.walls a b)

(** Cut boundary segment [k] open: the two jambs left either side of the gap,
    and the doorway filling it. The wall's own height and material become the
    threshold's lintel, so the strip left standing above the opening is still
    drawn — without it you would see over the top of a closed door.

    [door] hangs a leaf across the opening. You still walk through it — walking
    into a door is how you open it — but you cannot see through it, which is the
    difference that matters: a closed door is a room you have to enter to find
    out about. Both sides of a doorway must agree, or it would be a door from
    one room and an opening from the other. *)
let cut t k ~name ?door () =
  let a, b = segment t k in
  Room.doorway ~name ?door ~width ~opening ~height:t.height ~material:t.walls a
    b

(** The floor and the ceiling.

    Both flat, and both at whole heights. Flatness is not laziness: the
    elevation around a cycle of rooms need not come back to itself, so a house
    that closes a loop between two rooms reached by different routes would have
    a visible step in the doorway unless every floor is level. Flat floors are
    what make a loop legal, and a loop is the whole point. *)
let floor t = { Room.plane = Plane.horizontal 0.; material = t.floor }

let ceiling t =
  Room.Roof { Room.plane = Plane.horizontal t.height; material = t.ceiling }

(** Is the outline wound the way {!Raycaster.Transform.between} needs? Twice the
    signed area, by the shoelace formula: positive is counter-clockwise. *)
let winding t =
  let n = Array.length t.outline in
  let sum = ref 0. in
  for k = 0 to n - 1 do
    let a, b = segment t k in
    sum := !sum +. ((a.Vec.x *. b.Vec.y) -. (b.Vec.x *. a.Vec.y))
  done;
  !sum

(** A small square pillar. The commonest fitting, because it is the one thing
    that gives a bare room any parallax at all: walk past it and the wall behind
    slides, and that is the only way to be sure you moved. *)
let pillar ~centre ~radius ~height ~material ~rotation =
  Room.regular_polygon ~center:centre ~radius ~sides:4 ~rotation ~height
    ~material

(** A low wall you can see over: a sill, a step, the lip of something. Reads as
    furniture without ever being identifiable as any particular furniture. *)
let sill ~height ~material a b = [ Room.wall ~height ~material a b ]
