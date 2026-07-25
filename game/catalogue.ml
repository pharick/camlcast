(** The kinds of room the house is built from.

    Six of them, and the weights matter more than the shapes. Every prototype
    but the closet has more exits than it has ways in, so each room the
    generator places offers more than one way on; the branching factor is the
    average of [exits - 1] over the weights, and if it strays far above one the
    house grows faster than anyone can walk through it. The closet is what holds
    it down, and it is the one weight worth being careful with: at five it made
    nearly a quarter of the house dead ends, and between those and the loops a
    walk kept arriving back where it had been. At two the average is about one
    and a half, which keeps the three-doorway ball around the player at a dozen
    rooms and still means most doorways open onto somewhere new.

    Ceiling heights differ from one kind to the next while the doorways do not,
    so walking through a door changes the height of the room around you without
    changing the door. That is the only unsettling thing in here that was put
    there on purpose. *)

open Raycaster

let rect x0 y0 x1 y1 =
  [| Vec.make x0 y0; Vec.make x1 y0; Vec.make x1 y1; Vec.make x0 y1 |]

(* [walls] defaults to plain plaster; the two prototypes with walls long enough
   to recede into the fog take the striated one instead, which is the whole
   reason it exists. *)
let base ?(walls = Assets.Surfaces.wall) ~name ~outline ~exits ~height ~fittings
    ~weight () =
  {
    Prototype.name;
    outline;
    exits;
    height;
    walls;
    floor = Assets.Surfaces.floor;
    ceiling = Assets.Surfaces.ceiling;
    fittings;
    weight;
  }

(** The Hallway itself: long, narrow, and a door at either end. The one room in
    the house that tells you which way you were going. *)
let corridor =
  base ~walls:Assets.Surfaces.corridor ~name:"corridor"
    ~outline:(rect 0. 0. 9. 2.4) ~exits:[| 1; 3 |] ~height:2.6 ~weight:6
    ~fittings:(fun rng ->
      (* A sill along one wall, sometimes. Enough to give the long walls
         something to measure against as they recede into the fog. *)
      if Rng.chance rng 0.5 then
        let y = if Rng.chance rng 0.5 then 0.4 else 2.0 in
        let start = 2. +. (Rng.unit rng *. 3.) in
        Prototype.sill ~height:0.35 ~material:Assets.Surfaces.wall
          (Vec.make start y)
          (Vec.make (start +. 2.2) y)
      else [])
    ()

(** A room rather than a passage, and the only one with a door in every wall. *)
let chamber =
  base ~name:"chamber" ~outline:(rect 0. 0. 7. 7.) ~exits:[| 0; 1; 2; 3 |]
    ~height:3.4 ~weight:3
    ~fittings:(fun rng ->
      if Rng.chance rng 0.7 then
        Prototype.pillar
          ~centre:
            (Vec.make (2.8 +. (Rng.unit rng *. 1.4)) (2.8 +. (Rng.unit rng *. 1.4)))
          ~radius:0.55 ~height:3.4 ~material:Assets.Surfaces.wall
          ~rotation:(Rng.unit rng *. Float.pi /. 2.)
      else [])
    ()

(** A junction: small, square, three ways on. *)
let landing =
  base ~name:"landing" ~outline:(rect 0. 0. 4.5 4.5) ~exits:[| 0; 1; 2 |]
    ~height:2.6 ~weight:3
    ~fittings:(fun rng ->
      if Rng.chance rng 0.4 then
        Prototype.pillar ~centre:(Vec.make 2.25 2.25) ~radius:0.4 ~height:0.5
          ~material:Assets.Surfaces.wall ~rotation:0.4
      else [])
    ()

(** An L. Two doors, and you cannot see one from the other, which is the only
    place in the house where a room hides part of itself. *)
let annex =
  base ~name:"annex"
    ~outline:
      [|
        Vec.make 0. 0.;
        Vec.make 6. 0.;
        Vec.make 6. 3.;
        Vec.make 3. 3.;
        Vec.make 3. 6.5;
        Vec.make 0. 6.5;
      |]
    ~walls:Assets.Surfaces.corridor ~exits:[| 0; 4 |] ~height:2.8 ~weight:4
    ~fittings:(fun rng ->
      if Rng.chance rng 0.5 then
        Prototype.sill ~height:0.4 ~material:Assets.Surfaces.wall
          (Vec.make 4.2 2.4) (Vec.make 5.4 2.4)
      else [])
    ()

(** Nine sides, so none of its walls is square to any other. Every doorway out
    of it turns the world by twenty degrees, and after two of them nobody knows
    which way they came in. *)
let rotunda =
  let sides = 9 and radius = 5. in
  base ~name:"rotunda"
    ~outline:
      (Array.init sides (fun k ->
           let angle = float_of_int k *. 2. *. Float.pi /. float_of_int sides in
           Vec.make (radius *. cos angle) (radius *. sin angle)))
    ~exits:[| 0; 2; 4; 5; 7 |] ~height:3.8 ~weight:1
    ~fittings:(fun rng ->
      if Rng.chance rng 0.8 then
        Prototype.pillar ~centre:(Vec.make 0. 0.) ~radius:0.8 ~height:3.8
          ~material:Assets.Surfaces.wall ~rotation:(Rng.unit rng *. Float.pi)
      else [])
    ()

(** A dead end. Nothing in it, nothing beyond it, and without enough of them
    the house would branch faster than it could be walked. *)
let closet =
  base ~name:"closet" ~outline:(rect 0. 0. 3. 2.4) ~exits:[| 0 |] ~height:2.2
    ~weight:2
    ~fittings:(fun _ -> [])
    ()

let all = [ corridor; chamber; landing; annex; rotunda; closet ]

(** Where a run begins. A corridor, because the book does. *)
let entrance = corridor
