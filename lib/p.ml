(* Implementation of {!Camlcast.P}; the interface carries the prose. *)

open Camlcast_core
module E = Camlcast_loom.Element

type t = Prim.t E.t

let floor = Room.floor
let roof = Room.roof
let open_sky = Room.open_sky

let world ~atmosphere ~spawn children =
  E.prim ~children (Prim.World { atmosphere; spawn })

let room ?key ~name ~floor ~ceiling children =
  E.prim ?key ~children (Prim.Room { name; floor; ceiling })

let reacts ?on_gaze ?on_use () = { Prim.on_gaze; on_use }

let wall ?key ?on_gaze ?on_use ?(decals = []) ~height ~material a b =
  E.prim ?key ~children:decals
    (Prim.Wall { a; b; height; material; reacts = reacts ?on_gaze ?on_use () })

let decal ?key ?facing ?glow ~along ~z ~half_width ~half_height image =
  E.prim ?key
    (Prim.Decal
       (Room.decal ?facing ?glow ~along ~z ~half_width ~half_height image))

let sprite ?key ?on_gaze ?on_use ?base ?glow ~size ~image pos =
  E.prim ?key
    (Prim.Sprite
       (Room.sprite ?base ?glow ~size ~image pos, reacts ?on_gaze ?on_use ()))

let camera ?(pitch = 0.) ~room ~pos ~angle () =
  E.prim (Prim.Camera { room; pos; angle; pitch })

let hud children = E.prim ~children Prim.Hud

let rect ?key ?(alpha = 255) ~x ~y ~w ~h ~color () =
  E.prim ?key (Prim.Rect { x; y; w; h; color; alpha })

let bar ?key ~x ~y ~w ~h ~fraction ~color () =
  E.prim ?key (Prim.Bar { x; y; w; h; fraction; color })

let text ?key ?(color = Color.rgb 255 255 255) ~font ~x ~y body =
  E.prim ?key (Prim.Text { x; y; text = body; color; font })

let picture ?key ?tint ~x ~y image =
  E.prim ?key (Prim.Picture { x; y; image; tint })

let highlight ?(color = Color.rgb 255 255 255) () =
  E.prim (Prim.Highlight color)

let crosshair ?(color = Color.rgb 255 255 255) () =
  E.prim (Prim.Crosshair color)

let cursor = E.prim Prim.Cursor
let finish = E.prim Prim.Finish
let link here there = E.prim (Prim.Link { here; there })

(* Room.wall is private, so its parts can be read back out — which is what lets
   the helpers below hand their work to the same Wall primitive a game writes by
   hand, decals and all, rather than needing a second kind of wall for walls the
   engine built. *)
let of_wall (w : Room.wall) = wall ~height:w.height ~material:w.material w.a w.b

(* No [?key], unlike its neighbours: every argument here is labelled, so there
   is no positional one for an optional to be erased against. Nothing is lost by
   it — a polygon is walls and holds no state, and a game that needs to key a
   group of them can say so with {!Camlcast_loom.Element.fragment} directly. *)
let polygon ~center ~radius ~sides ~rotation ~height ~material =
  E.fragment
    (List.map of_wall
       (Room.regular_polygon ~center ~radius ~sides ~rotation ~height ~material))

(* The same arithmetic Room.doorway does to place its opening. Written once
   there and read back here rather than restated, so the two cannot disagree
   about where a doorway is.

   Which this said before it was true. It restated the arithmetic, and restated
   the version Room.doorway had already stopped using — out from the middle
   rather than in from the ends — so on an oblique wall the two put a full-width
   opening 6.21e-17 apart, and a description building its own jambs from these
   points got back the invisible blocker Room.cut_points exists to avoid. *)
let opening ~width a b =
  let edge = Vec.sub b a in
  let span = Vec.length edge in
  (* The same three refusals {!Room.doorway} makes, in the words of the function
     that was actually called. Without them the division below is a nan, and a
     nan travels: it comes back as a transform that will not invert or a doorway
     whose ends meet nothing, a long way from the pair of points that was
     wrong. Negated, so a nan argument is refused with the degenerate ones. *)
  if not (Float.is_finite span && span > 0.) then
    invalid_arg "P.opening: no wall to cut an opening into";
  if not (Float.is_finite width && width > 0.) then
    invalid_arg "P.opening: an opening has to have a width";
  if not (width <= span) then
    invalid_arg "P.opening: wider than the wall it is cut into";
  Room.cut_points ~width a b

let through ~from:(a1, a2) ~into:(b1, b2) plane =
  Plane.through (Transform.between ~a1 ~a2 ~b1 ~b2) plane

let threshold ?key ?door ?lintel ?on_gaze ?on_use ~name ~height a b =
  E.prim ?key
    (Prim.Threshold
       ( Room.threshold ~name ~height ?door ?lintel a b,
         reacts ?on_gaze ?on_use () ))

(* Twice the signed area, by the shoelace sum. Positive is the winding
   Room.rectangle produces, and Room.rectangle is the one boundary the engine
   documents as impossible to get wrong — so it is the definition to measure
   against rather than a rule restated here and left to drift from it. *)
let twice_signed_area points =
  let rec go total = function
    | (p : Vec.t) :: ((q : Vec.t) :: _ as rest) ->
        go (total +. (p.x *. q.y) -. (q.x *. p.y)) rest
    | [ last ] -> (
        match points with
        | (first : Vec.t) :: _ ->
            total +. (last.Vec.x *. first.y) -. (first.x *. last.Vec.y)
        | [] -> total)
    | [] -> total
  in
  go 0. points

let wound points =
  if twice_signed_area points < 0. then List.rev points else points

let path ?key ~height ~material points =
  E.fragment ?key
    (List.map of_wall
       (Room.path ~closed:false ~height ~material (wound points)))

let outline ?key ~height ~material points =
  let wound = wound points in
  (* Room.path does the closing and the refusing — fewer than three corners, two
     the same in a row — so those stay refused in one place and in the words
     they have always been refused in. *)
  E.fragment ?key
    (List.map of_wall (Room.path ~closed:true ~height ~material wound))

let doorway ?key ?door ?on_gaze ?on_use ~name ~width ~opening ~height ~material
    a b =
  let jambs, threshold =
    Room.doorway ?door ~name ~width ~opening ~height ~material a b
  in
  (* The handlers go on the opening and not on the jambs either side of it: what
     a player aims at to work a door is the door. The key goes on the fragment
     over the pair of them, because what a game rearranges is the doorway and
     there is no one primitive here that is it. *)
  E.fragment ?key
    (List.map of_wall jambs
    @ [ E.prim (Prim.Threshold (threshold, reacts ?on_gaze ?on_use ())) ])
