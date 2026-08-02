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

type leg = {
  key : string option;
  on_gaze : (bool -> unit) option;
  on_use : (Aim.spot -> unit) option;
  decals : t list;
  material : Material.t option;
  height : float option;
}

type corner = { at : Vec.t; leg : leg }

let bare l =
  Option.is_none l.key && Option.is_none l.on_gaze && Option.is_none l.on_use
  && Option.is_none l.material && Option.is_none l.height
  && match l.decals with [] -> true | _ -> false

let via ?key ?on_gaze ?on_use ?(decals = []) ?material ?height at =
  { at; leg = { key; on_gaze; on_use; decals; material; height } }

(* The segments a run describes, in the order they were written, each carrying
   the leg of the corner it leaves. Built here rather than read back off
   {!Room.path} because winding may reverse the run, and a leg has to stay with
   its own wall through that — see {!run}. *)
let legs ~closed corners =
  let rec go = function
    | { at = a; leg } :: ({ at = b; _ } :: _ as rest) -> (a, b, leg) :: go rest
    | [ { at = last; leg } ] when closed -> (
        match corners with [] -> [] | first :: _ -> [ (last, first.at, leg) ])
    | _ -> []
  in
  go corners

let run ?key ?(closed = false) ~height ~material corners =
  let points = List.map (fun c -> c.at) corners in
  (* Refused by the same call {!outline} and {!path} are refused by, so a run of
     two identical corners or a closed one of two says what it has always said,
     under a name a caller wrote. The walls it builds are discarded: they carry
     one material and one height, which is the whole of what this exists not to
     do. That is a handful of vectors normalised per run per frame, which is
     what {!outline} already pays to build the walls it keeps. *)
  ignore (Room.path ~closed ~height ~material points : Room.wall list);
  (match List.rev corners with
  | last :: _ when (not closed) && not (bare last.leg) ->
      invalid_arg
        "P.run: the last corner of an open run leaves no wall, so it can carry \
         nothing"
  | _ -> ());
  let written = legs ~closed corners in
  (* One reversal, and each wall flipped with it, so a leg stays on the wall its
     corner named however the run came out wound. Reversing the corners and
     letting the legs travel with them is the same thing off by one — a leg
     describes the wall it {e leaves}, and after a reversal that is the wall it
     arrives by. *)
  let laid =
    if twice_signed_area points >= 0. then written
    else
      (* Each wall flipped, and the traversal reversed — except that a closed
         run's last wall is the one that shuts the loop, and it shuts it at
         either winding, so it stays last. Reversing the whole list instead
         leaves the same walls rotated by one, which builds the same room and
         puts every leg on its neighbour. *)
      let flipped = List.map (fun (a, b, leg) -> (b, a, leg)) written in
      match List.rev flipped with
      | closing :: rest when closed -> rest @ [ closing ]
      | reversed -> reversed
  in
  E.fragment ?key
    (List.map
       (fun (a, b, leg) ->
         wall ?key:leg.key ?on_gaze:leg.on_gaze ?on_use:leg.on_use
           ~decals:leg.decals
           ~height:(Option.value leg.height ~default:height)
           ~material:(Option.value leg.material ~default:material)
           a b)
       laid)

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
