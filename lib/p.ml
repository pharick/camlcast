(* Implementation of {!Camlcast.P}; the interface carries the prose. *)

open Camlcast_core
module E = Camlcast_loom.Element

type t = Prim.t E.t
type surface = Prim.surface
type ceiling = Prim.ceiling

let floor ?plane material = { Prim.plane; material }
let roof ?plane ?headroom material = Prim.Roof { plane; headroom; material }
let open_sky sky = Prim.Sky sky
let world ~atmosphere children = E.prim ~children (Prim.World { atmosphere })
let spawn at = E.prim (Prim.Spawn at)
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

let camera ?(pitch = 0.) ~pos ~angle () =
  E.prim (Prim.Camera { pos; angle; pitch })

let hud children = E.prim ~children Prim.Hud

let rect ?key ?(alpha = 255) ~x ~y ~w ~h ~color () =
  E.prim ?key (Prim.Rect { x; y; w; h; color; alpha })

let line ?key ~x0 ~y0 ~x1 ~y1 ~color () =
  E.prim ?key (Prim.Line { x0; y0; x1; y1; color })

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

(* The same arithmetic Room.doorway does to place its opening. Written once
   there and called here rather than restated, so the two cannot disagree about
   where a doorway is.

   Restating it here is what the sentence above is guarding against, and the
   restatement that looks right is the wrong one: out from the middle rather
   than in from the ends. On an oblique wall the two put a full-width opening
   6.21e-17 apart, and a description building its own jambs from those points
   gets back the invisible blocker Room.cut_points exists to avoid. *)
let opening ~width a b =
  let edge = Vec.sub b a in
  let span = Vec.length edge in
  (* The same three refusals {!Room.doorway} makes, worded with the name of the
     function that was actually called. Without them the division below is a
     nan, and a nan propagates: it surfaces as a transform that will not invert
     or a doorway whose ends meet nothing, far from the pair of points that was
     wrong. The conditions are negated so a nan argument is refused with the
     degenerate ones. The span is measured as {!Room.doorway} measures it,
     {!Vec.normalizable} rather than merely positive, so a span only a
     subnormal long is refused here, in these words, and not by some later
     construction in the geometry. *)
  if not (Vec.normalizable span) then
    invalid_arg "P.opening: no wall to cut an opening into";
  if not (Float.is_finite width && width > 0.) then
    invalid_arg "P.opening: an opening has to have a width";
  if not (width <= span) then
    invalid_arg "P.opening: wider than the wall it is cut into";
  Room.cut_points ~width a b

let through ~from:(a1, a2) ~into:(b1, b2) plane =
  Plane.through (Transform.between ~a1 ~a2 ~b1 ~b2) plane

(* Twice the signed area, by the shoelace sum. Positive is the winding
   Room.rectangle produces. Room.rectangle is the one boundary the engine
   documents as impossible to get wrong, so it is the definition to measure
   against rather than a rule restated here that could drift from it. *)
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

type leg = {
  key : string option;
  on_gaze : (bool -> unit) option;
  on_use : (Aim.spot -> unit) option;
  decals : t list;
  material : Material.t option;
  height : float option;
}

type corner = { at : Vec.t; leg : leg }

let corner ?key ?on_gaze ?on_use ?(decals = []) ?material ?height at =
  { at; leg = { key; on_gaze; on_use; decals; material; height } }

(* The segments an outline describes, in the order they were written, each
   carrying the leg of the corner it leaves — including the one back to the
   first corner, because an outline is closed. Built here rather than read back
   off {!Room.path} because winding may reverse the run, and a leg has to stay
   with its own wall through that — see {!laid_outline}. *)
let legs corners =
  let rec go = function
    | { at = a; leg } :: ({ at = b; _ } :: _ as rest) -> (a, b, leg) :: go rest
    | [ { at = last; leg } ] -> (
        match corners with [] -> [] | first :: _ -> [ (last, first.at, leg) ])
    | [] -> []
  in
  go corners

let corners = List.map corner

let polygon ~center ~radius ~sides ~rotation =
  corners (Room.polygon_corners ~center ~radius ~sides ~rotation)

(* The legs of a closed outline, wound so the room is on the inside.

   Validated by {!Room.path}, so a run of two identical corners, or one of two
   corners at all, gets the established message under a name a caller wrote. The
   walls that call builds are discarded: they carry one material and one height,
   and avoiding that limit is the point of an outline. The cost is a handful of
   vectors normalised per room per frame, which is what the call pays anyway for
   the walls it hands back. *)
let laid_outline ~height ~material corners =
  let points = List.map (fun c -> c.at) corners in
  ignore (Room.path ~closed:true ~height ~material points : Room.wall list);
  let written = legs corners in
  if twice_signed_area points >= 0. then written
  else
    let flipped = List.map (fun (a, b, leg) -> (b, a, leg)) written in
    match List.rev flipped with
    | closing :: rest -> rest @ [ closing ]
    | [] -> []

let room ?key ?name ?outline ?height ?material ~floor ~ceiling children =
  let boundary_walls =
    match (outline, height, material) with
    | None, _, _ -> []
    | Some corners, Some height, Some material ->
        List.map
          (fun (a, b, leg) ->
            wall ?key:leg.key ?on_gaze:leg.on_gaze ?on_use:leg.on_use
              ~decals:leg.decals
              ~height:(Option.value leg.height ~default:height)
              ~material:(Option.value leg.material ~default:material)
              a b)
          (laid_outline ~height ~material corners)
    | Some _, _, _ ->
        invalid_arg
          "P.room: an outline needs a height and a material for its legs"
  in
  E.prim ?key
    ~children:(boundary_walls @ children)
    (Prim.Room { name; floor; ceiling; height })

let block ?key ~height ~material corners =
  E.fragment ?key
    (List.map
       (fun (a, b, leg) ->
         wall ?key:leg.key ?on_gaze:leg.on_gaze ?on_use:leg.on_use
           ~decals:leg.decals
           ~height:(Option.value leg.height ~default:height)
           ~material:(Option.value leg.material ~default:material)
           a b)
       (laid_outline ~height ~material corners))

(* A door is made once and cut once. The identity is what a {!connect} joins by,
   and it is why this is a value rather than an element: two rooms that are
   joined refer to the same door, and an element rebuilt every frame has nothing
   for them to refer to. Made at the top level, for the reason
   {!Camlcast_loom.Element.declare} is. *)
type door = {
  id : int;
  width : float;
  clearance : float;
  door_name : string option;
}

(* Atomic rather than a plain ref, because this is the only mutable state the
   description layer has and the compiler floor is a multicore runtime. Two
   domains describing worlds at once would read and write a ref between them
   and mint one id twice, and a duplicate id is a connection joining the wrong
   doorway: a world that assembles, is not the one that was written, and says
   nothing. *)
let fresh_door = Atomic.make 0

let door ?name ~width ~clearance () =
  (* [fetch_and_add] hands back the value it replaced, so the [+ 1] keeps the
     ids the sequence from one they have always been. *)
  let id = Atomic.fetch_and_add fresh_door 1 + 1 in
  { id; width; clearance; door_name = name }

let cut ?key ?leaf ?lintel ?on_gaze ?on_use d ~along =
  E.prim ?key
    (Prim.Door
       {
         id = d.id;
         along;
         width = d.width;
         clearance = d.clearance;
         name = d.door_name;
         leaf;
         lintel;
         reacts = reacts ?on_gaze ?on_use ();
       })

let connect a b = E.prim (Prim.Connect (a.id, b.id))
