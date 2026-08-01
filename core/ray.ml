(** Casting a ray against the wall segments of a {!Room}.

    {1 Ray versus segment}

    A grid raycaster steps a ray from cell to cell (the DDA). With arbitrary
    wall segments there is no grid to step through, so instead we intersect the
    ray with each wall directly and keep the ones it actually crosses.

    Write the ray as [origin + t*direction], [t >= 0], and a wall as
    [a + s*edge] with [edge = b - a] and [s] in [0, 1]. Setting them equal,

    {v   origin + t*direction = a + s*edge v}

    and solving with the 2-D cross product (see {!Vec.cross}) gives

    {v
      denom = direction x edge
      t     = (a - origin) x edge      / denom
      s     = (a - origin) x direction / denom
    v}

    The ray meets the wall when [denom] is non-zero (they are not parallel),
    [t > 0] (the wall is ahead), and [s] lies in [0, 1] (the crossing is between
    the wall's endpoints).

    {1 Why the distance has no fish-eye}

    [direction] is deliberately {e not} normalised: {!Viewport.ray_direction}
    builds it as [dir + right * k] with [dir] the unit view direction. Because
    [t] is measured in units of [direction], projecting the hit onto [dir]
    leaves exactly [t] — so [t] is the distance perpendicular to the camera
    plane, which is what the projection needs and what removes the fish-eye
    bulge. That argument is unchanged from the grid version; only the way we
    find [t] is different.

    {1 Seeing past a wall}

    Walls have different heights and the floor and ceiling are sloped, so a near
    wall does not necessarily hide what is behind it. The cast therefore keeps
    {e every} wall the ray crosses and returns them farthest-first, ready for
    {!Renderer} to paint back to front. *)

type hit = {
  distance : float;  (** perpendicular distance from the camera plane *)
  along : float;
      (** world distance from the wall's start [a] to the hit, for texturing *)
  wall : Room.wall;
  index : int;
      (** which of the room's walls it was. The wall itself is here for the
          renderer, which wants its material and its geometry; the index is for
          anything that has to name the wall afterwards — an index survives a
          {!World.replace_room} where a copy of the wall would go stale. *)
}

type opening = { distance : float; along : float; index : int }

(** Distance floor, so a player standing on a wall cannot divide by zero when
    the hit is turned into a wall height. *)
let min_distance = 1e-4

(* The cross product below is |direction| * |edge| * sin of the angle between
   them — an area, not an angle — so testing it against a fixed figure would
   read as a parallel test and behave as a length one: a wall short enough
   would fail it whatever angle the ray met it at, and go unrendered while
   Room.passable went on colliding with it. Scaled by both lengths, what is
   left is the sine, and the test says what it looks like it says. Neither
   length costs anything to come by: the caller has the edge's already, since
   Room.wall works it out once and keeps it, and the direction's is one square
   root for a whole ray rather than one per wall. *)
let parallel = 1e-12

let segment ~origin ~direction ~scale ~a ~edge ~length =
  let denom = Vec.cross direction edge in
  if Float.abs denom < parallel *. scale *. length then None
  else
    let ao = Vec.sub a origin in
    let t = Vec.cross ao edge /. denom in
    let s = Vec.cross ao direction /. denom in
    if t > min_distance && s >= 0. && s <= 1. then Some (t, s) else None

let cast (world : Room.t) ~(origin : Vec.t) ~(direction : Vec.t) =
  let hits = ref [] in
  let scale = Vec.length direction in
  for index = 0 to Room.wall_count world - 1 do
    let (w : Room.wall) = Room.wall_at world index in
    match
      segment ~origin ~direction ~scale ~a:w.a ~edge:w.edge ~length:w.length
    with
    | Some (t, s) ->
        hits :=
          { distance = t; along = s *. w.length; wall = w; index } :: !hits
    | None -> ()
  done;
  let hits = !hits in
  (* Farthest first: the order the painter's-algorithm renderer draws in. *)
  List.sort
    (fun (h1 : hit) (h2 : hit) -> Float.compare h2.distance h1.distance)
    hits

let openings (room : Room.t) ~origin ~direction =
  let scale = Vec.length direction in
  List.init (Room.threshold_count room) (fun index ->
      let (threshold : Room.threshold) = Room.threshold_at room index in
      match
        segment ~origin ~direction ~scale ~a:threshold.a ~edge:threshold.edge
          ~length:threshold.length
      with
      | Some (distance, s) ->
          Some { distance; along = s *. threshold.length; index }
      | None -> None)
  |> List.filter_map Fun.id
  |> List.sort (fun (a : opening) b -> Float.compare b.distance a.distance)

type step =
  | Wall of hit
  | Opening of opening
      (** One thing a ray met in a room, of whichever kind. Walls and doorways
          are found by two separate passes but have to be dealt with in one
          order, since each can stand in front of the other. *)

let step_distance = function Wall h -> h.distance | Opening o -> o.distance

(** Both lists arrive farthest-first, so one merge puts walls and thresholds
    into a single far-to-near stream without sorting either of them again.

    Far-to-near is the renderer's order — it paints back to front — and the
    reverse of it is what anything asking "what is the first thing out there"
    wants. Both read this. *)
let rec merge (walls : hit list) (openings : opening list) =
  match (walls, openings) with
  | [], rest -> List.map (fun o -> Opening o) rest
  | rest, [] -> List.map (fun h -> Wall h) rest
  | w :: ws, o :: os ->
      if o.distance > w.distance then Opening o :: merge walls os
      else Wall w :: merge ws openings

(** The closest wall along the ray, if it met one — the wall a solid-height
    caster would have stopped at. *)
let nearest (hits : hit list) =
  List.fold_left
    (fun (best : hit option) (hit : hit) ->
      match best with
      | Some b when b.distance <= hit.distance -> best
      | _ -> Some hit)
    None hits
