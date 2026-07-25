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
}

type opening = { distance : float; along : float; index : int }

(** Distance floor, so a player standing on a wall cannot divide by zero when
    the hit is turned into a wall height. *)
let min_distance = 1e-4

let segment ~origin ~direction ~a ~edge =
  let denom = Vec.cross direction edge in
  if Float.abs denom < 1e-12 then None
  else
    let ao = Vec.sub a origin in
    let t = Vec.cross ao edge /. denom in
    let s = Vec.cross ao direction /. denom in
    if t > min_distance && s >= 0. && s <= 1. then Some (t, s) else None

let cast (world : Room.t) ~(origin : Vec.t) ~(direction : Vec.t) =
  let hits =
    Array.fold_left
      (fun acc (w : Room.wall) ->
        match segment ~origin ~direction ~a:w.a ~edge:w.edge with
        | Some (t, s) ->
            { distance = t; along = s *. w.length; wall = w } :: acc
        | None -> acc)
      [] world.walls
  in
  (* Farthest first: the order the painter's-algorithm renderer draws in. *)
  List.sort
    (fun (h1 : hit) (h2 : hit) -> Float.compare h2.distance h1.distance)
    hits

let openings (room : Room.t) ~origin ~direction =
  Array.to_list
    (Array.mapi
       (fun index (threshold : Room.threshold) ->
         match segment ~origin ~direction ~a:threshold.a ~edge:threshold.edge with
         | Some (distance, s) ->
             Some { distance; along = s *. threshold.length; index }
         | None -> None)
       room.thresholds)
  |> List.filter_map Fun.id
  |> List.sort (fun (a : opening) b -> Float.compare b.distance a.distance)

(** The closest wall along the ray, if it met one — the wall a solid-height
    caster would have stopped at. *)
let nearest (hits : hit list) =
  List.fold_left
    (fun (best : hit option) (hit : hit) ->
      match best with
      | Some b when b.distance <= hit.distance -> best
      | _ -> Some hit)
    None hits
