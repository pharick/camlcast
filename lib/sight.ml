(* Implementation of {!Camlcast.Sight}; the interface carries the prose. *)

type kind =
  | Wall of {
      index : int;
      along : float;
      z : float;
      facing : Room.side;
      decal : int option;
    }
  | Sprite of { index : int }
  | Doorway of { index : int }

type t = {
  room : int;
  crossed : int;
  distance : float;
  pose : Player.t;
  kind : kind;
}

(* Does the crosshair fall on this sprite, at height [z] over a floor at
    [floor]?

    The centre ray runs along [dir], so the crosshair is within the sprite's
    width exactly when the sprite's centre is within half a width to one side,
    measured along [right]. Both halves of "covers" are {!Room}'s — the same
    {!Room.sprite_column} and {!Room.sprite_row} the drawn rectangle is built
    from — so this agrees with the picture by construction rather than by care,
    the way {!decal_at} does below. That includes a sprite floating above the
    floor: the crosshair passing under its foot finds whatever is behind it. *)
let touches (pose : Player.t) (sprite : Room.sprite) ~floor ~z =
  let lateral =
    Vec.dot (Vec.sub sprite.Room.pos pose.Player.pos) pose.Player.right
  in
  match
    ( Room.sprite_column sprite ~lateral,
      Room.sprite_row sprite ~floor_z:floor ~z )
  with
  (* And the image is not cut away there. *)
  | Some u, Some v -> snd (Image.sample sprite.Room.image ~u ~v) > 0
  | _ -> false

(* How far ahead a sprite stands, along the view. Behind the player is a
    negative distance, and just about on top of them is a useless one. *)
let ahead (pose : Player.t) (sprite : Room.sprite) =
  Vec.dot (Vec.sub sprite.Room.pos pose.Player.pos) pose.Player.dir

(* Which of a wall's decals the crosshair is on, if any: the last one that
    covers the point and is not transparent there, since decals are drawn in
    order and the last is on top. Both halves of "covers" are {!Room}'s, so this
    agrees with the picture by construction rather than by care — including the
    face, which {!Room.decal_column} refuses along with everything else it
    refuses. A mark hung on the {e far} face of a wall is not something the
    crosshair finds from this side, however much of the wall you can see
    through. *)
let decal_at (wall : Room.wall) ~seen_from ~along ~above =
  List.fold_left
    (fun (i, found) (d : Room.decal) ->
      ( i + 1,
        match
          (Room.decal_column d ~seen_from ~along, Room.decal_row d ~above)
        with
        | Some u, Some v when snd (Image.sample d.Room.image ~u ~v) > 0 ->
            Some i
        | _ -> found ))
    (0, None) wall.Room.decals
  |> snd

type candidate = Met of Ray.step | Billboard of int

let rec trace world ~room ~pose ~rise ~eye_z ~crossed ~budget ~entered =
  let here = World.room world room in
  let origin = pose.Player.pos and direction = pose.Player.dir in
  let floor_at point = Plane.elevation here.Room.floor.Room.plane point in
  (* The height of the crosshair at a distance, and the point it is over.

     Nothing is accumulated across a doorway, and nothing needs to be. A link is
     a rigid motion, so the pose carried into the next room sits exactly as far
     from everything in it as the player really is: a distance measured there is
     already measured from the eye. (The renderer leans on the same fact to put
     rooms several doorways deep into one depth buffer.) *)
  let z_at d = eye_z +. (rise *. d) in
  let point_at d = Vec.add origin (Vec.scale direction d) in
  let found d kind = Some { room; crossed; distance = d; pose; kind } in
  (* Everything in this room the ray could meet, nearest first. The walls and
     thresholds come merged by {!Ray.merge}; the sprites are not on that path at
     all — nothing casts against them — so they are placed by their own distance
     and the whole lot sorted once. *)
  let met =
    List.map
      (fun step -> (Ray.step_distance step, Met step))
      (Ray.merge
         (Ray.cast here ~origin ~direction)
         (Ray.openings here ~origin ~direction))
  and billboards =
    List.filter_map
      (fun i ->
        let away = ahead pose here.Room.sprites.(i) in
        if away > Config.sprite_near_clip then Some (away, Billboard i)
        else None)
      (List.init (Array.length here.Room.sprites) Fun.id)
  in
  let candidates =
    List.sort (fun (a, _) (b, _) -> Float.compare a b) (met @ billboards)
  in
  let rec first = function
    | [] -> None
    | (d, Billboard i) :: rest ->
        let sprite = here.Room.sprites.(i) in
        if touches pose sprite ~floor:(floor_at sprite.Room.pos) ~z:(z_at d)
        then found d (Sprite { index = i })
        else first rest
    | (d, Met (Ray.Wall hit)) :: rest ->
        let wall = hit.Ray.wall in
        let foot = floor_at (point_at d) in
        let z = z_at d in
        if z >= foot && z <= foot +. wall.Room.height then
          let along = hit.Ray.along and above = z -. foot in
          (* The face being looked at, taken from where the eye is and not from
             where the ray landed — the hit point is {e on} the wall, where
             which side it is on is a rounding error. *)
          let seen_from = Room.side_of wall origin in
          (* Asked of every wall in the band, and not only of the opaque ones,
             because a mark on a see-through wall is drawn and so has to be
             pickable. This is one ray a frame rather than one a column, so the
             extra work is not worth avoiding. *)
          let decal = decal_at wall ~seen_from ~along ~above in
          if Material.opaque wall.Room.material || decal <> None then
            found d
              (Wall
                 {
                   index = hit.Ray.index;
                   along;
                   z = above;
                   facing = seen_from;
                   decal;
                 })
          else first rest
        else first rest
    | (_, Met (Ray.Opening opening)) :: rest
      when entered = Some opening.Ray.index ->
        (* The doorway we are already looking through, met again from behind. *)
        first rest
    | (d, Met (Ray.Opening opening)) :: rest -> (
        let index = opening.Ray.index in
        let threshold = here.Room.thresholds.(index) in
        let foot = floor_at (point_at d) in
        let z = z_at d in
        (* On through the doorway, which is where the renderer has drawn the
           neighbour — whether the opening is bare or wearing something the eye
           goes through. With nowhere to go the doorway itself is the answer. *)
        let onwards () =
          match (World.portals world room).(index) with
          | Some portal when budget > 0 ->
              trace world ~room:portal.World.to_room
                ~pose:
                  (Player.through portal.World.onto ~room:portal.World.to_room
                     pose)
                ~rise ~eye_z ~crossed:(crossed + 1) ~budget:(budget - 1)
                ~entered:(Some portal.World.twin)
          | Some _ | None -> found d (Doorway { index })
        in
        if z > foot +. threshold.Room.height then
          (* Over the top of the opening. What is there is the strip of wall
             left standing above it — so this stops, but only if there is one
             and it is solid: an opening with no lintel runs the full height of
             the wall it was cut into and there is nothing up there to meet, and
             a glazed transom is looked through rather than at. *)
          match threshold.Room.lintel with
          | Some l when Material.opaque l.Room.material ->
              found d (Doorway { index })
          | Some _ -> onwards ()
          | None -> first rest
        else if z < foot then
          (* Under it, which is to say into the floor. Not something this picks;
             the ray carries on and finds nothing, which is the honest answer. *)
          first rest
        else
          (* The same rule the wall above follows: what is opaque stops the ray,
             and what you can see through does not. {!Room.shut} is not asked,
             because it is about the step and this is about the eye. *)
          match Room.leaf threshold with
          | Some material when Material.opaque material ->
              found d (Doorway { index })
          | Some _ | None -> onwards ())
  in
  first candidates

let look ?(through = 1) world (player : Player.t) =
  let here = World.room world player.Player.room in
  let eye_z =
    Plane.elevation here.Room.floor.Room.plane player.Player.pos
    +. Config.eye_height
  in
  trace world ~room:player.Player.room ~pose:player
    ~rise:(Viewport.centre_rise ~pitch:player.Player.pitch)
    ~eye_z ~crossed:0 ~budget:through ~entered:None
