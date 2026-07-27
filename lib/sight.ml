(** What the player is looking at: the crosshair's ray, traced through doorways
    until it meets something.

    This is the other half of a doorway. The renderer already looks through one
    — a column of a neighbouring room is drawn in the opening, transformed into
    its frame — and this asks the same question of the middle column and answers
    it in names rather than pixels: which room, and what in it. A game that lets
    the player study the room beyond a threshold, and act on what is in it
    without walking in, reads this.

    {1 What it reports, and what it does not}

    It answers with indices — a room, and a wall, sprite or threshold of that
    room — and never with the thing itself. An index is what survives
    {!World.replace_room}, which a game animating what the player is looking at
    will be doing every frame; a copy of the wall would be a picture of how it
    used to look. What those indices {e mean} is the game's own business: the
    engine has no notion of a sign, only of the sprite that happens to be one.

    It does not pick the floor or the ceiling. A ray aimed below the foot of
    every wall it meets finds nothing and says so.

    {1 What stops the ray}

    The same things that stop the eye, which is the point: what can be picked is
    what can be seen.

    - An {b opaque wall}, but only where the wall actually is — the ray's height
      has to fall between the wall's foot on the sloped floor and its top, or it
      passes over or under it. A see-through wall ({!Material.opaque} is false)
      does not stop it, exactly as it does not stop the renderer, and by the same
      whole-wall rule rather than texel by texel.
    - A {b sprite}, where its image is not transparent there. Sprites are cut
      out against {!Image.clear} and mostly empty, so this is asked of the texel
      and not of the bounding box: the crosshair between a figure's arm and its
      side is looking at what is behind it.
    - A {b shut door} ({!Room.shut}), a doorway that {b leads nowhere yet}, and
      the {b lintel} over any opening — a ray meeting a doorway above its head
      meets the wall standing over it.
    - Running out of {b doorways to look through}.

    {1 Depth and distance}

    [through] is how many doorways the ray may pass, and it defaults to one:
    §13.6 of the design wants the room directly beyond a threshold and no
    further, and a game that wants "not the room I am standing in" reads
    {!crossed}.

    Distances need no adding up across rooms, and are not added up. A link is a
    rigid motion, so the pose carried into the next room sits exactly as far
    from everything in it as the player really does: a distance measured three
    rooms deep is already measured from the eye, and directly comparable with
    one measured here. The renderer's shared depth buffer works for the same
    reason. Height carries across the same way, which does assume the two floors
    meet at the doorway; {!World.seam_gap} is what says they do. *)

type kind =
  | Wall of { index : int; along : float; z : float; decal : int option }
      (** which wall of the room, how far along it from its [a] endpoint, how
          high above the floor at that point, and which of the wall's decals was
          under the crosshair if any — counted from the head of
          {!Room.type-wall}'s list, and the topmost where two overlap, since
          that is the one drawn last and so the one you can see *)
  | Sprite of { index : int }  (** which sprite of the room *)
  | Doorway of { index : int }
      (** which threshold: a shut door, one that leads nowhere, the lintel over
          an opening, or as far as [through] allowed *)

type t = {
  room : int;  (** the room the thing is in *)
  crossed : int;  (** how many doorways the ray went through to reach it *)
  distance : float;  (** how far, in cells *)
  pose : Player.t;
      (** the player, in {!room}'s own frame. The same eye, expressed where the
          thing it is looking at lives — which is what anything wanting to
          {e draw} the target needs, since the room it is in has its own
          coordinates. {!Viewport.sprite_box} takes this. Where the ray crossed
          no doorway it is the player unchanged. *)
  kind : kind;
}

(** Does the crosshair fall on this sprite, at height [z] and distance [away]?

    A sprite is a billboard square to the view, [size] cells wide and tall,
    standing on the floor. The centre ray runs along [dir], so it is within the
    sprite's width exactly when the sprite's centre is within half a width to
    one side, measured along [right] — the same mapping the renderer inverts to
    place it on the screen, read the other way round. *)
let touches (pose : Player.t) (sprite : Room.sprite) ~floor ~z =
  let size = sprite.Room.size in
  let half = size /. 2. in
  let lateral = Vec.dot (Vec.sub sprite.Room.pos pose.Player.pos) pose.Player.right in
  Float.abs lateral <= half
  && z >= floor
  && z <= floor +. size
  &&
  (* And the image is not cut away there. *)
  let image = sprite.Room.image in
  let texel n fraction =
    Int.max 0 (Int.min (n - 1) (int_of_float (fraction *. float_of_int n)))
  in
  let u = texel image.Image.width ((lateral +. half) /. size)
  and v = texel image.Image.height ((floor +. size -. z) /. size) in
  snd (Image.sample image ~u ~v) > 0

(** How far ahead a sprite stands, along the view. Behind the player is a
    negative distance, and just about on top of them is a useless one. *)
let ahead (pose : Player.t) (sprite : Room.sprite) =
  Vec.dot (Vec.sub sprite.Room.pos pose.Player.pos) pose.Player.dir

(** Which of a wall's decals the crosshair is on, if any: the last one that
    covers the point and is not transparent there, since decals are drawn in
    order and the last is on top. Both halves of "covers" are {!Room}'s, so this
    agrees with the picture by construction rather than by care. *)
let decal_at (wall : Room.wall) ~along ~above =
  List.fold_left
    (fun (i, found) (d : Room.decal) ->
      ( i + 1,
        match (Room.decal_column d ~along, Room.decal_row d ~above) with
        | Some u, Some v when snd (Image.sample d.Room.image ~u ~v) > 0 ->
            Some i
        | _ -> found ))
    (0, None) wall.Room.decals
  |> snd

type candidate = Met of Ray.step | Billboard of int

let rec look world ~room ~pose ~rise ~eye_z ~crossed ~budget ~entered =
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
        if away > Ray.min_distance then Some (away, Billboard i) else None)
      (List.init (Array.length here.Room.sprites) Fun.id)
  in
  let candidates =
    List.sort (fun (a, _) (b, _) -> Float.compare a b) (met @ billboards)
  in
  let rec first = function
    | [] -> None
    | (d, Billboard i) :: rest ->
        let sprite = here.Room.sprites.(i) in
        if touches pose sprite ~floor:(floor_at sprite.Room.pos) ~z:(z_at d) then
          found d (Sprite { index = i })
        else first rest
    | (d, Met (Ray.Wall hit)) :: rest ->
        let wall = hit.Ray.wall in
        let foot = floor_at (point_at d) in
        let z = z_at d in
        if
          z >= foot
          && z <= foot +. wall.Room.height
          && Material.opaque wall.Room.material
        then
          let along = hit.Ray.along and above = z -. foot in
          found d
            (Wall
               {
                 index = hit.Ray.index;
                 along;
                 z = above;
                 decal = decal_at wall ~along ~above;
               })
        else first rest
    | (_, Met (Ray.Opening opening)) :: rest
      when entered = Some opening.Ray.index ->
        (* The doorway we are already looking through, met again from behind. *)
        first rest
    | (d, Met (Ray.Opening opening)) :: rest ->
        let index = opening.Ray.index in
        let threshold = here.Room.thresholds.(index) in
        let foot = floor_at (point_at d) in
        let z = z_at d in
        if z > foot +. threshold.Room.height then
          (* Over the top of the opening. What is there is the strip of wall
             left standing above it — so this stops, but only if there is one:
             an opening with no lintel runs the full height of the wall it was
             cut into and there is nothing up there to meet. *)
          if Option.is_some threshold.Room.lintel then found d (Doorway { index })
          else first rest
        else if z < foot then
          (* Under it, which is to say into the floor. Not something this picks;
             the ray carries on and finds nothing, which is the honest answer. *)
          first rest
        else if Room.shut threshold then found d (Doorway { index })
        else
          let portal = (World.portals world room).(index) in
          match portal with
          | Some portal when budget > 0 ->
              look world ~room:portal.World.to_room
                ~pose:
                  (Player.through portal.World.onto ~room:portal.World.to_room
                     pose)
                ~rise ~eye_z ~crossed:(crossed + 1) ~budget:(budget - 1)
                ~entered:(Some portal.World.twin)
          | Some _ | None -> found d (Doorway { index })
  in
  first candidates

(** What the player has the crosshair on, if anything.

    [through] is how many doorways the ray may pass through; one by default,
    which is the room beyond the doorway in front of you and no further. *)
let cast ?(through = 1) world (player : Player.t) =
  let here = World.room world player.Player.room in
  let eye_z =
    Plane.elevation here.Room.floor.Room.plane player.Player.pos
    +. Config.eye_height
  in
  look world ~room:player.Player.room ~pose:player
    ~rise:(Viewport.centre_rise ~pitch:player.Player.pitch)
    ~eye_z ~crossed:0 ~budget:through ~entered:None
