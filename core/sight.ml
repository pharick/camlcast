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
    from — so this agrees with the picture by construction, as {!decal_at}
    does below. That includes a sprite floating above the floor: the crosshair
    passing under its foot finds whatever is behind it.

    {!Room.sprite_column} wants the {e point's} offset from the sprite's
    centre. The point here is the crosshair, which sits at zero along [right]
    by definition of being the centre ray, so the offset is the sprite's own,
    negated: [pos - sprite.pos] and not the other way round. It was once
    written the other way round, and being a mirror image the bug never
    showed: the width test is on the absolute value, the row is a separate
    calculation, and every sprite anyone had pointed at was symmetric or solid
    to its edges. The cost was a sprite with a lopsided cut-out being pickable
    exactly where it is transparent. See
    [a_sprite_is_picked_where_it_is_drawn]. *)
let touches (pose : Player.t) (sprite : Room.sprite) ~floor ~z =
  let lateral =
    Vec.dot (Vec.sub pose.Player.pos sprite.Room.pos) pose.Player.right
  in
  match
    ( Room.sprite_column sprite ~lateral,
      Room.sprite_row sprite ~floor_z:floor ~z )
  with
  (* And the image is not transparent at that texel. *)
  | Some u, Some v -> snd (Image.sample sprite.Room.image ~u ~v) > 0
  | _ -> false

(* How far ahead a sprite stands, along the view. A sprite behind the player
    gives a negative distance; one almost on top of the player gives one too
    small to use. *)
let ahead (pose : Player.t) (sprite : Room.sprite) =
  Vec.dot (Vec.sub sprite.Room.pos pose.Player.pos) pose.Player.dir

(* Which of a wall's decals the crosshair is on, if any: the last one that
    covers the point and is not transparent there, since decals are drawn in
    order and the last is on top. Both halves of "covers" are {!Room}'s, so
    this agrees with the picture by construction — including the face, which
    {!Room.decal_column} refuses along with everything else it refuses. A mark
    hung on the {e far} face of a wall is not found from this side, however
    much of the wall is see-through. *)
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

let rec trace world ~room ~pose ~rise ~eye_z ~near ~crossed ~budget ~entered =
  let here = World.room world room in
  let origin = pose.Player.pos and direction = pose.Player.dir in
  let floor_at point = Plane.elevation (Room.floor_plane here) point in
  (* The height of the crosshair at a distance, and the point it is over.

     Nothing is accumulated across a doorway, and nothing needs to be. A link is
     a rigid motion, so the pose carried into the next room sits exactly as far
     from everything in it as the player really is: a distance measured there is
     already measured from the eye. (The renderer uses the same fact to put
     rooms several doorways deep into one depth buffer.) *)
  let z_at d = eye_z +. (rise *. d) in
  let point_at d = Vec.add origin (Vec.scale direction d) in
  (* A height, capped at the roof standing over that point. This one rule keeps
     every case below consistent with the picture: Renderer.draw_wall stops a
     wall, a leaf and a lintel strip alike at the ceiling and leaves the rows
     above them to draw_planes, so a height past the cap is a height nothing
     was drawn at. Taken per point rather than per room because a ceiling is a
     Plane and may be sloped, which caps each column at its own height. Open
     sky caps nothing, and the renderer does not cap against it either. *)
  let under_roof point z =
    match Room.ceiling here with
    | Room.Roof ceiling ->
        Float.min z (Plane.elevation ceiling.Room.plane point)
    | Room.Open _ -> z
  in
  let found d kind = Some { room; crossed; distance = d; pose; kind } in
  (* Everything in this room the ray could meet, nearest first. The walls and
     thresholds come merged by {!Ray.merge}; the sprites are not on that path
     at all — nothing casts against them — so they are placed by their own
     distance and the whole lot sorted once.

     The merged list is reversed rather than re-sorted, and the difference only
     matters for a tie. {!Ray.merge} hands back the renderer's order, far to
     near, and the renderer paints along it, so of two things at one distance
     the picture shows the {e last}. Reversing puts that one first here, and
     the stable sort below leaves it there. Sorting the merged list ascending
     instead would keep the tie in the order it was painted and take the one
     that was painted over: at the corner a jamb shares with its threshold, the
     crosshair would name the jamb while the frame showed the room beyond it,
     and a sprite standing plainly in that column would be unpickable.

     {b So the merge is not here for the order, although it reads that way.}
     The sort below re-establishes the order by itself. What it cannot
     re-establish is which of two equal distances comes first, and that is the
     one thing carried from {!Ray.merge} through the reverse and into a sort
     chosen to be stable. Interleaving two already-sorted lists to produce an
     order that is then produced again is genuinely redundant work: one
     traversal of a handful of steps, once a frame, this being one ray and not
     one per column.

     What the redundancy buys is that the tie stays {!Ray.merge}'s to decide.
     Concatenating the two lists instead would work in exactly one of the two
     possible orders: openings before walls passes every test in this
     repository, and walls before openings fails the corner the renderer draws.
     Picking the right one restates {!Ray.merge}'s rule at a call site, where
     the rule's other reader cannot see it, and the two readers disagreeing
     about a corner is exactly that bug. It is cheaper to keep paying the
     traversal. *)
  let met =
    List.map
      (fun step -> (Ray.step_distance step, Met step))
      (List.rev
         (Ray.merge
            (Ray.cast here ~origin ~direction)
            (Ray.openings here ~origin ~direction)))
  and billboards =
    List.filter_map
      (fun i ->
        let away = ahead pose (Room.sprite_at here i) in
        if away > Config.sprite_near_clip then Some (away, Billboard i)
        else None)
      (List.init (Room.sprite_count here) Fun.id)
  in
  (* Everything nearer than the doorway this room is being looked at through is
     dropped before any of it is considered. Along one ray the side of that
     doorway's plane a point is on is affine in the distance and changes sign
     exactly where the ray crosses it, so [near] is not an approximation of the
     opening but exact: past that distance is precisely what is beyond the
     doorway. A room that folds back on itself has walls of its own standing on
     this side of it, in space the player is really in, and those are what this
     removes. It is one filter for all three kinds — walls, doorways and
     sprites alike — which is the reason it belongs here and not in {!Ray}. *)
  (* The sort is stable, and stated so rather than relied on silently: the
     list above is already in the order this wants, and the sort only folds
     the billboards into it, so it must not disturb a tie it was handed. *)
  let candidates =
    List.stable_sort
      (fun (a, _) (b, _) -> Float.compare a b)
      (List.filter (fun (d, _) -> d > near) (met @ billboards))
  in
  let rec first = function
    | [] -> None
    | (d, Billboard i) :: rest ->
        let sprite = Room.sprite_at here i in
        if touches pose sprite ~floor:(floor_at sprite.Room.pos) ~z:(z_at d)
        then found d (Sprite { index = i })
        else first rest
    | (d, Met (Ray.Wall hit)) :: rest ->
        let wall = hit.Ray.wall in
        let where = point_at d in
        let foot = floor_at where in
        (* The wall's own height or the ceiling over it, whichever is lower. A
           roof that does not clear the floor there leaves the renderer nothing
           to draw at all; the guard below is the same one the renderer
           makes. *)
        let top = under_roof where (foot +. wall.Room.height) in
        let z = z_at d in
        if top > foot && z >= foot && z <= top then
          let along = hit.Ray.along and above = z -. foot in
          (* The face being looked at, taken from where the eye is and not
             from where the ray landed: the hit point is {e on} the wall,
             where which side it is on is a rounding error. *)
          let seen_from = Room.side_of wall origin in
          (* Asked of every wall in the band, and not only of the opaque ones,
             because a decal on a see-through wall is drawn and so has to be
             pickable. This is one ray a frame rather than one a column, so
             the extra work is affordable; the same budget covers the texel
             {!Material.opaque_at} fetches below. *)
          let decal = decal_at wall ~seen_from ~along ~above in
          if
            Material.opaque_at wall.Room.material ~along ~above || decal <> None
          then
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
        (* The doorway already being looked through, met again from behind.
           This guard exists as well as [near], because [near] stands at
           exactly this distance and would only remove it on an equality
           between two floats that went through a transform on their way
           here. *)
        first rest
    | (d, Met (Ray.Opening opening)) :: rest -> (
        let index = opening.Ray.index in
        let threshold = Room.threshold_at here index in
        let where = point_at d in
        let foot = floor_at where in
        let z = z_at d in
        (* Where a leaf or a lintel is met, in that surface's own coordinates.
           The renderer draws both as though they were walls: its [as_wall]
           hands them the opening's [along] and the floor under the hit point.
           So the texel this names is the texel that was painted here. *)
        let along = opening.Ray.along and above = z -. foot in
        (* Continue through the doorway, which is where the renderer has drawn
           the neighbour, whether the opening is bare or covered by something
           the eye sees through. With no portal, or no budget left, the result
           is the doorway itself. *)
        let onwards () =
          match World.portal world ~room ~threshold:index with
          | Some portal when budget > 0 ->
              trace world ~room:portal.World.to_room
                ~pose:
                  (Player.through portal.World.onto ~room:portal.World.to_room
                     pose)
                ~rise ~eye_z ~near:d ~crossed:(crossed + 1) ~budget:(budget - 1)
                ~entered:(Some portal.World.twin)
          | Some _ | None -> found d (Doorway { index })
        in
        if z > foot +. threshold.Room.height then
          (* Over the top of the opening. What is there is the strip of wall
             left standing above it, so this stops — but only if the strip
             exists and is solid where the crosshair is on it. The clear pane
             of a transom is looked through rather than at; its glazing bar is
             looked at. With no lintel the opening already runs the full
             height of the wall it was cut into, so there is no strip: what is
             up there is this room's own ceiling, which is not pickable and
             not a way through either, so the ray carries on and finds
             nothing. That matches what the renderer draws.

             A strip has a top of its own, and the ceiling may cut it shorter
             still. Past either, the lintel is behind the ray and this is the
             bare case again, whatever the strip is made of: the renderer
             paints neither the strip nor the neighbour up there. *)
          match threshold.Room.lintel with
          | Some l when z > under_roof where (foot +. l.Room.top) -> first rest
          | Some l when Material.opaque_at l.Room.material ~along ~above ->
              found d (Doorway { index })
          | Some _ -> onwards ()
          | None -> first rest
        else if z < foot then
          (* Under the opening, which is to say into the floor. Not something
             this picks; the ray carries on and finds nothing, which is the
             correct result. *)
          first rest
        else if z > under_roof where (foot +. threshold.Room.height) then
          (* Within the opening's own height, but over a ceiling that hangs
             below its head. The renderer draws that ceiling across these rows
             and neither the leaf nor the room beyond reaches them, so there is
             nothing here to pick and nothing to look through. *)
          first rest
        else
          (* The same rule the wall case above follows: a leaf stops the ray
             where its texel is solid, and does not where it is see-through.
             {!Room.shut} is not asked, because it is about the step and this
             is about the eye: a barred gate blocks the step and not the eye,
             and now blocks the eye only along its bars. *)
          match Room.leaf threshold with
          | Some material when Material.opaque_at material ~along ~above ->
              found d (Doorway { index })
          | Some _ | None -> onwards ())
  in
  first candidates

(* The default budget is the renderer's own, read from where the renderer reads
   it, for the same reason {!Config.sprite_near_clip} is read from there above:
   a cutoff the picture and the crosshair both have is one number and not two.
   A ray given any less stops short of things the frame plainly shows.
   Stopping on a doorway the player is looking straight through is the one way
   this module can be wrong without being wrong about anything it looked at. *)
let look ?(through = Config.max_portal_depth) world (player : Player.t) =
  let here = World.room world player.Player.room in
  let eye_z =
    Plane.elevation (Room.floor_plane here) player.Player.pos
    +. Config.eye_height
  in
  trace world ~room:player.Player.room ~pose:player
    ~rise:(Viewport.centre_rise ~pitch:player.Player.pitch)
    ~eye_z ~near:0. ~crossed:0 ~budget:through ~entered:None
