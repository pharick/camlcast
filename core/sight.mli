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

    - A {b wall}, where it is solid — the ray's height has to fall between the
      wall's foot on the sloped floor and its top, or it passes over or under
      it. Its top is its own height or the ceiling standing over that point,
      whichever is lower: {!Renderer} caps a wall at the ceiling and paints the
      ceiling above it, so a wall that runs on past one is a wall nobody can see
      up there. A room open to the sky caps nothing. Solid is asked of the
      {e texel} the crosshair is on and not of the wall as a whole, by
      {!Material.opaque_at}, because that is the question the renderer asks of
      each pixel: a texel it writes outright is one that hides what is behind,
      and a texel it merely blends — the gap in a grille, a pane of glass — is
      one the eye goes through. So the bars of a grille stop the ray and the
      holes between them do not, which is what the picture shows. One ray a
      frame can afford to look; the renderer, at one a column, routes a whole
      wall by {!Material.opaque} first and reaches the same answer.
    - A {b decal} on either sort of wall, where its image is not transparent
      there. This is what makes the see-through case an exception rather than a
      hole: the renderer draws a wall's decals whether or not the wall itself
      was drawn, so a mark painted on glass is visible, and what is visible is
      what can be picked. The wall it is on is what gets named — a decal is
      something on a wall, never a thing of its own.
    - A {b sprite}, where its image is not transparent there. Sprites are cut
      out against {!Image.clear} and mostly empty, so this is asked of the texel
      and not of the bounding box: the crosshair between a figure's arm and its
      side is looking at what is behind it. One nearer than
      {!Config.sprite_near_clip} is not reported at all — that is the distance
      the renderer stops drawing them at, read from the same place so that the
      two cannot drift apart, and a sprite the frame does not show is not
      something the crosshair should claim to be looking at.
    - A {b leaf} across a doorway, a doorway that {b leads nowhere yet}, and a
      {b lintel} over an opening — a ray meeting a doorway above its head meets
      the wall standing over it. The renderer draws a leaf and a lintel as
      though each were a wall, so both are read the same way a wall is: solid
      where the texel under the crosshair is, see-through where it is not. A
      barred gate stops the ray along its bars and not between them, the
      neighbouring room is drawn behind it either way, and the leaf still
      refuses the step it always did. What can be picked is what can be seen,
      not what can be walked through. An opening with {b no lintel} has no wall
      standing over it, and is no way through either — above its head is this
      room's own ceiling — so a ray up there meets nothing at all.
    - Running out of {b doorways to look through} — which by default is the
      renderer running out of them too, and the opening the ray stops at is the
      one the frame filled with haze.

    A doorway is bounded above the same way a wall is, and for the same reason.
    A lintel reaches its own [top] and no further, the ceiling may cut it or the
    opening itself shorter still, and past whichever of the three comes first
    the answer is the bare case again whatever hangs there: nothing to pick, and
    no way through. The renderer has drawn this room's own ceiling across those
    rows, and neither the strip nor the neighbour reaches them.

    {1 Depth and distance}

    [through] is how many doorways the ray may pass, and it defaults to
    {!Config.max_portal_depth} — the renderer's own budget, read from where the
    renderer reads it. That is the depth at which the two agree: at any less the
    crosshair reports the doorway while the picture shows what is beyond it, and
    there is no more to ask for, since past that budget the frame has stopped
    drawing rooms and filled the opening with haze. It is the same arrangement
    {!Config.sprite_near_clip} is under, and for the same reason — a cutoff both
    the picture and the crosshair have is one number and not two.

    A game wanting the ray to stop sooner says so. Zero confines it to the room
    the player is standing in; one is the room beyond the doorway in front of
    you and no further. But "how far away may a thing be worked from" is usually
    a question about {!t.distance} or {!t.crossed} rather than about the cast —
    those describe what was found, and refusing it is the game's to do, while a
    shortened ray refuses by not looking and cannot tell you what it declined to
    see.

    Distances need no adding up across rooms, and are not added up. A link is a
    rigid motion, so the pose carried into the next room sits exactly as far
    from everything in it as the player really does: a distance measured three
    rooms deep is already measured from the eye, and directly comparable with
    one measured here. The renderer's shared depth buffer works for the same
    reason. Height carries across the same way, which does assume the two floors
    meet at the doorway; {!World.seam_gap} is what says they do.

    The same fact is what makes the near edge of a doorway a single number.
    Through an opening the ray reports only what stands beyond that opening's
    own distance — nearer than it, along that ray, is the room the player is
    standing in, whichever room the geometry there belongs to. That matters
    because a world may fold back on itself: a neighbour whose doorway is set at
    the back of a recess has walls of its own, in its own coordinates, in front
    of it, and those are no more pickable through the doorway than the wall
    behind you is. It is the same rule and the same number the renderer clips
    its portals with, which is how "what can be picked is what can be seen"
    survives a room that folds. *)

(** What the ray met. Concrete because matching on it is the whole point — see
    the recipe in {{!page-"making-a-game"} Making a game}, and the [chalk] and
    [targets] demos for the two shapes it takes in practice. *)
type kind =
  | Wall of {
      index : int;
      along : float;
      z : float;
      facing : Room.side;
      decal : int option;
    }
      (** which wall of the room, how far along it from its [a] endpoint, how
          high above the floor at that point, which face of it was being looked
          at, and which of the wall's decals was under the crosshair if any —
          counted from the head of {!Room.type-wall}'s list, and the topmost
          where two overlap, since that is the one drawn last and so the one you
          can see.

          The first four are what {!Room.add_decal} wants: [index] says which
          wall, [along] and [z] are that wall's own coordinates and are what a
          {!Room.type-decal} is placed in, and [facing] is the face the mark
          should be on so that it is not also on the back. Nothing has to be
          converted, and nothing has to know the winding. *)
  | Sprite of { index : int }  (** which sprite of the room *)
  | Doorway of { index : int }
      (** which threshold: a shut door, one that leads nowhere, the lintel over
          an opening, or as far as [through] allowed *)

type t = {
  room : int;  (** the room the thing is in *)
  crossed : int;  (** how many doorways the ray went through to reach it *)
  distance : float;  (** how far, in cells *)
  pose : Player.t;
      (** the player, in {!t.room}'s own frame. The same eye, expressed where
          the thing it is looking at lives — which is what anything wanting to
          {e draw} the target needs, since the room it is in has its own
          coordinates. {!Viewport.sprite_box} takes this. Where the ray crossed
          no doorway it is the player unchanged. *)
  kind : kind;
}
(** One answer: where the thing is, how far, and what it is. *)

val look : ?through:int -> World.t -> Player.t -> t option
(** [look world player] is what [player] has the crosshair on, or [None] if the
    ray met nothing — aimed at the sky, or below every wall it passed, or out of
    doorways to look through.

    [through] is how many doorways the ray may pass; {!Config.max_portal_depth}
    by default, which is as far through them as the frame was drawn. Zero
    confines it to the room the player is standing in.

    The crosshair is the middle of the view, so this reads the player's [dir]
    and [pitch] and nothing about the window: the answer is the same whatever
    size it has been dragged to. *)
