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
      does not stop it, exactly as it does not stop the renderer, and by the
      same whole-wall rule rather than texel by texel.
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
    - An {b opaque leaf} across a doorway, a doorway that {b leads nowhere yet},
      and an {b opaque lintel} over an opening — a ray meeting a doorway above
      its head meets the wall standing over it. A leaf or a lintel of a
      see-through material stops the ray no more than a see-through wall does:
      the renderer draws the neighbouring room behind both, so the ray goes on
      into it, and the leaf still refuses the step it always did. What can be
      picked is what can be seen, not what can be walked through.
    - Running out of {b doorways to look through}.

    {1 Depth and distance}

    [through] is how many doorways the ray may pass, and it defaults to one: the
    room directly beyond a threshold and no further, and a game that wants "not
    the room I am standing in" reads {!t.crossed}.

    Distances need no adding up across rooms, and are not added up. A link is a
    rigid motion, so the pose carried into the next room sits exactly as far
    from everything in it as the player really does: a distance measured three
    rooms deep is already measured from the eye, and directly comparable with
    one measured here. The renderer's shared depth buffer works for the same
    reason. Height carries across the same way, which does assume the two floors
    meet at the doorway; {!World.seam_gap} is what says they do. *)

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

    [through] is how many doorways the ray may pass; one by default, which is
    the room beyond the doorway in front of you and no further. Zero confines it
    to the room the player is standing in.

    The crosshair is the middle of the view, so this reads the player's [dir]
    and [pitch] and nothing about the window: the answer is the same whatever
    size it has been dragged to. *)
