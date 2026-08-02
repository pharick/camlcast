(** Draws one frame into a {!Framebuffer}, a column at a time, entirely in
    software.

    {1 Why software}

    The floor and ceiling are inclined {!Plane}s, so the surface a pixel shows —
    and how far away it is — differs from one pixel to the next even within a
    row. That cannot be expressed by blitting whole textures, so every pixel is
    computed by hand and the finished buffer is scaled up to the window by the
    GPU in a single copy.

    {1 A column}

    For each screen column we take the ray's horizontal direction and:

    - {b Cast the floor, and the ceiling or sky.} For every pixel below the
      horizon, {!Plane.view_distance} gives the distance to the floor, its
      {!Texture} sampled in world space and faded by fog. Above the horizon it
      is the ceiling the same way, or — where the level has no roof — the
      {!Sky}, whose colour depends only on the direction looked in.

    - {b Paint the opaque walls.} {!Ray.cast} returns every wall the ray
      crosses, farthest first. Each solid wall is drawn from its foot on the
      sloped floor up to its height (capped at the ceiling), its {!Texture}
      sampled per pixel and tinted by its {!Material}, with any
      {!Room.type-decal}s — paintings, posters — blended over it. Painting
      far-to-front lets a near wall cover the ones behind it while a tall wall
      still shows over a short one. Each wall records its distance into a
      per-pixel depth, so the passes that follow know exactly which pixels it
      covers — a short wall only the pixels of its own strip.

    {1 A frame}

    A column alone cannot place things that span columns or see past each other,
    so a frame is two phases: first the backgrounds and opaque walls above, per
    column; then everything translucent — the {b sprites} ({!Room.type-sprite}s
    drawn as billboards that always face the player) and the
    {b see-through walls} (grilles and windows, {!Texture.alpha}) — composited
    over them.

    The translucent things are drawn in one combined pass, farthest first, each
    still hidden — per pixel — by a nearer opaque wall, so a short wall in front
    hides only the lower part of a sprite or window behind it and the top still
    shows. They cannot each have their own pass: a sprite in front of a window
    has to cover it while a sprite behind one has to show through it, so sprites
    and see-through walls are sorted {e together} by depth.

    A sprite nearer than {!Config.sprite_near_clip} is not drawn, in this room
    or through a doorway. Its box is one over its distance, so close up there is
    nothing useful left to draw — and {!Sight} reads the same constant, so what
    the crosshair can pick stays what the frame actually shows.

    {1 Portals}

    What makes a doorway fit the painter's algorithm above is that
    {b it is just one more thing the ray meets}. A {!Room.type-threshold} joins
    the same far-to-near stream as the walls, so walls of this room beyond it
    are painted first and get covered, and walls nearer are painted after and
    cover it.

    A threshold with a leaf across it draws as a wall of that leaf's texture;
    which those are is {!Room.leaf}'s to say, and it says a closed door and
    nothing else. One with nothing across it recurses: the neighbouring room is
    drawn in the same column, with the camera and the ray carried into its frame
    by the link's {!Transform} and the row clip narrowed to the opening. Above
    either, the {!Room.type-lintel} fills the strip of wall left standing over
    the gap — which is why an opening lower than the wall around it wants one:
    the strip is what is drawn over the top of a closed door. An opening with no
    lintel is one that reaches the wall's own top, so there is no strip and
    nothing is drawn; the rows above it are the room's own ceiling, the same
    thing that stands over a wall which stops below one.

    Opacity decides the rest, exactly as it does for a wall. A leaf or a lintel
    you can see through is {e both}: the neighbour is drawn behind it first and
    the leaf composited over it in the translucent pass, which is what makes a
    barred door a door you can look through. Being able to see through it says
    nothing about being able to walk through it — {!Room.shut} asks whether a
    leaf hangs there and not what it is made of, so a grille stops the step the
    same way a grille wall does.

    The rigid transform is horizontal, so eye height, projection and horizon are
    unchanged inside a portal and the {!Viewport} is not rebuilt; and because it
    preserves distance, a distance measured several rooms deep is directly
    comparable with everything else in the shared depth buffer. Rooms may form a
    cycle, so it is {!Config.max_portal_depth} and nothing else that ends the
    recursion — out of budget, the opening is filled with the world's
    {!Atmosphere.haze}.

    Everything drawn inside a doorway is clipped to that doorway's own distance,
    which is the same one measure the whole picture is already in. The camera
    arrives behind the neighbour's copy of the opening, so a room that folds
    back on itself — one whose doorway is set at the back of a recess — has
    walls of its own in front of it, standing in space the player is really in;
    without the clip they would be painted over the room the doorway was a
    window onto. {!Sight} clips its ray at the same distance by the same rule,
    which is what keeps what can be picked and what is drawn the same thing.

    The sky belongs to the room. A room open to the sky takes its azimuth from
    the {e nested} direction, so it has its own sun: with per-room local
    coordinates there is no world compass to appeal to.

    Sprites and see-through walls still have to be composited together at the
    end, so a fragment records which room it was seen in, the pose it was seen
    from, and a mask — the single column and row range of the doorway it was
    seen through. Because that is per column and not a bounding box, the clip is
    exact: a sprite behind a doorway is trimmed to the doorway's outline.

    One consequence of the mask being per column is that a room seen through a
    doorway contributes one fragment per column of that doorway. They are three
    words each and each draws a single column, so the cost is in the columns
    either way; but it does mean that if a chain of doorways ever loops back to
    a room already on it, that room's sprites are composited once for the near
    view and again for the far one. Both are honest views of the same objects at
    different distances, so they are left alone rather than suppressed.

    {1 Size}

    Casting per pixel costs one division per pixel, so the buffer is rendered no
    taller than {!Config.max_render_height} and the GPU scales it to the actual
    window. That keeps the cost — and so the frame rate — steady at any window
    size, and covers resizing and fullscreen with it. *)

val internal_size : width:int -> height:int -> int * int
(** [internal_size ~width ~height] is the buffer size to render a window of that
    size at: the window scaled down by a whole number until it is within
    {!Config.max_render_height}, so its aspect ratio — and with it the
    {!Viewport} resize rules — is preserved.

    A game needs this only to know what coordinates its overlay is drawing in,
    which are the buffer's and not the window's. *)

val fit :
  Tsdl.Sdl.renderer -> Framebuffer.t ref -> (unit, [ `Msg of string ]) result
(** Size the buffer to the window it will be shown in, at {!internal_size} of
    it: a new buffer when the window has changed shape, and the one that is
    already there when it has not.

    {!render} does this itself, so a loop that only draws never needs to call
    it. It is here for a loop that needs the buffer's size {e settled} before it
    draws — which means a loop that puts a pointer into the buffer's
    coordinates, because the cursor is reported in the window's. Fit at the top
    of the frame and the buffer converted against is the buffer drawn into;
    leave it to {!render} at the end and the first frame after a resize measures
    the cursor by the old layout and draws the new one. *)

val render :
  Tsdl.Sdl.renderer ->
  Framebuffer.t ref ->
  ?overlay:(Framebuffer.t -> unit) ->
  World.t ->
  Player.t ->
  (unit, [ `Msg of string ]) result
(** Render a frame: fit the buffer to the current window, fill it, upload it and
    present. The window size is read fresh every frame, so resizing and
    fullscreen need no special handling — which is why the buffer arrives in a
    [ref], to be replaced when the window changes shape.

    [overlay] is handed the finished world, still in the buffer and not yet on
    the screen, and may draw over it — an indicator, a message, a fade. {!Paint}
    and {!Font} are what to draw with: they clip to the buffer and clamp what
    they are given, and a game's overlay is exactly the caller neither of those
    can be assumed of. {!Framebuffer.set} and {!Framebuffer.blend} are
    underneath them and take a pixel that is already on the buffer and channels
    that are already in range. It runs after everything in the world and is
    clipped by nothing, so whatever it draws is in front of all of it. The
    buffer's size is the one in it, and it changes with the window.

    This is the loop's seam and not a game's: {!Engine.run} calls it once a
    frame, and a game reaches the overlay through the callback of the same name
    on {!Engine.type-game}. It is the only thing here that touches SDL. *)

val draw_frame : Framebuffer.t -> World.t -> Player.t -> unit
(** Fill the whole framebuffer from the player's point of view: the backgrounds
    and opaque walls per column, then the sprites and see-through walls
    composited over them together, farthest first.

    Pure array writes, no SDL calls — those are {!render}'s. That is what makes
    it the way to test drawing: pair it with {!Framebuffer.offscreen}, which
    needs no window, and read the result back with {!Framebuffer.pixel}. Every
    rendering test in this repository is written that way. *)
