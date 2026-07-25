(** The house, and how it gets built.

    A run starts with one corridor and no doorways at all. Every time the player
    walks from one room into another, {!horizon} makes sure that every room
    within {!Raycaster.Config.max_portal_depth} doorways of them is finished —
    every wall that could become a doorway has become one, and every doorway
    leads somewhere. Beyond that ring the house does not exist, and the player
    can never see far enough to find out.

    {1 Why three}

    Exactly as deep as the renderer looks. It draws the player's room, then
    three rooms of doorways past it, and at the third the budget runs out and
    the opening fills with haze. So a room three doorways away must already have
    its doorways {e cut}, or the player would watch a blank wall turn into a
    door as they walked towards it. It need not have anything behind them: at
    that depth they read as openings onto black, and they resolve into rooms as
    you approach. The generator being one room ahead of the renderer costs
    nothing and is invisible; being one room behind it would be the only thing
    anyone noticed.

    {1 Why nothing has to fit}

    There is no packing problem here, no floor plan, and no test for whether two
    rooms overlap — because there is nowhere for them to overlap {e in}. Each
    room is authored in its own coordinates and joined to its neighbours only by
    a {!Raycaster.Transform} derived from one shared doorway. Lay the whole
    house out on a single sheet of paper and rooms would sit on top of each
    other everywhere; nothing ever does that, so nothing ever notices.

    Which is also why {!loop_chance} costs almost nothing. Joining a doorway to
    a room already a few doorways behind produces a corridor that returns you
    somewhere it could not possibly reach — four left turns and you are not
    where you started, or you are somewhere you have been but arrived facing the
    wrong way. In a world with a global frame that is a bug to be prevented.
    Here there is no global frame for it to contradict, so it is simply what the
    house does. *)

open Raycaster

(** How often an opened doorway leads back into the house instead of on to
    somewhere new, and how far back it is willing to reach. Low, because a loop
    is only unsettling if most doorways are not one: at a half the house stops
    being somewhere you are getting lost in and becomes a small set of rooms you
    are being shuffled between. *)
let loop_chance = 0.18

let loop_radius = 3

(** How often a doorway has a leaf hung across it. You still walk through a
    closed door — walking into it is how it opens — but you cannot see through
    it, so a quarter of the house is rooms you have to go into to find out
    about. Any higher and the corridors stop reading as corridors; any lower and
    the one closed door you do meet reads as a special case rather than as how
    the place is. *)
let shut_chance = 0.25

type room = {
  prototype : Prototype.t;
  opened : (int * string * bool) list;
      (** which boundary segments have been cut open, what each doorway was
          called, and whether a leaf was hung across it — {b in the order they
          were cut}, since that is the order of the room's thresholds and every
          [twin] index in the world points into it *)
  fittings : Room.wall list;
      (** drawn once when the room is first built and kept, so that rebuilding
          the room to add a doorway does not silently rearrange its furniture *)
}

type t = {
  world : World.t;
  rooms : room array;  (** parallel to [world]'s rooms *)
  rng : Rng.t;
  frontier : int;
      (** how many walls in the whole house could still become doorways.

          The house is infinite only for as long as this is not zero, and it can
          reach zero: a loop consumes {e two} openings and gives nothing back,
          and a dead end consumes the one it was entered by. Left to chance the
          first few rooms are where it happens — with two openings in the world
          a single loop can join them to each other and seal the house at two
          rooms, which is not a house, it is a box.

          So it is counted rather than hoped for. {!open_one} will not loop
          unless there is slack to spare, and when there is none it builds
          something with a way on out of it. Both rules together mean this never
          falls below one, whatever the dice do. *)
}

(** How much slack the house insists on keeping. One would be enough to make the
    invariant hold; two costs nothing and keeps the generator from repeatedly
    scraping along the bottom, where every choice is forced and the house fills
    up with dead ends. *)
let slack = 2

(** A doorway is named after the boundary segment it was cut into, which makes
    the name unique within its room without having to check. *)
let exit_name k = Printf.sprintf "exit%d" k

(** Build the {!Raycaster.Room.t} a piece of state describes: the boundary with
    its opened segments replaced by jambs, the doorways in the order they were
    cut, and the fittings.

    Walls may be rearranged freely — nothing outside the room refers to one —
    but the thresholds are strictly append-only, because a portal's [twin] is a
    bare index into them and every one of them would mean a different doorway if
    anything shifted. *)
let assemble state =
  let cuts =
    List.map
      (fun (k, name, shut) ->
        ( k,
          Prototype.cut state.prototype k ~name
            ?door:(if shut then Some Assets.Surfaces.door else None)
            () ))
      state.opened
  in
  let opened = List.map fst cuts in
  let solid =
    Array.to_list (Prototype.boundary state.prototype)
    |> List.filteri (fun k _ -> not (List.mem k opened))
  in
  Room.make
    ~thresholds:(List.map (fun (_, (_, t)) -> t) cuts)
    ~floor:(Prototype.floor state.prototype)
    ~ceiling:(Prototype.ceiling state.prototype)
    (solid @ List.concat_map (fun (_, (jambs, _)) -> jambs) cuts @ state.fittings)

(** The boundary segments of [room] that are still solid wall. *)
let unopened house room =
  let state = house.rooms.(room) in
  Array.to_list state.prototype.Prototype.exits
  |> List.filter (fun k ->
         not (List.exists (fun (j, _, _) -> j = k) state.opened))

let neighbours house room =
  Array.to_list (World.portals house.world room)
  |> List.filter_map (Option.map (fun (p : World.portal) -> p.World.to_room))

(** Cut one of [room]'s remaining walls open, and return what the doorway is
    called. It leads nowhere until something links it. *)
let cut_exit house ~room ~exit ~shut =
  let state = house.rooms.(room) in
  let name = exit_name exit in
  let state = { state with opened = state.opened @ [ (exit, name, shut) ] } in
  let rooms = Array.copy house.rooms in
  rooms.(room) <- state;
  ( {
      house with
      rooms;
      frontier = house.frontier - 1;
      world = World.open_doorway house.world ~room ~opened:(assemble state);
    },
    name )

(** Hang a new room off a doorway that leads nowhere yet. Which kind of room is
    drawn from the catalogue by weight, and which of {e its} walls becomes the
    way in is drawn from its own exits — so the same prototype is entered from a
    different side each time and never reads as the same room twice.

    [least] is how many exits the new room must have. One lets a dead end be
    built, which is most of the time; two is what the house asks for when it has
    no slack left, since a room whose only doorway is the one you came in by
    gives the frontier nothing back. *)
let graft house ~room ~name ~least ~shut =
  let prototype =
    Rng.weighted house.rng
      (Catalogue.all
      |> List.filter (fun (p : Prototype.t) ->
             Array.length p.Prototype.exits >= least)
      |> List.map (fun (p : Prototype.t) -> (p.Prototype.weight, p)))
  in
  let entrance = Rng.pick house.rng prototype.Prototype.exits in
  let back = exit_name entrance in
  let state =
    {
      prototype;
      opened = [ (entrance, back, shut) ];
      fittings = prototype.Prototype.fittings house.rng;
    }
  in
  let label =
    Printf.sprintf "%s#%d" prototype.Prototype.name (Array.length house.rooms)
  in
  let world, index = World.add_room house.world ~name:label (assemble state) in
  {
    house with
    world = World.link world (room, name) (index, back);
    rooms = Array.append house.rooms [| state |];
    (* Every wall of it but the one it was entered by is a way on. *)
    frontier =
      house.frontier + Array.length prototype.Prototype.exits - 1;
  }

(** Every room within {!loop_radius} doorways that still has a wall it could
    open, paired with which wall. Includes the room the doorway is being cut
    from: a corridor whose far door leads back into its own near one is the
    purest thing this generator can produce. *)
let candidates house room =
  let seen = Hashtbl.create 32 in
  let rec sweep layer depth found =
    if depth > loop_radius then found
    else
      match List.filter (fun i -> not (Hashtbl.mem seen i)) layer with
      | [] -> found
      | layer ->
          List.iter (fun i -> Hashtbl.replace seen i ()) layer;
          let here =
            List.concat_map
              (fun i -> List.map (fun exit -> (i, exit)) (unopened house i))
              layer
          in
          sweep (List.concat_map (neighbours house) layer) (depth + 1)
            (here @ found)
  in
  sweep [ room ] 0 []

(** Join a doorway back into the house instead of on to somewhere new. Needs a
    room within reach with a wall to spare; the spare walls the house has may
    all be further off than {!loop_radius}, in which case this falls through to
    a fresh room — a doorway that has been cut has to lead somewhere. *)
let weld house ~room ~name ~least ~shut =
  match candidates house room with
  | [] -> graft house ~room ~name ~least ~shut
  | options ->
      let there, exit = Rng.pick house.rng (Array.of_list options) in
      let house, back = cut_exit house ~room:there ~exit ~shut in
      { house with world = World.link house.world (room, name) (there, back) }

(** Decide what is behind a wall, and cut it open.

    The two guards here are what keep the house infinite. A loop spends two of
    the house's remaining openings and returns none, so it is only allowed while
    there are more than that to spare; and when the house is down to its last
    one, whatever gets built behind it must have a way on out of it. Between
    them the frontier can never reach zero, and a house whose frontier reaches
    zero is finished — not in the sense of complete, in the sense of over. *)
let open_one house ~room ~exit =
  (* Decided once, before the wall is cut, because both sides of a doorway have
     to agree: a leaf on one side and an opening on the other would be a door
     you could see through from behind. *)
  let shut = Rng.chance house.rng shut_chance in
  let house, name = cut_exit house ~room ~exit ~shut in
  let least = if house.frontier < slack then 2 else 1 in
  if house.frontier >= slack && Rng.chance house.rng loop_chance then
    weld house ~room ~name ~least ~shut
  else graft house ~room ~name ~least ~shut

(** Open every wall of [room] that could be a doorway, and put something behind
    each. Idempotent: a room with nothing left to open is returned untouched,
    which is what makes {!horizon} cheap to run on every room change.

    The remaining walls are recounted after each one, because a doorway that
    loops back may have spent one of {e this} room's walls on the way. *)
let rec expand house room =
  match unopened house room with
  | [] -> house
  | exit :: _ -> expand (open_one house ~room ~exit) room

(** Finish every room within {!Raycaster.Config.max_portal_depth} doorways of
    the player. Rooms further out may exist — expanding the outermost ring
    creates them — but nothing has been decided about them, and nothing can see
    that far. *)
let horizon house (player : Player.t) =
  let seen = Hashtbl.create 64 in
  let rec sweep house layer depth =
    if depth > Config.max_portal_depth then house
    else
      match List.filter (fun i -> not (Hashtbl.mem seen i)) layer with
      | [] -> house
      | layer ->
          List.iter (fun i -> Hashtbl.replace seen i ()) layer;
          let house = List.fold_left expand house layer in
          sweep house (List.concat_map (neighbours house) layer) (depth + 1)
  in
  sweep house [ player.Player.room ] 0

(** One corridor, no doorways, and a seed. Everything else follows from walking.

    The same seed builds the same house, which is the only reason any of this
    can be tested: a run is a value, not an event. *)
let start ~seed =
  let rng = Rng.make seed in
  let prototype = Catalogue.entrance in
  let state =
    { prototype; opened = []; fittings = prototype.Prototype.fittings rng }
  in
  let label = prototype.Prototype.name ^ "#0" in
  let spawn =
    let n = Array.length prototype.Prototype.outline in
    Array.fold_left Vec.add (Vec.make 0. 0.) prototype.Prototype.outline
    |> fun sum -> Vec.scale sum (1. /. float_of_int n)
  in
  {
    world =
      World.make
        ~rooms:[ (label, assemble state) ]
        ~links:[] ~atmosphere:Assets.Surfaces.air ~spawn:(label, spawn);
    rooms = [| state |];
    rng;
    frontier = Array.length prototype.Prototype.exits;
  }
