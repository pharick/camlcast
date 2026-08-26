(* Implementation of {!Camlcast_edit.Session}; the interface carries the
   prose. *)

open Camlcast
open Camlcast_core

type panel = Plan | Graph | Tree

type t = {
  mutable showing : bool;
  mutable panel : panel;
  mutable room : int;
  patch : Patch.t;
  tree : Tree.t;
  revise : Revise.t;
  (* The last frame the reconciler committed. Kept because the panels are drawn
     from it -- a plan and a graph are both readings of what the description
     came to -- and it is only in hand inside the patch. *)
  mutable frame : Watch.node list;
  (* What is picked on the plan, and -- while a corner is being held -- which
     end of what is travelling with the mouse. *)
  mutable selected : Sheet.hit;
  mutable dragging : (Camlcast_loom.Path.t * int) option;
  (* Which of the selection's fields the keys act on, and -- in the graph --
     the first of the two doorways a join needs. *)
  mutable field : int;
  mutable joining : Graph.door option;
  link : Link.t;
  mutable said : string option;
}

let create ?read ?write () =
  let read = Option.value read ~default:(Revise.from_disk ()) in
  let write = Option.value write ~default:(Revise.to_disk ()) in
  {
    showing = false;
    panel = Plan;
    room = 0;
    patch = Patch.create ();
    tree = Tree.create ();
    revise = Revise.create ~read ~write;
    frame = [];
    selected = Sheet.Nothing;
    dragging = None;
    field = 0;
    joining = None;
    link = Link.create read;
    said = None;
  }

let watch t =
  Watch.make
    ~trace:(Tree.watch t.tree)
      (* The patch is where a frame is known to have stood. A trace handler
         cannot tell: it is called while the walk is happening, and a walk can
         still be refused after its last event. A patch is called once, with
         the committed forest, after everything that could refuse the frame
         except the host itself -- which is the closest thing to "this frame
         happened" the runtime offers, and near enough that Trace.Refused
         covers the rest. *)
    ~patch:(fun forest ->
      Tree.commit t.tree;
      t.frame <- forest;
      Patch.apply t.patch forest)
    ()

let open_ t = t.showing <- true
let close t = t.showing <- false
let is_open t = t.showing
let panel t = t.panel

let show t panel =
  t.panel <- panel;
  t.showing <- true

let room t = t.room
let selected t = t.selected
let said t = t.said
let show_room t room = t.room <- Int.max 0 room
let pending t = Patch.count t.patch
let undo t = Revise.undo t.revise

(* {1 Picking things up} *)

let pointer t = if t.showing then P.cursor else Element.empty

(* The two spans holding one point of a call: the [which]-th argument written
   positionally and as numbers. A wall's ends are its two, a sprite's place is
   its one. Read off Span rather than through Field because what a drag needs
   is exactly a pair, and Field flattens a pair into two fields to be typed
   over one at a time. *)
let point_spans (call : Span.call) which =
  List.nth_opt
    (List.filter_map
       (fun (argument : Span.argument) ->
         match (argument.label, argument.value) with
         | None, Span.Numbers [ x; y ] -> Some (x, y)
         | _ -> None)
       call.arguments)
    which

let source_of t (item : Sheet.item) = Link.locate t.link item.at

(* Where the [which]-th point of a thing is written, when it is written at all.
   Both questions below are this one: colouring a plan asks it of the first
   point, and taking hold of a corner asks it of that corner's.

   Link.editable is the wrong question here and was asked first: it answers
   whether {e anything} about a call can be changed, and a wall whose ends are
   worked out from a function still has a height written down. Coloured by that
   answer the wall looks draggable; dragged, it moves on the plan and writes
   nothing. The predicate has to be about the point, not the call. *)
let written_point t (item : Sheet.item) which =
  match source_of t item with
  | Link.Found source -> point_spans source.Link.call which
  | Link.Unpositioned | Link.Unreadable _ -> None

(* Whether the plan should offer to drag this. Asked of the source rather than
   guessed from the frame: a wall can carry a position and still have its ends
   worked out, which is exactly the case worth drawing differently. *)
let draggable t item = written_point t item 0 <> None

let item_at t path =
  List.find_opt
    (fun (item : Sheet.item) -> Camlcast_loom.Path.equal item.Sheet.path path)
    (Sheet.items t.frame ~room:t.room)

(* While the mouse is held, the patch says where the thing is; the description
   still says where it was. Recomputed from the pointer every frame rather than
   from the last position, so a drag cannot accumulate error. *)
let drag_to t (item : Sheet.item) which (at : Vec.t) =
  match item.Sheet.what with
  | Prim.Wall { a; b; _ } ->
      let a, b = if which = 0 then (at, b) else (a, at) in
      Patch.move_wall t.patch item.Sheet.path ~a ~b
  | Prim.Sprite _ | Prim.Spawn _ -> Patch.move_sprite t.patch item.Sheet.path at
  | _ -> ()

(* And when it is let go, the same point is written to the file. Both
   coordinates in one splice: two edits applied one at a time would have the
   second land in bytes the first had already moved. *)
let write_point t (item : Sheet.item) which (at : Vec.t) =
  match source_of t item with
  | Link.Unpositioned ->
      t.said <- Some "no position -- build with ppx_camlcast to edit"
  | Link.Unreadable message -> t.said <- Some message
  | Link.Found source -> (
      match point_spans source.Link.call which with
      | None -> t.said <- Some "that point is not written as numbers"
      | Some (x, y) -> (
          match
            Revise.apply t.revise ~path:source.Link.file
              [ (x, Span.number at.Vec.x); (y, Span.number at.Vec.y) ]
          with
          | Ok () ->
              Patch.clear t.patch;
              t.said <-
                Some
                  (Printf.sprintf "%s:%d written" source.Link.file
                     source.Link.line)
          | Error (`Msg message) -> t.said <- Some message))

(* {1 Changing one field, and joining two doorways} *)

let fields t =
  match t.selected with
  | Sheet.Nothing -> []
  | Sheet.Corner { item; _ } | Sheet.Body item -> (
      match source_of t item with
      | Link.Found source -> (
          match Link.parsed t.link source.Link.file with
          | Ok parsed -> Field.of_call parsed source.Link.call
          | Error _ -> [])
      | Link.Unpositioned | Link.Unreadable _ -> [])

let file_of t =
  match t.selected with
  | Sheet.Nothing -> None
  | Sheet.Corner { item; _ } | Sheet.Body item -> (
      match source_of t item with
      | Link.Found source -> Some source.Link.file
      | Link.Unpositioned | Link.Unreadable _ -> None)

(* A tenth of a cell, which is the size of thing this is for: a height, a glow,
   a sprite's size. A coordinate is dragged rather than nudged. *)
let step = 0.1

let nudge t by =
  match (List.nth_opt (fields t) t.field, file_of t) with
  | Some ({ Field.value = Field.Number number; _ } as field), Some path -> (
      match Field.write field (Field.Number (number +. (by *. step))) with
      | Error (`Msg message) -> t.said <- Some message
      | Ok edit -> (
          match Revise.apply t.revise ~path [ edit ] with
          | Ok () ->
              t.said <- Some (Printf.sprintf "%s %s" field.Field.label path)
          | Error (`Msg message) -> t.said <- Some message))
  | Some { Field.label; _ }, _ ->
      t.said <- Some (label ^ " holds a name; there is nothing to nudge")
  | None, _ -> t.said <- Some "nothing selected to change"

(* The world's own call, which is where a connection is appended: a connection
   is a child of the world rather than of either room, so joining two rooms and
   unjoining them touches neither. *)
let world_call t =
  List.find_map
    (fun (node : Watch.node) ->
      match node.Camlcast_loom.Host.prim with
      | Prim.World _ -> (
          match Link.locate t.link node.Camlcast_loom.Host.at with
          | Link.Found source -> Some source
          | _ -> None)
      | _ -> None)
    t.frame

let join t (door : Graph.door) =
  match t.joining with
  | None ->
      t.joining <- Some door;
      t.said <-
        Some
          (Printf.sprintf "%s picked; choose the doorway it leads to"
             (Option.value door.Graph.name ~default:"that doorway"))
  | Some first when first.Graph.id = door.Graph.id ->
      t.joining <- None;
      t.said <- Some "let go of that doorway"
  | Some first -> (
      t.joining <- None;
      match world_call t with
      | None -> t.said <- Some "cannot find the world this is written in"
      | Some source -> (
          match Link.parsed t.link source.Link.file with
          | Error (`Msg message) -> t.said <- Some message
          | Ok parsed -> (
              match Graph.join parsed ~world:source.Link.call first door with
              | Error (`Msg message) -> t.said <- Some message
              | Ok edit -> (
                  match
                    Revise.apply t.revise ~path:source.Link.file [ edit ]
                  with
                  | Ok () ->
                      t.said <-
                        Some
                          (Printf.sprintf "joined, in %s -- rebuild to walk it"
                             source.Link.file)
                  | Error (`Msg message) -> t.said <- Some message))))

(* Moving what is picked into a component of its own.

   Named after the element's own key rather than typed, which is what makes
   this a gesture at all: the engine reports keys as places on a keyboard and
   has no text input, so a name asked for would have to be spelled out of
   scancodes. A key is already there, already unique among its siblings, and
   already the name the author chose for the thing -- P asks for one on
   anything that can be rearranged, so anything worth extracting has one. *)
let extract t =
  let keyed =
    match t.selected with
    | Sheet.Nothing -> None
    | Sheet.Corner { item; _ } | Sheet.Body item ->
        let spelling = Camlcast_loom.Path.to_debug_string item.Sheet.path in
        (* The last step's key, which is what a path prints in brackets. *)
        let rec last_bracket index found =
          if index >= String.length spelling then found
          else if spelling.[index] = '[' then
            match String.index_from_opt spelling index ']' with
            | Some stop ->
                last_bracket (stop + 1)
                  (Some (String.sub spelling (index + 1) (stop - index - 1)))
            | None -> found
          else last_bracket (index + 1) found
        in
        Option.map (fun key -> (item, key)) (last_bracket 0 None)
  in
  match keyed with
  | None ->
      t.said <- Some "give it a ~key and it can be moved into a file of its own"
  | Some (item, key) -> (
      match source_of t item with
      | Link.Unpositioned -> t.said <- Some "no position -- nothing to move"
      | Link.Unreadable message -> t.said <- Some message
      | Link.Found source -> (
          match Link.parsed t.link source.Link.file with
          | Error (`Msg message) -> t.said <- Some message
          | Ok parsed -> (
              match Scaffold.extract_call parsed source.Link.call ~name:key with
              | Error (`Msg message) -> t.said <- Some message
              | Ok (file, edit) -> (
                  match
                    Revise.write t.revise ~path:file.Scaffold.path
                      file.Scaffold.source
                  with
                  | Error (`Msg message) -> t.said <- Some message
                  | Ok () -> (
                      match
                        Revise.apply t.revise ~path:source.Link.file [ edit ]
                      with
                      | Ok () ->
                          t.said <-
                            Some
                              (Printf.sprintf "moved into %s" file.Scaffold.path)
                      | Error (`Msg message) -> t.said <- Some message)))))

let take_back t =
  match Revise.undo t.revise with
  | Error (`Msg message) -> t.said <- Some message
  | Ok None -> t.said <- Some "nothing left to take back"
  | Ok (Some (Revise.Restored path)) -> t.said <- Some (path ^ " put back")
  | Ok (Some (Revise.Created path)) ->
      t.said <- Some (path ^ " was made by this session and is left where it is")

let title t =
  match t.panel with
  | Plan -> Printf.sprintf "plan  room %d" t.room
  | Graph -> "graph"
  | Tree -> "tree"

let plain = Color.rgb 205 210 220
let quiet = Color.rgb 120 128 140
let loud = Color.rgb 255 190 90

let lines t =
  if not t.showing then []
  else
    let header =
      [
        { Panel.text = title t; color = loud };
        {
          Panel.text =
            (match pending t with
            | 0 -> "saved"
            | 1 -> "1 edit not written"
            | n -> Printf.sprintf "%d edits not written" n);
          color = (if pending t = 0 then quiet else loud);
        };
      ]
    in
    (* What is picked, where it was written, and what about it can be changed.
       Shown under the panel's own two lines because an overlay drawn over a
       game is the only place a game developer is looking. *)
    let chosen =
      match t.selected with
      | Sheet.Nothing -> []
      | Sheet.Corner { item; _ } | Sheet.Body item ->
          let where =
            match source_of t item with
            | Link.Found source ->
                Printf.sprintf "%s:%d"
                  (Filename.basename source.Link.file)
                  source.Link.line
            | Link.Unpositioned -> "written where nothing says"
            | Link.Unreadable message -> message
          in
          [
            { Panel.text = Prim.describe item.Sheet.what; color = loud };
            { Panel.text = "  " ^ where; color = quiet };
          ]
          @ List.mapi
              (fun index (field : Field.t) ->
                {
                  Panel.text =
                    Printf.sprintf "%s %s %s"
                      (if index = t.field then ">" else " ")
                      field.Field.label
                      (match field.Field.value with
                      | Field.Number number -> Printf.sprintf "%g" number
                      | Field.Name name -> name);
                  color = (if index = t.field then plain else quiet);
                })
              (fields t)
    in
    let body =
      match t.panel with
      | Tree -> Tree.lines ~plain ~remounted:loud t.tree
      | Plan ->
          List.map
            (fun (item : Sheet.item) ->
              {
                Panel.text = Prim.describe item.Sheet.what;
                color = (if item.Sheet.at = None then quiet else plain);
              })
            (Sheet.items t.frame ~room:t.room)
      | Graph ->
          let graph = Graph.read t.frame in
          List.concat_map
            (fun (r : Graph.room) ->
              {
                Panel.text =
                  Printf.sprintf "%s"
                    (Option.value r.Graph.name ~default:"(unnamed)");
                color = plain;
              }
              :: List.map
                   (fun (d : Graph.door) ->
                     {
                       Panel.text =
                         Printf.sprintf "  %s %s"
                           (Option.value d.Graph.name ~default:"(unnamed)")
                           (if d.Graph.joined then "joined" else "leads nowhere");
                       color = (if d.Graph.joined then plain else loud);
                     })
                   r.Graph.doors)
            graph.Graph.rooms
    in
    header @ chosen @ body

(* Declared once, here, and not inside {!overlay}. A component built inside a
   function is a fresh closure every frame, so it is never the same component
   as last frame's and is torn down and rebuilt along with everything under it
   -- which for this one would mean losing what it holds, every frame,
   silently. The rule is Element's; Tree.remounting is how it is caught.

   Its props are a pair rather than two arguments because a component takes one
   props value, and monomorphic at that. *)
let component =
  Element.declare ~name:"camlcast-edit" @@ fun ((t, font) : t * Font.t) ->
  (* Both hooks before anything conditional. A component that skipped one on
     the frames it draws nothing would present a different row of hooks from
     one render to the next, which is Hook_order_changed -- and the shape of
     mistake this whole overlay exists to make visible. *)
  let across, down = Events.use_viewport () in
  let actions = Events.use_actions () in
  (* Which panel is up, and whether one is, are read whether or not the overlay
     is showing -- or the key that opens it would only work while it was
     already open. Everything below the branch acts on what is drawn, and only
     makes sense once something is. *)
  let tapped key = Input.pressed actions (Input.Key key) in
  if tapped Key.f1 then show t Plan;
  if tapped Key.f2 then show t Graph;
  if tapped Key.f3 then show t Tree;
  if tapped Key.f4 then close t;
  if tapped Key.tab then begin
    let rooms =
      List.length
        (List.concat_map
           (fun (node : Watch.node) ->
             List.filter
               (fun (child : Watch.node) ->
                 match child.Camlcast_loom.Host.prim with
                 | Prim.Room _ -> true
                 | _ -> false)
               node.Camlcast_loom.Host.children)
           t.frame)
    in
    if rooms > 0 then t.room <- (t.room + 1) mod rooms
  end;
  if not t.showing then Element.empty
  else begin
    let items = Sheet.items t.frame ~room:t.room in
    let box = Panel.square ~across ~down in
    let x, y, side, _ = box in
    let view =
      Option.map
        (fun bounds ->
          Overhead.fit ~bounds ~x ~y ~width:side ~height:side ~inset:12)
        (Sheet.bounds items)
    in
    (* The rest act on what is picked, so they belong on this side of the
       branch. Keys the bindings do not already use: walking has WASD and the
       arrows, leaving has Escape, and the engine's own map has F3 -- which
       this takes over while it is up, being the same picture grown. *)
    if tapped Key.u then take_back t;
    if tapped Key.x then extract t;
    if tapped Key.leftbracket then t.field <- Int.max 0 (t.field - 1);
    if tapped Key.rightbracket then
      t.field <- Int.min (Int.max 0 (List.length (fields t) - 1)) (t.field + 1);
    if tapped Key.minus then nudge t (-1.);
    if tapped Key.equals then nudge t 1.;
    (* Picking up, holding, and letting go. All three read the same pointer,
       which Input has already put in the framebuffer's coordinates -- the ones
       Overhead.to_room takes. *)
    (match (t.panel, view) with
    | Plan, Some view -> (
        let at = Input.pointer actions in
        if Input.pressed actions (Input.Button Input.Left) then begin
          t.selected <- Sheet.hit view items at;
          t.field <- 0;
          t.dragging <-
            (match t.selected with
            | Sheet.Corner { item; which }
              when written_point t item which <> None ->
                Some (item.Sheet.path, which)
            | _ -> None)
        end;
        match t.dragging with
        | Some (path, which) when Input.down actions (Input.Button Input.Left)
          -> (
            match item_at t path with
            | Some item -> drag_to t item which (Overhead.to_room view at)
            | None -> ())
        | Some (path, which) ->
            (match item_at t path with
            | Some item -> write_point t item which (Overhead.to_room view at)
            | None -> ());
            t.dragging <- None
        | None -> ())
    | Graph, _ ->
        if Input.pressed actions (Input.Button Input.Left) then begin
          let graph = Graph.read t.frame in
          let view = Graph.place graph ~x ~y ~width:side ~height:side in
          match Graph.hit view (Input.pointer actions) with
          | Graph.Door { door; _ } -> join t door
          | Graph.Room _ | Graph.Nothing -> ()
        end
    | _ -> ());
    let ink = Color.rgb 205 210 220
    and computed = Color.rgb 105 112 124
    and chosen = Color.rgb 255 190 90 in
    let picture =
      match (t.panel, view) with
      | Plan, Some view ->
          Sheet.draw ~view ~draggable:(draggable t) ~ink ~computed
            ~picked:chosen ~selected:t.selected items
      | Graph, _ ->
          let graph = Graph.read t.frame in
          Graph.draw
            ~view:(Graph.place graph ~x ~y ~width:side ~height:side)
            ~ink ~joined:(Color.rgb 90 210 120) ~stub:(Color.rgb 240 90 80)
            ~picked:chosen ~selected:Graph.Nothing graph
      | _ -> Element.empty
    in
    let column = Int.max 120 (across / 3) in
    Element.fragment
      [
        (match t.panel with
        | Tree -> Element.empty
        | Plan | Graph ->
            P.rect ~x ~y ~w:side ~h:side ~color:(Color.rgb 10 12 18) ~alpha:210
              ());
        picture;
        Panel.draw ~font ~backing:(Color.rgb 10 12 18)
          ~x:(across - column - 4)
          ~y:4 ~width:column ~height:(down - 8) (lines t);
      ]
  end

let overlay t ~font = component (t, font)

let play ?title ?width ?height ?controls t description =
  Run.play ?title ?width ?height ?controls ~watch:(watch t) description
