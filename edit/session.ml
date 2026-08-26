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

(* Whether the plan should offer to drag this. The predicate, asked of the
   source rather than guessed from the frame: a wall can carry a position and
   still be written out of state, which is exactly the case worth drawing
   differently. *)
let draggable t item =
  match source_of t item with
  | Link.Found source -> Link.editable source
  | Link.Unpositioned | Link.Unreadable _ -> false

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
    header @ body

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
    (* Picking up, holding, and letting go. All three read the same pointer,
       which Input has already put in the framebuffer's coordinates -- the ones
       Overhead.to_room takes. *)
    (match (t.panel, view) with
    | Plan, Some view -> (
        let at = Input.pointer actions in
        if Input.pressed actions (Input.Button Input.Left) then begin
          t.selected <- Sheet.hit view items at;
          t.dragging <-
            (match t.selected with
            | Sheet.Corner { item; which } when draggable t item ->
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
