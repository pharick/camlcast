(* Implementation of {!Camlcast_edit.Session}; the interface carries the
   prose. *)

open Camlcast

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
let show_room t room = t.room <- Int.max 0 room
let pending t = Patch.count t.patch
let undo t = Revise.undo t.revise

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
   -- which for this one would mean the panel losing whatever it holds, every
   frame, silently. The rule is Element's; Tree.remounting is how it is caught.

   Its props are a pair rather than two arguments because a component takes one
   props value, and monomorphic at that. *)
let component =
  Element.declare ~name:"camlcast-edit" @@ fun ((t, font) : t * Font.t) ->
  if not t.showing then Element.empty
  else
    let across, down = Events.use_viewport () in
    (* Down the right-hand side, a third of the width, clear of the edge. A
       third rather than a half because what is underneath is the game, and an
       overlay that covers the thing it is about is no use. *)
    let margin = 4 in
    let width = Int.max 120 (across / 3) in
    Panel.draw ~font ~backing:(Color.rgb 10 12 18)
      ~x:(across - width - margin)
      ~y:margin ~width
      ~height:(down - (2 * margin))
      (lines t)

let overlay t ~font = component (t, font)

let play ?title ?width ?height ?controls t description =
  Run.play ?title ?width ?height ?controls ~watch:(watch t) description
