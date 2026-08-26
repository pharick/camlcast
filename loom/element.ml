(* Implementation of {!Camlcast_loom.Element}; the interface carries the prose. *)

exception Duplicate_key of { at : string; key : string }
exception Render_refused of { at : string; message : string }

(* Without this the default printer puts the path and the message inside a
   constructor: Render_refused("#0/Hall", "Room.doorway: ..."). That is less
   readable than the bare Invalid_argument it replaced. A game that does not
   run this through Check sees the exception as the line that stops the
   program, so the four lines of printer are worth having it read as one. *)
let () =
  Printexc.register_printer (function
    | Render_refused { at; message } -> Some (at ^ ": " ^ message)
    | _ -> None)

type pos = string * int * int * int

type 'prim t =
  | Empty
  | Fragment of {
      key : string option;
      at : pos option;
      children : 'prim t list;
    }
  | Prim of {
      prim : 'prim;
      key : string option;
      at : pos option;
      children : 'prim t list;
    }
  | Provide of { binding : Context.binding; children : 'prim t list }
  | Component : {
      render : 'props -> 'prim t;
      props : 'props;
      key : string option;
      at : pos option;
      name : string;
    }
      -> 'prim t

let empty = Empty
let fragment ?key children = Fragment { key; at = None; children }

let provide context value children =
  Provide { binding = Context.bind context value; children }

let prim ?key ?(children = []) prim = Prim { prim; key; at = None; children }

(* [render] is captured once, when this is called. Every element the returned
   function makes carries that same closure. The reconciler compares that
   closure, so it is what preserves a component's identity, and its state,
   from one frame to the next. *)
let declare ~name render ?key props =
  Component { render; props; key; at = None; name }

(* Rebuilt rather than updated with [with]: [Component]'s ['props] is
   existential, so the record it carries cannot be named as a value and only
   its fields are in scope here. *)
let at pos = function
  | Prim { prim; key; at = _; children } ->
      Prim { prim; key; at = Some pos; children }
  | Fragment { key; at = _; children } ->
      Fragment { key; at = Some pos; children }
  | Component { render; props; key; at = _; name } ->
      Component { render; props; key; at = Some pos; name }
  | (Empty | Provide _) as element -> element

let key = function
  | Fragment { key; _ } | Prim { key; _ } | Component { key; _ } -> key
  | Empty | Provide _ -> None

let name = function
  | Component { name; _ } -> Some name
  | Empty | Fragment _ | Prim _ | Provide _ -> None
