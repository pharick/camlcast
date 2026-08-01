(* Implementation of {!Camlcast_loom.Element}; the interface carries the prose. *)

exception Duplicate_key of { at : string; key : string }

type 'prim t =
  | Empty
  | Fragment of { key : string option; children : 'prim t list }
  | Prim of { prim : 'prim; key : string option; children : 'prim t list }
  | Provide of { binding : Context.binding; children : 'prim t list }
  | Component : {
      render : 'props -> 'prim t;
      props : 'props;
      key : string option;
      name : string;
    }
      -> 'prim t

let empty = Empty
let fragment ?key children = Fragment { key; children }

let provide context value children =
  Provide { binding = Context.bind context value; children }

let prim ?key ?(children = []) prim = Prim { prim; key; children }
let component ?key ~name render props = Component { render; props; key; name }

(* [render] is captured once, when this is called, and every element the
   returned function makes carries that same closure. That is what the
   reconciler compares, so it is what makes a component keep its identity —
   and its state — from one frame to the next. *)
let declare ~name render ?key props = Component { render; props; key; name }

let key = function
  | Fragment { key; _ } | Prim { key; _ } | Component { key; _ } -> key
  | Empty | Provide _ -> None

let name = function
  | Component { name; _ } -> Some name
  | Empty | Fragment _ | Prim _ | Provide _ -> None
