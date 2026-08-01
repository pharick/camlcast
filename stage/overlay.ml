(* Implementation of {!Camlcast_stage.Overlay}; the interface carries the
   prose. *)

open Camlcast

let draw buffer items =
  List.iter
    (fun item ->
      match item with
      | Prim.Rect { x; y; w; h; color; alpha } ->
          Paint.rect buffer ~x ~y ~w ~h ~color ~alpha
      | Prim.Bar { x; y; w; h; fraction; color } ->
          Paint.bar buffer ~x ~y ~w ~h ~fraction ~color
      | Prim.Text { x; y; text; color; font } ->
          Font.draw buffer font text ~x ~y ~color
      | Prim.Picture { x; y; image; tint } ->
          Paint.image ?tint buffer image ~x ~y
      | Prim.Crosshair color -> Paint.crosshair buffer ~color
      | _ -> ())
    items
