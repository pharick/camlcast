(* Implementation of {!Camlcast.Overlay}; the interface carries the
   prose. *)

open Camlcast_core

let draw ?aim buffer items =
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
      | Prim.Highlight color -> (
          match aim with
          | None -> ()
          | Some (world, player) -> (
              match
                Aim.ring world player ~width:buffer.Framebuffer.width
                  ~height:buffer.Framebuffer.height
              with
              | None -> ()
              | Some corners ->
                  Paint.ring buffer
                    (List.map
                       (fun (x, y) ->
                         ( int_of_float (Float.round x),
                           int_of_float (Float.round y) ))
                       corners)
                    ~color))
      | Prim.Crosshair color -> Paint.crosshair buffer ~color
      | _ -> ())
    items
