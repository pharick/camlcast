(** What a frame of description comes to.

    A record of one field today, and a record rather than a bare
    {!Camlcast.World.t} because of what is about to join it: where the camera
    stands, and whatever a HUD tree draws over the top. Those arrive in their
    own steps, and a caller that already pattern-matches on a record will not
    have to be rewritten when they do. *)

open Camlcast

type t = { world : World.t }
