open Camlcast_demo

let listing () =
  print_endline "camlcast-demo — one small world per engine feature\n";
  print_endline "usage: camlcast-demo [demo]\n";
  List.iter
    (fun (demo : Catalogue.t) ->
      Printf.printf "  %-9s %s\n" demo.Catalogue.name demo.Catalogue.blurb)
    Catalogue.demos;
  print_newline ();
  print_endline
    "Named, it runs that one. Named nothing, it opens the same list on screen.";
  print_endline
    "W A S D to walk, mouse to look, F11 for fullscreen, Esc to leave a demo."

(* Menu, demo, menu again, for as long as the player keeps coming back.
   Escape ends a demo and returns here; shutting the window ends the program.
   Telling those two apart is the whole reason {!Engine.run} reports an
   ending, and the only reason the launcher is a loop rather than a line.

   One window for the whole of it. The list and the demo it opens are two runs
   and not two windows, so coming back to the list is the picture changing
   rather than the window going away and returning at the size it first had.

   The list comes back open on the demo just left, too. It is a fresh run and
   the state that remembers the cursor went with the last one, so what is passed
   forward is the demo itself and the list finds its own row again. *)
let rec browse ?from window =
  match Menu.choose ?from window with
  | Error _ as error -> error
  | Ok None -> Ok ()
  | Ok (Some (demo : Catalogue.t)) -> (
      match Catalogue.attempt (fun () -> demo.Catalogue.run window) with
      | Error _ as error -> error
      | Ok Camlcast.Run.Closed -> Ok ()
      | Ok Camlcast.Run.Returned -> browse ~from:demo window)

let () =
  match Sys.argv with
  | [| _ |] -> (
      match Camlcast.Run.with_window (fun window -> browse window) with
      | Ok () -> ()
      | Error (`Msg message) ->
          (* Whatever it was — a window SDL would not open, or a demo whose art
             is not on the disk — say so, and then fall back to the listing
             rather than leaving an empty terminal. *)
          prerr_endline ("camlcast-demo: " ^ message ^ "\n");
          listing ();
          exit 1)
  | [| _; "--list" |] -> listing ()
  | [| _; name |] -> (
      match Catalogue.find name with
      | None ->
          prerr_endline ("camlcast-demo: no demo called " ^ name ^ "\n");
          listing ();
          exit 1
      | Some demo -> (
          (* Named on the command line, a demo is the whole program: however it
             ends, there is nothing to come back to. *)
          match
            Camlcast.Run.with_window (fun window ->
                Catalogue.attempt (fun () -> demo.Catalogue.run window))
          with
          | Ok _ -> ()
          | Error (`Msg message) ->
              prerr_endline ("camlcast-demo: " ^ message);
              exit 1))
  | _ ->
      prerr_endline "camlcast-demo: one demo at a time\n";
      listing ();
      exit 1
