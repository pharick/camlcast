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
   Telling those two apart is the whole reason {!Engine.run_state} reports an
   ending, and the only reason the launcher is a loop rather than a line. *)
let rec browse () =
  match Menu.choose () with
  | Error _ as error -> error
  | Ok None -> Ok ()
  | Ok (Some (demo : Catalogue.t)) -> (
      match demo.Catalogue.run () with
      | Error _ as error -> error
      | Ok Camlcast.Engine.Closed -> Ok ()
      | Ok Camlcast.Engine.Left -> browse ())

let () =
  match Sys.argv with
  | [| _ |] -> (
      match browse () with
      | Ok () -> ()
      | Error (`Msg message) ->
          (* A window is what was asked for, so say why there is none and then
             fall back to the listing rather than leaving an empty terminal. *)
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
          match demo.Catalogue.run () with
          | Ok _ -> ()
          | Error (`Msg message) ->
              prerr_endline ("SDL error: " ^ message);
              exit 1))
  | _ ->
      prerr_endline "camlcast-demo: one demo at a time\n";
      listing ();
      exit 1
