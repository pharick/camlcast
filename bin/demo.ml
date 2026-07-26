open Camlcast_demo

let listing () =
  print_endline "camlcast-demo — one small world per engine feature\n";
  print_endline "usage: camlcast-demo <demo>\n";
  List.iter
    (fun (demo : Catalogue.t) ->
      Printf.printf "  %-9s %s\n" demo.Catalogue.name demo.Catalogue.blurb)
    Catalogue.demos;
  print_newline ();
  print_endline
    "W A S D to walk, mouse to look, F11 for fullscreen, Esc to quit."

let () =
  match Sys.argv with
  | [| _ |] | [| _; "--list" |] -> listing ()
  | [| _; name |] -> (
      match Catalogue.find name with
      | None ->
          prerr_endline ("camlcast-demo: no demo called " ^ name ^ "\n");
          listing ();
          exit 1
      | Some demo -> (
          match demo.Catalogue.run () with
          | Ok () -> ()
          | Error (`Msg message) ->
              prerr_endline ("SDL error: " ^ message);
              exit 1))
  | _ ->
      prerr_endline "camlcast-demo: one demo at a time\n";
      listing ();
      exit 1
