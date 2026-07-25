let () =
  match Raycaster.Engine.run Camlcast_demo.Level.default with
  | Ok () -> ()
  | Error (`Msg message) ->
      prerr_endline ("SDL error: " ^ message);
      exit 1
