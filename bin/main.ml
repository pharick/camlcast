let () =
  match Raycaster.Engine.run Raycaster.World.default with
  | Ok () -> ()
  | Error (`Msg message) ->
      prerr_endline ("SDL error: " ^ message);
      exit 1
