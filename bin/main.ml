open Base

(* Find circular imports in a project *)
let find_and_show_cicular_imports ~verbose ~entrypoint_file =
  let open Circular_imports.Logger in
  log verbose "Loading all imports";
  let project_imports = Circular_imports.Project_imports.find ~verbose entrypoint_file in
  log verbose "Finding cycles...";
  Circular_imports.Cycles.(find ~verbose project_imports |> show ~verbose)
;;

(* Build the command and pass it to Term.eval *)
let () = Circular_imports.Argparser.run ~callback:find_and_show_cicular_imports
