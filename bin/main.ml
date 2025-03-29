open Core
open Cyclopy

(* Find circular imports in a project *)
let find_and_show_cicular_imports ~verbose ~entrypoint_file =
  ignore verbose;
  Import_graph.create (Fpath.v entrypoint_file) |> Cycles.find |> Cycles.show
;;

(* Build the command and pass it to Term.eval *)
let () = Cyclopy.Argparser.run ~callback:find_and_show_cicular_imports
