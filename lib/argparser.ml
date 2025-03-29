open Cmdliner

(* let setup_logging verbose = *)
(*   let level = if verbose then Some Logs.info else None in *)
(*   Logs.set_level level; *)
(*   (* Logs.set_reporter (Logs_fmt.reporter ()); *) *)
(*   Logs.info (fun m -> m "Verbose mode enabled") *)
(* ;; *)

(* Define a term for the command line interface *)
let term callback =
  let entrypoint_file =
    Arg.(
      required
      & pos
          0
          (some string)
          None
          (info
             []
             ~doc:"The entrypoint module for the project."
             ~docv:"FILE"
             ~docs:"ARGUMENTS"))
  in
  let verbose =
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc:"Enable verbose logging")
  in
  Term.(
    const (fun verbose entrypoint_file ->
      (* setup_logging verbose; *)
      callback ~verbose ~entrypoint_file)
    $ verbose
    $ entrypoint_file)
;;

(* Define a info for the command line interface *)
let info = Cmd.info "circular_imports" ~version:"1.0" ~doc:"Visualize circular imports"

(* Build the command and pass it to Term.eval *)
let run ~callback = Cmd.eval (Cmd.v info (term callback)) |> exit
