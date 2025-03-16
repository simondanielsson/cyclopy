open Cmdliner

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
    const (fun verbose entrypoint_file -> callback ~verbose ~entrypoint_file)
    $ verbose
    $ entrypoint_file)
;;

(* Define a info for the command line interface *)
let info = Cmd.info "circular_imports" ~version:"1.0" ~doc:"Visualize circular imports"

(* Build the command and pass it to Term.eval *)
let run ~callback = Cmd.eval (Cmd.v info (term callback)) |> exit
