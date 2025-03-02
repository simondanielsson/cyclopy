open Cmdliner

(* Define a term for the command line interface *)
let term callback =
  let file =
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
  Term.(const (fun file -> callback file) $ file)
;;

(* Define a info for the command line interface *)
let info = Cmd.info "circular_imports" ~version:"1.0" ~doc:"Visualize circular imports"

(* Build the command and pass it to Term.eval *)
let run ~callback = Cmd.eval (Cmd.v info (term callback)) |> exit
