(** Parse arguments and run the callback. *)
val run : callback:(verbose:bool -> entrypoint_file:string -> unit) -> unit
