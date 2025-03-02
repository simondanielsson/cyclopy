(* Define a function for logging *)
let log verbose message = if verbose then Stdlib.Printf.printf "%s\n" message
