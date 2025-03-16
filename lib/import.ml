(** An import. *)
type t =
  { source : string
  ; target : string
  }

let make ~source ~target = { source; target }
let source_of_t t = t.source
let target_of_t t = t.target
