(** An import. *)
type t =
  { source : string
  ; target : string
  }

let make ~source ~target = { source; target }
let source_of_t t = t.source
let target_of_t t = t.target
let equal t1 t2 = String.equal t1.source t2.source && String.equal t1.target t2.target
