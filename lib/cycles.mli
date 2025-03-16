open Base

type t

(** Find cycles in the dependency graph *)
val find : verbose:bool -> Project_imports.t -> t

(** Show cycles *)
val show : verbose:bool -> t -> unit
