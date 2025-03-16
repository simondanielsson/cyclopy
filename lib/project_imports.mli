(** All imports in a project *)
type t = Module_imports.t

val find : verbose:bool -> string -> t
