open Base

(* TODO: hide the hashtable behind t, and make Imports a separate module as well *)

(** Represent an import statement *)
type import =
  { source : string (** Module where import is found *)
  ; target : string (** Module being imported *)
  }

(** Extract import statements from a Python AST *)
val extract_imports : verbose:bool -> PyreAst.Concrete.Module.t -> import list

val normalize_imports
  :  verbose:bool
  -> project_root:string
  -> string
  -> import list
  -> import list

(** Build a dependency graph from import statements *)
val build_graph : import list -> (string, string list) Base.Hashtbl.t

(** Find cycles in the dependency graph *)
val find_cycles : verbose:bool -> (string, string list) Base.Hashtbl.t -> string list list

(** Format cycles for display *)
val format_cycles : string list list -> string
