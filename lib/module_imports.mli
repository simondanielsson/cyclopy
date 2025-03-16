(* Represent all import statements of a module *)
type t = Import.t list

(** Extract imports from a file's content *)
val module_name_of_path : project_root:string -> string -> string

(** Extract imports from a file's content *)
val from_file_content : verbose:bool -> module_name:string -> string -> t

(** Extract imports from a file *)
val from_file
  :  verbose:bool
  -> project_root:string
  -> ?reader:(string -> string)
  -> string
  -> t
