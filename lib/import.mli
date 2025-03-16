(** An import. *)
type t

(** Construct an import for a [source] and [target]. *)
val make : source:string -> target:string -> t

(** Get the import's source. *)
val source_of_t : t -> string

(** Get the import's target. *)
val target_of_t : t -> string
