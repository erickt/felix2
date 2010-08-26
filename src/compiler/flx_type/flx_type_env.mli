type t

(** Return an empty type environment. *)
val empty: t

(** Find the type bound to the variable name. *)
val find: t -> Flx_type.name -> Flx_type.Type.t option

(** Bind a variable name to a type. *)
val add: t -> Flx_type.name -> Flx_type.Type.t -> t

(** Print the type environment. *)
val print: Format.formatter -> t -> unit
