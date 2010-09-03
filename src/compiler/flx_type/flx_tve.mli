type t

(** Return an empty type environment. *)
val empty: t

(** Make a new type variable. *)
val make_type_variable: t -> t * Flx_type.Type.t

(** Find the type bound to the variable name. *)
val find: t -> int -> Flx_type.Type.t option

(** Print the type environment. *)
val print: Format.formatter -> t -> unit
