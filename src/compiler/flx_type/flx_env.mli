type 'a t

(** Return an empty type environment. *)
val empty: 'a t

(** Find the type bound to the variable name. *)
val find: 'a t -> Flx_type.name -> 'a option

(** Bind a variable name to a type. *)
val add: 'a t -> Flx_type.name -> 'a -> 'a t

(** Print the type environment. *)
val print: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
