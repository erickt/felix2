open Flx_type

exception Unification_failed

(** Unify two types together. *)
val unify: Flx_tve.t -> Type.t -> Type.t -> Flx_tve.t
