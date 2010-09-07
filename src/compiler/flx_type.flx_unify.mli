open Flx_type

exception Unification_failed

(** Unify two types together. *)
val unify: ~sr:Flx_srcref.t -> Flx_tve.t -> Type.t -> Type.t
