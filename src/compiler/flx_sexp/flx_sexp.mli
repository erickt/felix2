type t =
  | Int of string
  | Str of string
  | Sym of string
  | Id of string
  | Lst of t list

(* Convert from ocs to an s-expression. *)
val of_ocs : Ocs_types.sval -> t
