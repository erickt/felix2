type t =
  | Int of int
  | Big_int of Big_int.big_int
  | Str of string
  | Sym of string
  | Id of string
  | List of t list

exception Sexp_error of t * string

(* Convert from ocs to an s-expression. *)
val of_ocs : Ocs_types.sval -> t

(* Convert an s-expression to a statement. *)
val to_stmt : t -> Flx_ast.Stmt.t

(** Prints out the s-expression to the formatter. *)
val print : Format.formatter -> t -> unit
