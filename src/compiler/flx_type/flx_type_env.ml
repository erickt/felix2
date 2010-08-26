open Flx_format

type t = (Flx_type.name * Flx_type.Type.t) list

(** Return an empty type environment. *)
let empty = []

(** Find the type bound to the variable name. *)
let find env name =
  try Some (List.assoc name env)
  with Not_found -> None

(** Bind a variable name to a type. *)
let add env name typ = (name, typ) :: env

(** Print the type environment. *)
let print ppf env =
  Flx_list.print begin fun ppf (name, typ) ->
    Flx_format.print_tuple2 ppf
      Format.pp_print_string name
      Flx_type.Type.print typ
  end ppf env
