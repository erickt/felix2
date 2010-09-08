open Batteries

open Format

module NameMap = Map.Make(
  struct
    type t = Flx_type.name
    let compare = compare
  end)

type 'a t = 'a NameMap.t

(** Return an empty environment. *)
let empty = NameMap.empty

(** Find an element in an environment. *)
let find env name =
  try Some (NameMap.find name env)
  with Not_found -> None

(** Bind an element to a name in an environment. *)
let add env name elt = NameMap.add name elt env

let print_map pp_value f table =
  Format.fprintf f "@[<hv0>@[<hv2>{.@ ";
  let _ =
    NameMap.fold begin fun key value first ->
      if not first then Format.fprintf f ";@ ";
      Format.fprintf f "%a@ =@ %a" pp_print_string key pp_value value;
      false
    end table true
  in
  Format.fprintf f "@]@ .}@]"

(** Print the environment. *)
let print print_elt ppf env =
  print_map print_elt ppf env
