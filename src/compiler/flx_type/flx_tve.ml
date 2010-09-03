open Batteries

open Format
open Flx_format
open Flx_type

module IntMap = Map.IntMap

type t = int * Type.t IntMap.t

(** Return an empty type variable environment. *)
let empty = 0, IntMap.empty

let make_type_variable (n, tve) =
  (n + 1, tve), Type.variable n

(** Find the type bound to the type variable name. *)
let find (_, tve) index =
  try Some (IntMap.find index tve)
  with Not_found -> None

let print_map pp_value f table =
  Format.fprintf f "@[<hv0>@[<hv2>{.@ ";
  let _ =
    IntMap.fold begin fun key value first ->
      if not first then Format.fprintf f ";@ ";
      Format.fprintf f "%a@ =@ %a" pp_print_int key pp_value value;
      false
    end table true
  in
  Format.fprintf f "@]@ .}@]"

(** Print the type variable environment. *)
let print ppf (n, tve) =
  print_tuple2 ppf
    pp_print_int n
    (print_map Type.print) tve
