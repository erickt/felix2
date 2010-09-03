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
let find (_, tve) var =
  try Some (IntMap.find var tve)
  with Not_found -> None

(** Bind a free type variable to the type variable environment. *)
let add (n, tve) var typ = n, IntMap.add var typ tve

(** Subsitute type variables in a type. *)
let rec substitute tve typ =
  let open Type in

  match node typ with
  | Variable var ->
      (* Look up the variable in the type variable environment. If we find it,
       * recursively substitute types into that type variable. Otherwise, do
       * nothing. *)
      begin match find tve var with
      | Some typ -> substitute tve typ
      | None -> typ
      end
  | Arrow (lhs, rhs) ->
      let lhs = substitute tve lhs in
      let rhs = substitute tve rhs in
      arrow ~sr:(sr typ) lhs rhs

  | _ -> typ


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
