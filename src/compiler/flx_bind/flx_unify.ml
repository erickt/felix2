open Flx_type

exception Unification_failed

(** Unify two types together. *)
let rec unify ~sr tve typ1 typ2 =
  unify' ~sr tve (tvchase tve typ1) (tvchase tve typ2)

(** Chase through a substitution 'shallowly': stop at the last equivalent type
 * variable. *)
and tvchase tve typ =
  let open Type in

  match typ.node with
  | Variable v ->
      begin match Flx_tve.find tve v with
      | Some typ -> tvchase tve typ
      | None -> typ
      end
  | _ -> typ

(** If either typ1 or typ2 are type variables, they must be unbound. *)
and unify' ~sr tve typ1 typ2 =
  let open Type in

  match typ1.node, typ2.node with
  | Integer kind1, Integer kind2 when kind1 = kind2 -> tve
  | String, String -> tve
  | Name s1, Name s2 when s1 = s2 -> tve
  | Tuple ts1, Tuple ts2 ->
      begin try List.fold_left2 (unify ~sr) tve ts1 ts2
      with Type_error (_,_) | Invalid_argument _ ->
        raise Unification_failed
      end
  | Arrow (lhs1,rhs1), Arrow (lhs2,rhs2) ->
      unify ~sr (unify ~sr tve rhs1 rhs2) lhs1 lhs2
  | Variable var1, _ -> unify_free_variable ~sr tve var1 typ2
  | _, Variable var2 -> unify_free_variable ~sr tve var2 typ1
  | _, _ -> raise Unification_failed

and unify_free_variable ~sr tve var1 typ2 =
  let open Type in

  match typ2.node with
  | Variable var2 ->
      if var1 = var2 then tve else

      (* Record a new constraint. *)
      Flx_tve.add tve var1 typ2

  | _ ->
      if occurs tve var1 typ2
      then raise Unification_failed
      else Flx_tve.add tve var1 typ2

and occurs tve var1 typ2 =
  let open Type in

  match typ2.node with
  | Integer _ -> false
  | String -> false
  | Name _ -> false
  | Tuple ts -> List.exists (occurs tve var1) ts
  | Arrow (lhs, rhs) -> occurs tve var1 lhs || occurs tve var1 rhs
  | Variable var2 ->
      begin match Flx_tve.find tve var2 with
      | None -> var1 = var2
      | Some typ2 -> occurs tve var1 typ2
      end
  (*
  | _ -> error ~sr:(sr typ2) "occurs does not support %a yet" print typ2
  *)
