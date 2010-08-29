open Flx_format
open Flx_type

module Ast_expr = Flx_ast.Expr

let bind_type = ()

(** Bind an AST literal to a typed literal. *)
let bind_literal env literal =
  let open Literal in
  let typ =
    match Ast_literal.node literal with
    | Int (kind,_) -> Type.int ~sr:Flx_srcref.dummy_sr ~kind
    | String _ -> Type.string Flx_srcref.dummy_sr
  in
  literal, typ

(** Bind an AST expression to a typed expression. *)
let rec bind_expr env expr =
  let open Expr in
  let sr = Ast_expr.sr expr in
  match Ast_expr.node expr with
  | Ast_expr.Literal literal ->
      let literal, typ = bind_literal env literal in
      make ~sr ~typ ~node:(Literal literal)

  | Ast_expr.Name name ->
      (* Look up the variable name in the environment. *)
      let typ =
        begin match Flx_type_env.find env name with
        | Some typ -> typ
        | None -> failwith "unbound variable named \"%s\"" name
        end
      in
      make ~sr ~typ ~node:(Name name)

  | Ast_expr.Sum (e :: es) ->
      (* Bind all the expressions in the summation. *)
      let e = bind_expr env e in
      let es = List.map (bind_expr env) es in

      (* Make sure all the types are the same. *)
      let typ =
        List.fold_left begin fun typ e ->
          let typ' = Expr.typ e in
          if Type.node typ = Type.node typ' then typ else
          failwith "Trying to add non-integers:@.%a@.%a@."
            Type.print typ
            Type.print typ';
        end (Expr.typ e) es
      in

      make ~sr ~typ ~node:(Sum (e :: es))

  | Ast_expr.Product (e :: es) ->
      (* Bind all the expressions in the summation. *)
      let e = bind_expr env e in
      let es = List.map (bind_expr env) es in

      (* Make sure all the types are the same. *)
      let typ =
        List.fold_left begin fun typ e ->
          let typ' = Expr.typ e in
          if Type.node typ = Type.node typ' then typ else
          failwith "Trying to multiply non-integers:@.%a@.%a@."
            Type.print typ
            Type.print typ';
        end (Expr.typ e) es
      in

      make ~sr ~typ ~node:(Product (e :: es))

  | _ -> failwith "cannot handle %a yet@." Ast_expr.print expr


(** Bind an AST statement to a typed statement. *)
let bind_stmt env stmt =
  let open Stmt in
  let sr = Ast_stmt.sr stmt in
  match Ast_stmt.node stmt with
  | Ast_stmt.Noop s ->
      env, make ~sr ~node:(Noop s) ~typ:(Type.unit sr)

  | Ast_stmt.Val (name,constraint_type,expr) ->
      begin match expr with
      | None -> failwith "val statement not provided an expression"
      | Some expr ->
          let expr = bind_expr env expr in
          let typ : Type.t = Expr.typ expr in

          (* Make sure the constraint matches the inferred type. *)
          begin match constraint_type with
          | Some constraint_type when constraint_type != typ ->
              failwith "This expression of type %a but was expected of type %a"
                Type.print typ
                Type.print constraint_type
          | _ -> ()
          end;

          (* Bind this name to the type. *)
          let env = Flx_type_env.add env name typ in

          env, make ~sr ~typ ~node:(Val (name, expr))
      end