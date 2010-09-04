open Batteries
open Flx_format
open Flx_type

module Ast_type = Flx_ast.Type
module Ast_literal = Flx_ast.Literal
module Ast_expr = Flx_ast.Expr
module Ast_parameter = Flx_ast.Parameter
module Ast_param = Flx_ast.Param
module Ast_lambda = Flx_ast.Lambda
module Ast_stmt = Flx_ast.Stmt

(** Error out *)
let error sr format =
  ksprintf (fun s -> raise (Type_error (sr, s))) format


let bind_int_kind = function
  | Ast_type.Int_int -> Type.Int
  | Ast_type.Int_uint -> Type.Uint


let bind_type env tve typ =
  let open Ast_type in

  let sr = sr typ in
  match node typ with
  (* Unknowns are transformed into free type variables. *)
  | Unknown ->
      let tve, typ = Flx_tve.make_type_variable tve in
      env, tve, typ

  (* Bind integer types. *)
  | Int int_kind -> env, tve, Type.integer ~sr (bind_int_kind int_kind)
  | Name "int" -> env, tve, Type.integer ~sr Type.Int
  | Name "uint" -> env, tve, Type.integer ~sr Type.Uint

  (* Bind string types. *)
  | String -> env, tve, Type.string ~sr ()
  | Name "string" -> env, tve, Type.string ~sr ()

  | Name name ->
      begin match Flx_env.find env name with
      | None -> error sr "Cannot find type named \"%s\"" name
      | Some typ -> env, tve, typ
      end

  | _ -> error sr "Cannot bind type yet:@ %a" Ast_type.print typ


(** Bind an AST literal to a typed literal. *)
let bind_literal literal =
  let open Ast_literal in

  match node literal with
  | Int (kind,num) -> Literal.integer (bind_int_kind kind) num
  | String s -> Literal.string s


(** Bind an AST expression to a typed expression. *)
let rec bind_expr env tve expr =
  let open Ast_expr in

  let bind_binary_expr env tve lhs rhs =
      (* Bind the left and right hand sides of the expression. *)
      let env, tve, lhs = bind_expr env tve lhs in
      let env, tve, rhs = bind_expr env tve rhs in

      let typ_lhs = Expr.typ lhs in
      let typ_rhs = Expr.typ rhs in

      (* Make sure all the types are the same. *)
      if not (Type.equals typ_lhs typ_rhs) then
        error (Expr.sr rhs)
          "This expression has type@ %a but but expected type@ %a instead"
          Type.print typ_lhs
          Type.print typ_rhs;

      env, tve, typ_lhs, lhs, rhs
  in

  let sr = sr expr in
  match node expr with
  | Literal literal -> env, tve, Expr.literal ~sr (bind_literal literal)

  | Name name ->
      (* Look up the variable name in the environment. *)
      let typ =
        begin match Flx_env.find env name with
        | Some typ -> typ
        | None -> error sr "unbound variable named \"%s\"" name
        end
      in
      env, tve, Expr.make ~sr typ (Expr.Name name)

  | Sum [lhs; rhs] ->
      let env, tve, typ, lhs, rhs = bind_binary_expr env tve lhs rhs in
      env, tve, Expr.make ~sr typ (Expr.Sum (lhs, rhs))

  | Product [lhs; rhs] ->
      let env, tve, typ, lhs, rhs = bind_binary_expr env tve lhs rhs in
      env, tve, Expr.make ~sr typ (Expr.Product (lhs, rhs))

  | _ -> error sr "Cannot bind expression:@ %a" Ast_expr.print expr


let bind_parameter env tve parameter =
  let open Ast_parameter in

  let kind =
    match parameter.kind with
    | Val -> Parameter.Val
  in

  (* Bind the parameter type. *)
  let env, tve, typ = bind_type env tve parameter.typ in

  (* Add the type name to the type environment. *)
  let env = Flx_env.add env parameter.name typ in

  (* Make the parameter. *)
  let parameter = Parameter.make
    ~kind
    ~name:parameter.name
    ~typ
    ~default:None
  in

  env, tve, parameter


let bind_param env tve param =
  let open Ast_param in

  let env, tve, parameters =
    List.fold_left begin fun (env, tve, parameters) parameter ->
      let env, tve, parameter = bind_parameter env tve parameter in
      env, tve, parameter :: parameters
    end
    (env, tve, [])
    param.parameters
  in
  env, tve, Param.make (List.rev parameters)


let bind_params env tve params =
  let env, tve, params =
    List.fold_left begin fun (env, tve, params) param ->
      let env, tve, param = bind_param env tve param in
      env, tve, param :: params
    end
    (env, tve, [])
    params
  in
  env, tve, List.rev params


let rec bind_lambda env tve { Ast_lambda.kind; params; return_typ; stmts } =
  let kind =
    match kind with
    | Ast_lambda.Function -> Lambda.Function
  in

  let env, tve, params = bind_params env tve params in
  let env, tve, return_typ = bind_type env tve return_typ in
  let env, tve, stmts = bind_stmts env tve stmts in

  env, tve, Lambda.make kind params return_typ stmts


(** Bind an AST statement to a typed statement. *)
and bind_stmt env tve stmt =
  let open Ast_stmt in

  let sr = sr stmt in
  match node stmt with
  | Noop s -> env, tve, Stmt.noop ~sr s

  | Val (name,constraint_type,expr) ->
      begin match expr with
      | None -> error sr "val statement not provided an expression"
      | Some expr ->
          let env, tve, expr = bind_expr env tve expr in
          let typ = Expr.typ expr in

          (* Make sure the constraint matches the inferred type. *)
          let env, tve, constraint_type =
            match constraint_type with
            | Some constraint_type ->
                let env, tve, constraint_type = bind_type
                  env
                  tve
                  constraint_type
                in

                if constraint_type != typ then
                  error sr
                    "This expression of type %a but was expected of type %a"
                    Type.print typ
                    Type.print constraint_type;

                env, tve, Some constraint_type
            | None -> env, tve, None
          in

          (* Bind this name to the type. *)
          let env = Flx_env.add env name typ in

          env, tve, Stmt.value ~sr name expr
      end

  | Curry (name,lambda) ->
      let env, tve, lambda = bind_lambda env tve lambda in
      env, tve, Stmt.curry ~sr name lambda

  | Return expr ->
      let env, tve, expr = bind_expr env tve expr in
      env, tve, Stmt.return ~sr expr


and bind_stmts env tve stmts =
  let env, tve, stmts =
    List.fold_left begin fun (env, tve, stmts) stmt ->
      let env, tve, stmt = bind_stmt env tve stmt in
      env, tve, stmt :: stmts
    end
    (env, tve, [])
    stmts
  in
  env, tve, List.rev stmts
