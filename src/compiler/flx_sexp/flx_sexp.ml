open Format
open Big_int

open Ocs_types
open Flx_format
open Flx_ast

type t =
  | Int of int
  | Big_int of Big_int.big_int
  | Str of string
  | Sym of string
  | Id of string
  | List of t list

exception Sexp_error of t * string

(** Error out *)
let error sexp format =
  ksprintf (fun s -> raise (Sexp_error (sexp, s))) format

(* Convert from ocs to an s-expression. *)
let rec of_ocs = function
  | Sunbound -> failwith "unmapped ocs type Sunbound"
  | Seof -> failwith "unmapped ocs type Seof"
  | Sreal _ -> failwith "unmapped ocs type Sreal"
  | Scomplex _ -> failwith "unmapped ocs type Scomplex"
  | Srational _ -> failwith "unmapped ocs type Srational"
  | Schar _ -> failwith "unmapped ocs type Schar"
  | Sport _ -> failwith "unmapped ocs type Sport"
  | Sprim _ -> failwith "unmapped ocs type Sprim"
  | Svalues _ -> failwith "unmapped ocs type Svalues"
  | Sesym _ -> failwith "unmapped ocs type Sesym"
  | Swrapped _ -> failwith "unmapped ocs type Swrapped"
  | Sunspec -> failwith "unmapped ocs type Sunspec"
  | Spromise _ -> failwith "unmapped ocs type Spromise"
  | Sproc _ -> failwith "unmapped ocs type Sproc"
  | Snull -> List []
  | Strue -> Id "true"
  | Sfalse -> Id "false"
  | Sstring s -> Str s
  | Ssymbol s -> Id s
  | Sint i -> Int i
  | Sbigint i -> Big_int i
  | Spair _ as ocs -> List (List.map of_ocs (Ocs_misc.list_to_caml ocs))
  | Svector a -> List (List.map of_ocs (Array.to_list a))

let to_int = function
  | Int i -> i
  | (Big_int i) as sexp ->
      begin try int_of_big_int i with Failure _ ->
        error sexp "Invalid integer"
      end
  | (Str s) as sexp ->
      begin try int_of_string s with Failure _ ->
        error sexp "Invalid integer"
      end
  | sexp -> error sexp "Invalid integer"

let to_big_int = function
  | Int i -> big_int_of_int i
  | Big_int i -> i
  | (Str s) as sexp ->
      begin try big_int_of_string s with Failure _ ->
        error sexp "Invalid integer"
      end
  | sexp -> error sexp "Invalid big integer"

let to_option f = function
  | Id "none" -> None
  | List [Id "some"; expr] -> Some (f expr)
  | sexp -> error sexp "Invalid option"

let to_list f = function
  | List ls -> List.map f ls
  | sexp -> error sexp "Invalid list"

(** Parse a source reference. *)
let to_sr = function
  | List [Str filename; fl; fc; ll; lc] ->
      Flx_srcref.make (filename, to_int fl, to_int fc, to_int ll, to_int ll)
  | sexp -> error sexp "Invalid source reference"

let to_literal sexp =
  let open Literal in
  match sexp with
  | List [Id "ast_int"; Str s; i] ->
      let i = to_big_int i in
      begin match s with
      | "int" -> int Type.Int_int i
      | "uint" -> int Type.Int_uint i
      | _ -> error sexp "Invalid integer type: %s" s
      end

  | List [Id "ast_string"; Str s] -> string s

  | _ -> error sexp "Invalid literal"

let rec to_expr =
  let open Expr in
  function
  (* "foo" *)
  | Str s ->
      make ~sr:Flx_srcref.dummy_sr ~node:(Literal (Literal.string s))

  (* () *)
  | List [] ->
      make ~sr:Flx_srcref.dummy_sr ~node:(Tuple [])

  (* (<expr>) *)
  | List [expr] -> to_expr expr

  (* <literal> *)
  | List [Id "ast_literal"; sr; literal] ->
      make ~sr:(to_sr sr) ~node:(Literal (to_literal literal))

  (* a *)
  | List [Id "ast_name"; sr; Str name; List ts] ->
      (* Ignoring ts for the moment. *)
      make ~sr:(to_sr sr) ~node:(Name name)

  (* <expr> + <expr> *)
  | List [Id "ast_sum"; sr; List es] ->
      make ~sr:(to_sr sr) ~node:(Sum (List.map to_expr es))

  (* <expr> * <expr> *)
  | List [Id "ast_product"; sr; List es] ->
      make ~sr:(to_sr sr) ~node:(Product (List.map to_expr es))

  | sexp -> error sexp "Invalid expression"

let to_type =
  let open Type in
  function
  (* typedef foo *)
  | List [Id "ast_name"; sr; (Id name | Str name); List ts] ->
      (* Ignoring ts for the moment. *)
      make ~sr:(to_sr sr) ~node:(Name name)

  | Id "typ_none" ->
      make ~sr:Flx_srcref.dummy_sr ~node:Unknown

  | sexp -> error sexp "Invalid type"

let rec to_type_variables sexp =
  match sexp with
  | List [] -> []
  | _ -> error sexp "Invalid type variable"

let to_parameter sexp =
  let open Parameter in
  match sexp with
  | List [Id kind; Str name; typ; default] ->
      let kind =
        match kind with
        | "PVal" -> Val
        | _ -> error sexp "Unknown parameter kind: %s" kind
      in

      (* Currently types are required. *)
      make ~kind ~name ~typ:(to_type typ) ?default:(to_option to_expr default)

  | _ -> error sexp "Invalid parameter"

let to_param sexp =
  let open Param in
  match sexp with
  | List [List parameter; precondition] ->
      make
        ?precondition:(to_option to_expr precondition)
        (List.map to_parameter parameter)

  | _ -> error sexp "Invalid parameter"

let rec to_lambda' sexp vs params return_typ postcondition kind stmts =
  let open Lambda in

  (* Ignoring type variables (vs) for now. *)
  (*
  let vs = List.map to_type_variables vs in
  *)

  let params = List.map to_param params in

  let kind =
    match kind with
    | "Function" -> Lambda.Function
    | _ -> error sexp "unknown function kind %S" kind
  in

  let return_typ = to_type return_typ in

  let stmts = List.map to_stmt stmts in
  Lambda.make kind params return_typ stmts

and to_stmt sexp =
  let open Stmt in
  match sexp with
  | List [] -> make ~sr:Flx_srcref.dummy_sr ~node:(Stmt.Noop "")

  (* val a = <expr>; *)
  | List [Id "ast_val_decl"; sr; Str name; vs; typ; expr] ->
      (* Ignoring type variables (vs) for now. *)
      make
        ~sr:(to_sr sr)
        ~node:(Val (
          name,
          to_option to_type typ,
          to_option to_expr expr))

  (* fun foo (...) = ... *)
  | List [Id "ast_curry";
          sr;
          Str name;
          List vs;
          List params;
          List [return_typ; postcondition];
          Id kind;
          List stmts]
    ->
      let lambda = to_lambda' sexp vs params return_typ postcondition kind stmts in
      make ~sr:(to_sr sr) ~node:(Curry (name, lambda))

  | List [Id "ast_fun_return"; sr; expr] ->
      make ~sr:(to_sr sr) ~node:(Return (to_expr expr))

  | _ -> error sexp "Invalid statement"

(** Prints out the s-expression to the formatter. *)
let rec print ppf = function
  | Int i -> print_variant1 ppf "Int" pp_print_int i
  | Big_int i -> print_variant1 ppf "Big_int" print_big_int i
  | Str s -> print_variant1 ppf "Str" print_string s
  | Sym s -> print_variant1 ppf "Sym" print_string s
  | Id s -> print_variant1 ppf "Id" print_string s
  | List ss -> print_variant1 ppf "List" (Flx_list.print print) ss
