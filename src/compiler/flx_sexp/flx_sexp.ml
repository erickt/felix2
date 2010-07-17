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
  Format.kprintf (fun s -> raise (Sexp_error (sexp, s))) format

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

let to_literal = function
  | List [Id "ast_int"; Str s; i] -> Literal.Int (s, to_big_int i)
  | sexp -> error sexp "Invalid literal"

let to_expr = function
  (* <literal> *)
  | List [Id "ast_literal"; sr; literal] ->
      Expr.Literal (to_sr sr, to_literal literal)

  (* a *)
  | List [Id "ast_name"; sr; Id name; List ts] ->
      (* Ignoring ts for the moment. *)
      Expr.Name (to_sr sr, name)

  | sexp -> error sexp "Invalid expression"

let to_type = function
  (* typedef foo *)
  | List [Id "ast_name"; sr; (Id name | Str name); List ts] ->
      (* Ignoring ts for the moment. *)
      Type.Name (to_sr sr, name)

  | sexp -> error sexp "Invalid type"

let to_stmt = function
  | List [] -> Stmt.Noop (Flx_srcref.dummy_sr, "")

  (* val a = <expr>; *)
  | List [Id "ast_val_decl"; sr; Str name; vs; typ; expr ] ->
      (* Ignoring type variables (vs) for now. *)
      Stmt.Val (
        to_sr sr,
        name,
        to_option to_type typ,
        to_option to_expr expr)

  | sexp -> error sexp "Invalid statement"

(** Prints out the s-expression to the formatter. *)
let rec print ppf = function
  | Int i -> print_variant1 ppf "Int" pp_print_int i
  | Big_int i -> print_variant1 ppf "Big_int" print_big_int i
  | Str s -> print_variant1 ppf "Str" print_string s
  | Sym s -> print_variant1 ppf "Sym" print_string s
  | Id s -> print_variant1 ppf "Id" print_string s
  | List ss -> print_variant1 ppf "List" (Flx_list.print print) ss
