open Format
open Flx_format

module Ast_type = Flx_ast.Type
module Ast_literal = Flx_ast.Literal
module Ast_expr = Flx_ast.Expr
module Ast_stmt = Flx_ast.Stmt

type name = Flx_ast.name

module Type =
  struct
    include Ast_type
  end

module Literal = Ast_literal

module Expr =
  struct
    type t = { sr: Flx_srcref.t; node: node; typ: Type.t }

    and node =
      | Literal of Literal.t
      | Tuple of t list
      | Name of string
      | Sum of t list
      | Product of t list

    (** make an expression. *)
    let make ~sr ~node ~typ = { sr; node; typ }

    (** return the expression's node. *)
    let node { node } = node

    (** return the expression's source reference. *)
    let sr { sr } = sr

    (** return the expression's type. *)
    let typ { typ } = typ

    let rec print_node ppf = function
      | Literal lit -> print_variant1 ppf "Literal" Literal.print lit
      | Tuple es -> print_variant1 ppf "Tuple" (Flx_list.print print) es
      | Name name -> print_variant1 ppf "Name" print_string name
      | Sum es -> print_variant1 ppf "Sum" (Flx_list.print print) es
      | Product es -> print_variant1 ppf "Product" (Flx_list.print print) es

    (** Print an expression. *)
    and print ppf { sr; node; typ } =
      print_record3 ppf
        "sr" Flx_srcref.print sr
        "node" print_node node
        "typ" Type.print typ
  end

module Stmt =
  struct
    type t = { sr: Flx_srcref.t; node: node; typ: Type.t }

    and node =
      | Noop of string
      | Val of name * Expr.t

    (** make a statement. *)
    let make ~sr ~node ~typ = { sr; node; typ }

    (** return the statement's node. *)
    let node { node } = node

    (** return the statement's source reference. *)
    let sr { sr } = sr

    (** return the statement's type. *)
    let typ { typ } = typ

    let rec print_node ppf = function
      | Noop s -> print_variant1 ppf "Noop" print_string s
      | Val (name,expr) ->
          print_variant2 ppf "Val"
            print_string name
            Expr.print expr

    (** Print a statement. *)
    and print ppf { sr; node; typ } =
      print_record3 ppf
        "sr" Flx_srcref.print sr
        "node" print_node node
        "typ" Type.print typ
  end
