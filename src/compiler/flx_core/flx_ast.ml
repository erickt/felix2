open Format
open Flx_format

type id_t = string

module Type =
  struct
    type t = { sr: Flx_srcref.t; node: node }

    and node =
      | Int
      | String
      | Name of string

    (** Make a type. *)
    let make ~sr ~node = { sr; node }

    (** Return the type's node. *)
    let node { node } = node

    (** Return the type's source reference. *)
    let sr { sr } = sr

    let rec print_node ppf = function
      | Int -> print_variant0 ppf "Int"
      | String -> print_variant0 ppf "String"
      | Name s -> print_variant1 ppf "Name" print_string s

    (** Print a type. *)
    and print ppf { sr; node } =
      print_record2 ppf
        "sr" Flx_srcref.print sr
        "node" print_node node
  end

module Literal =
  struct
    type t = node

    and node =
      | Int of string * Big_int.big_int
      | String of string

    (** Make a literal. *)
    let make ~node = node

    (** Return the literal's node. *)
    let node literal = literal

    (** Make a literal integer. *)
    let int suffix num = make ~node:(Int (suffix, num))

    (** Make a literal string. *)
    let string s = make ~node:(String s)

    (** Print a literal. *)
    let print ppf = function
      | Int (s,i) ->
          print_variant2 ppf "Int"
            print_string s
            print_big_int i
      | String s ->
          print_variant1 ppf "String"
            print_string s
  end

module Expr =
  struct
    type t = { sr: Flx_srcref.t; node: node }

    and node =
      | Literal of Literal.t
      | Tuple of t list
      | Name of string
      | Sum of t list
      | Product of t list

    (** make an expression. *)
    let make ~sr ~node = { sr; node }

    (** return the expression's node. *)
    let node { node } = node

    (** return the expression's source reference. *)
    let sr { sr } = sr

    let rec print_node ppf = function
      | Literal lit -> print_variant1 ppf "Literal" Literal.print lit
      | Tuple es -> print_variant1 ppf "Tuple" (Flx_list.print print) es
      | Name name -> print_variant1 ppf "Name" print_string name
      | Sum es -> print_variant1 ppf "Sum" (Flx_list.print print) es
      | Product es -> print_variant1 ppf "Product" (Flx_list.print print) es

    (** Print an expression. *)
    and print ppf { sr; node } =
      print_record2 ppf
        "sr" Flx_srcref.print sr
        "node" print_node node
  end

module Stmt =
  struct
    type t = { sr: Flx_srcref.t; node: node }

    and node =
      | Noop of string
      | Val of id_t * Type.t option * Expr.t option

    (** make a statement. *)
    let make ~sr ~node = { sr; node }

    (** return the statement's node. *)
    let node { node } = node

    (** return the statement's source reference. *)
    let sr { sr } = sr

    let rec print_node ppf = function
      | Noop s -> print_variant1 ppf "Noop" print_string s
      | Val (id,typ,expr) ->
          print_variant3 ppf "Val"
            print_string id
            (print_opt Type.print) typ
            (print_opt Expr.print) expr

    (** Print a statement. *)
    and print ppf { sr; node } =
      print_record2 ppf
        "sr" Flx_srcref.print sr
        "node" print_node node
  end
