open Format
open Flx_format

type id_t = string

module Type =
  struct
    type desc =
      | Int
      | String
      | Name of string

    and t = { sr: Flx_srcref.t; desc: desc }

    let rec print_desc ppf = function
      | Int -> print_variant0 ppf "Int"
      | String -> print_variant0 ppf "String"
      | Name s -> print_variant1 ppf "Name" print_string s

    and print ppf { sr=sr; desc=desc } =
      print_record2 ppf
        "sr" Flx_srcref.print sr
        "desc" print_desc desc
  end

module Literal =
  struct
    type t =
      | Int of string * Big_int.big_int
      | String of string

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
    type desc =
      | Literal of Literal.t
      | Tuple of t list
      | Name of string
      | Sum of t list
      | Product of t list

    and t = { sr: Flx_srcref.t; desc: desc }

    let rec print_desc ppf = function
      | Literal lit -> print_variant1 ppf "Literal" Literal.print lit
      | Tuple es -> print_variant1 ppf "Tuple" (Flx_list.print print) es
      | Name name -> print_variant1 ppf "Name" print_string name
      | Sum es -> print_variant1 ppf "Sum" (Flx_list.print print) es
      | Product es -> print_variant1 ppf "Product" (Flx_list.print print) es

    and print ppf { sr=sr; desc=desc } =
      print_record2 ppf
        "sr" Flx_srcref.print sr
        "desc" print_desc desc
  end

module Stmt =
  struct
    type desc =
      | Noop of string
      | Val of id_t * Type.t option * Expr.t option

    and t = { sr: Flx_srcref.t; desc: desc }

    let rec print_desc ppf = function
      | Noop s -> print_variant1 ppf "Noop" print_string s
      | Val (id,typ,expr) ->
          print_variant3 ppf "Val"
            print_string id
            (print_opt Type.print) typ
            (print_opt Expr.print) expr

    and print ppf { sr=sr; desc=desc } =
      print_record2 ppf
        "sr" Flx_srcref.print sr
        "desc" print_desc desc

  end
