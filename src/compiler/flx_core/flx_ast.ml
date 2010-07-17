open Format
open Flx_format

type id_t = string

module Type =
  struct
    type t =
      | Int of Flx_srcref.t
      | String of Flx_srcref.t
      | Name of Flx_srcref.t * string

    let print ppf = function
      | Int sr ->
          print_variant1 ppf "Int"
            Flx_srcref.print sr
      | String sr ->
          print_variant1 ppf "String"
            Flx_srcref.print sr
      | Name (sr,s) ->
          print_variant2 ppf "Name"
            Flx_srcref.print sr
            print_string s
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
    type t =
      | Literal of Flx_srcref.t * Literal.t
      | Tuple of Flx_srcref.t * t list
      | Name of Flx_srcref.t * string
      | Sum of Flx_srcref.t * t list

    let rec print ppf = function
      | Literal (sr,lit) ->
          print_variant2 ppf "Literal"
            Flx_srcref.print sr
            Literal.print lit
      | Tuple (sr, es) ->
          print_variant2 ppf "Tuple"
            Flx_srcref.print sr
            (Flx_list.print print) es
      | Name (sr,name) ->
          print_variant2 ppf "Name"
            Flx_srcref.print sr
            print_string name
      | Sum (sr,es) ->
          print_variant2 ppf "Sum"
            Flx_srcref.print sr
            (Flx_list.print print) es
  end

module Stmt =
  struct
    type t =
      | Noop of Flx_srcref.t * string
      | Val of Flx_srcref.t * id_t * Type.t option * Expr.t option

    let print ppf = function
      | Noop (sr,s) ->
          print_variant2 ppf "Noop"
            Flx_srcref.print sr
            print_string s
      | Val (sr,id,typ,expr) ->
          print_variant4 ppf "Val"
            Flx_srcref.print sr
            print_string id
            (print_opt Type.print) typ
            (print_opt Expr.print) expr
  end
