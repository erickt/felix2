open Format
open Flx_format

type name = string

module Type =
  struct
    type int_kind =
      | Int_int
      | Int_uint

    type t = { sr: Flx_srcref.t; node: node }

    and node =
      | Unknown
      | Int of int_kind
      | String
      | Name of string
      | Tuple of t list

    (** Make a type. *)
    let make ~sr ~node = { sr; node }

    (** Return the type's node. *)
    let node { node } = node

    (** Return the type's source reference. *)
    let sr { sr } = sr

    (** Return the int type. *)
    let int ~sr ~kind = make ~sr ~node:(Int kind)

    (** Return the string type. *)
    let string sr = make ~sr ~node:String

    (** Return the unit type. *)
    let unit sr = make ~sr ~node:(Tuple [])

    let rec print_node ppf = function
      | Unknown -> print_variant0 ppf "Unknown"
      | Int k -> print_variant1 ppf "Int" print_int_kind k
      | String -> print_variant0 ppf "String"
      | Name s -> print_variant1 ppf "Name" print_string s
      | Tuple ts -> print_variant1 ppf "Tuple" (Flx_list.print print) ts

    and print_int_kind ppf = function
      | Int_int -> print_variant0 ppf "Int_int"
      | Int_uint -> print_variant0 ppf "Int_uint"

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
      | Int of Type.int_kind * Big_int.big_int
      | String of string

    (** Make a literal. *)
    let make ~node = node

    (** Return the literal's node. *)
    let node literal = literal

    (** Make a literal integer. *)
    let int ~kind ~num = make ~node:(Int (kind, num))

    (** Make a literal string. *)
    let string s = make ~node:(String s)

    (** Print a literal. *)
    let print ppf = function
      | Int (k,i) ->
          print_variant2 ppf "Int"
            Type.print_int_kind k
            print_big_int i
      | String s ->
          print_variant1 ppf "String"
            print_string s
  end

module rec Expr :
  sig
    type t

    type node =
      | Literal of Literal.t
      | Tuple of t list
      | Name of string
      | Sum of t list
      | Product of t list
      | Lambda of Lambda.t

    (** Make an expression. *)
    val make: sr:Flx_srcref.t -> node:node -> t

    (** Return the expression's node. *)
    val node: t -> node

    (** Return the expression's source reference. *)
    val sr: t -> Flx_srcref.t

    (** Return a tuple expression. *)
    val tuple: sr:Flx_srcref.t -> t list -> t

    (** Return the unit expression. *)
    val unit: sr:Flx_srcref.t -> t

    (** Print an expression. *)
    val print: Format.formatter -> t -> unit
  end = struct
    type t = { sr: Flx_srcref.t; node: node }

    and node =
      | Literal of Literal.t
      | Tuple of t list
      | Name of string
      | Sum of t list
      | Product of t list
      | Lambda of Lambda.t

    (** make an expression. *)
    let make ~sr ~node = { sr; node }

    (** return the expression's node. *)
    let node { node } = node

    (** return the expression's source reference. *)
    let sr { sr } = sr

    (** Return a tuple expression. *)
    let tuple ~sr exprs = make ~sr ~node:(Tuple exprs)

    (** Return the unit expression. *)
    let unit ~sr = tuple ~sr []

    let rec print_node ppf = function
      | Literal lit -> print_variant1 ppf "Literal" Literal.print lit
      | Tuple es -> print_variant1 ppf "Tuple" (Flx_list.print print) es
      | Name name -> print_variant1 ppf "Name" print_string name
      | Sum es -> print_variant1 ppf "Sum" (Flx_list.print print) es
      | Product es -> print_variant1 ppf "Product" (Flx_list.print print) es
      | Lambda lambda -> print_variant1 ppf "Lambda" Lambda.print lambda

    (** Print an expression. *)
    and print ppf { sr; node } =
      print_record2 ppf
        "sr" Flx_srcref.print sr
        "node" print_node node
  end

and Parameter :
  sig
    type kind = Val

    type t = private {
      kind: kind;
      name: name;
      typ: Type.t;
      default: Expr.t option }

    (** Make a parameter. *)
    val make: ?default:Expr.t -> kind:kind -> name:name -> typ:Type.t -> t

    (** Print a parameter. *)
    val print: Format.formatter -> t -> unit
  end = struct
    type kind = Val

    type t = {
      kind: kind;
      name: name;
      typ: Type.t;
      default: Expr.t option }

    let make ?default ~kind ~name ~typ = { kind; name; typ; default }

    let print_kind ppf = function
      | Val -> print_variant0 ppf "Val"

    (** Print a parameter. *)
    let print ppf { kind; name; typ; default } =
      print_record4 ppf
        "kind" print_kind kind
        "name" print_string name
        "typ" Type.print typ
        "default" (print_opt Expr.print) default
  end

(** A param is a set of curry-able parameters that can have a precondition test
 * set on them. *)
and Param :
  sig
    type t = private {
      parameters: Parameter.t list;
      precondition: Expr.t option }

    (** Make a param. *)
    val make : ?precondition:Expr.t -> Parameter.t list -> t

    (** Print a param. *)
    val print: Format.formatter -> t -> unit
  end = struct
    type t = { parameters: Parameter.t list; precondition: Expr.t option }

    let make ?precondition parameters = { parameters; precondition }

    let print ppf { parameters; precondition } =
      print_record2 ppf
        "parameters" (Flx_list.print Parameter.print) parameters
        "precondition" (print_opt Expr.print) precondition
  end

and Lambda :
  sig
    type kind =
      | Function

    type t = {
      kind: kind;
      params: Param.t list;
      return_typ: Type.t;
      stmts: Stmt.t list }

    val make : kind -> Param.t list -> Type.t -> Stmt.t list -> t

    (** Print a lambda. *)
    val print : formatter -> t -> unit
  end = struct
    type kind =
      | Function

    type t = {
      kind: kind;
      params: Param.t list;
      return_typ: Type.t;
      stmts: Stmt.t list }

    (** Make a lambda. *)
    let make kind params return_typ stmts = { kind; params; return_typ; stmts }

    (** Print a function kind. *)
    let print_kind ppf = function
      | Function -> print_variant0 ppf "Function"

    (** Print a lambda. *)
    let print ppf { kind; params; return_typ; stmts } =
      print_record4 ppf
        "kind" print_kind kind
        "params" (Flx_list.print Param.print) params
        "return_typ" Type.print return_typ
        "stmts" (Flx_list.print Stmt.print) stmts
  end

and Stmt :
  sig
    type t

    type node =
      | Noop of string
      | Val of name * Type.t option * Expr.t option
      | Curry of name * Lambda.t
      | Return of Expr.t

    (** Make a statement. *)
    val make: sr:Flx_srcref.t -> node:node -> t

    (** Return the statement's node. *)
    val node: t -> node

    (** Return the statement's source reference. *)
    val sr: t -> Flx_srcref.t

    (** Print a statement. *)
    val print: Format.formatter -> t -> unit
  end = struct
    type t = { sr: Flx_srcref.t; node: node }

    and node =
      | Noop of string
      | Val of name * Type.t option * Expr.t option
      | Curry of name * Lambda.t
      | Return of Expr.t

    (** make a statement. *)
    let make ~sr ~node = { sr; node }

    (** return the statement's node. *)
    let node { node } = node

    (** return the statement's source reference. *)
    let sr { sr } = sr

    let rec print_node ppf = function
      | Noop s -> print_variant1 ppf "Noop" print_string s
      | Val (name,typ,expr) ->
          print_variant3 ppf "Val"
            print_string name
            (print_opt Type.print) typ
            (print_opt Expr.print) expr
      | Curry (name,lambda) ->
          print_variant2 ppf "Curry"
            print_string name
            Lambda.print lambda
      | Return expr ->
          print_variant1 ppf "Return" Expr.print expr

    (** Print a statement. *)
    and print ppf { sr; node } =
      print_record2 ppf
        "sr" Flx_srcref.print sr
        "node" print_node node
  end
