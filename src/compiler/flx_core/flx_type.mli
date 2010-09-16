exception Type_error of Flx_srcref.t * string

type name = string

module Type :
  sig
    type int_kind =
      | Int
      | Uint

    type t = private { sr: Flx_srcref.t; node: node }

    and node =
      | Variable of int
      | Integer of int_kind
      | String
      | Name of string
      | Tuple of t list
      | Arrow of t * t

    (** Make a type. *)
    val make: ?sr:Flx_srcref.t -> node -> t

    (** Return a type variable. *)
    val variable: ?sr:Flx_srcref.t -> int -> t

    (** Return the int type. *)
    val integer: ?sr:Flx_srcref.t -> int_kind -> t

    (** Return the string type. *)
    val string: ?sr:Flx_srcref.t -> unit -> t

    (** Return a type alias. *)
    val name: ?sr:Flx_srcref.t -> string -> t

    (** Return the arrow type. *)
    val arrow: ?sr:Flx_srcref.t -> t -> t -> t

    (** Return a tuple type. *)
    val tuple: ?sr:Flx_srcref.t -> t list -> t

    (** Return the unit type. *)
    val unit: ?sr:Flx_srcref.t -> unit -> t

    (** Recursively map a function over a type. Depth first. *)
    val map: (t -> t) -> t -> t

    (** Recursively iterate a function over a type. Depth first. *)
    val iter: (t -> unit) -> t -> unit

    (** Print a type. *)
    val print: Format.formatter -> t -> unit

    (** Test the equality of two types. *)
    val equals: t -> t -> bool
  end

module Literal :
  sig
    type int_kind = Type.int_kind

    type t

    type node =
      | Integer of int_kind * Big_int.big_int
      | String of string

    (** Make a literal. *)
    val make: node -> t

    (** Return the literal's node. *)
    val node: t -> node

    (** Make a literal integer. *)
    val integer: int_kind -> Big_int.big_int -> t

    (** Make a literal string. *)
    val string: string -> t

    (** Print a literal. *)
    val print: Format.formatter -> t -> unit
  end

module rec Expr :
  sig
    type t

    type node =
      | Literal of Literal.t
      | Tuple of t list
      | Name of string
      | Sum of t * t
      | Product of t * t
      | Lambda of Lambda.t

    (** Make an expression. *)
    val make: ?sr:Flx_srcref.t -> Type.t -> node -> t

    (** Return the expression's source reference. *)
    val sr: t -> Flx_srcref.t

    (** Return the expression's type. *)
    val typ: t -> Type.t

    (** Return the expression's node. *)
    val node: t -> node

    (** Return a literal expression. *)
    val literal: ?sr:Flx_srcref.t -> Literal.t -> t

    (** Return a tuple expression. *)
    val tuple: ?sr:Flx_srcref.t -> t list -> t

    (** Return the unit expression. *)
    val unit: ?sr:Flx_srcref.t -> unit -> t

    (** Return a lambda expression. *)
    val lambda: ?sr:Flx_srcref.t -> Lambda.t -> t

    (** Print an expression. *)
    val print: Format.formatter -> t -> unit
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
    val make: kind:kind -> name:name -> typ:Type.t -> default:Expr.t option -> t

    (** Print a parameter. *)
    val print: Format.formatter -> t -> unit
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

    (** Return the param's type. *)
    val typ : t -> Type.t

    (** Print a param. *)
    val print: Format.formatter -> t -> unit
  end

and Lambda :
  sig
    type kind =
      | Function

    type t = private {
      kind: kind;
      params: Param.t list;
      return_typ: Type.t;
      stmts: Stmt.t list }

    (** Make a lambda. *)
    val make : kind -> Param.t list -> Type.t -> Stmt.t list -> t

    (** Return the lambda's type. *)
    val typ : t -> Type.t

    (** Print a lambda. *)
    val print : Format.formatter -> t -> unit
  end

and Stmt :
  sig
    type t

    type node =
      | Noop of string
      | Value of name * Expr.t
      | Curry of name * Lambda.t
      | Return of Expr.t

    (** Make a statement. *)
    val make: ?sr:Flx_srcref.t -> Type.t -> node -> t

    (** Return the statement's node. *)
    val node: t -> node

    (** Return the statement's source reference. *)
    val sr: t -> Flx_srcref.t

    (** Return the statement's type. *)
    val typ: t -> Type.t

    (** Return a no-op statement. *)
    val noop: ?sr:Flx_srcref.t -> string -> t

    (** Return a value definition statement. *)
    val value: ?sr:Flx_srcref.t -> name -> Expr.t -> t

    (** Return a curry-able function definition statement. *)
    val curry: ?sr:Flx_srcref.t -> name -> Lambda.t -> t

    (** Return a return statement. *)
    val return: ?sr:Flx_srcref.t -> Expr.t -> t

    (** Print a statement. *)
    val print: Format.formatter -> t -> unit
  end
