type name = string

module Type :
  sig
    type t

    type int_kind =
      | Int_int
      | Int_uint

    type node =
      | Unknown
      | Int of int_kind
      | String
      | Name of string
      | Tuple of t list

    (** Make a type. *)
    val make: sr:Flx_srcref.t -> node:node -> t

    (** Return the type's node. *)
    val node: t -> node

    (** Return the type's source reference. *)
    val sr: t -> Flx_srcref.t

    (** Make a literal integer. *)
    val int: sr:Flx_srcref.t -> kind:int_kind -> t

    (** Return the string type. *)
    val string: Flx_srcref.t -> t

    (** Return the unit type. *)
    val unit: Flx_srcref.t -> t

    (** Print a type. *)
    val print: Format.formatter -> t -> unit
  end

module Literal :
  sig
    type t

    type node =
      | Int of Type.int_kind * Big_int.big_int
      | String of string

    (** Make a literal. *)
    val make: node:node -> t

    (** Return the literal's node. *)
    val node: t -> node

    (** Make a literal integer. *)
    val int: kind:Type.int_kind -> num:Big_int.big_int -> t

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
      | Sum of t list
      | Product of t list
      | Lambda of Lambda.t

    (** Make an expression. *)
    val make: sr:Flx_srcref.t -> node:node -> t

    (** Return the expression's node. *)
    val node: t -> node

    (** Return the expression's source reference. *)
    val sr: t -> Flx_srcref.t

    (** Return the unit expression. *)
    val unit: sr:Flx_srcref.t -> t

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
    val make: ?default:Expr.t -> kind:kind -> name:name -> typ:Type.t -> t

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

    (** Print a param. *)
    val print: Format.formatter -> t -> unit
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

    (** Make a lambda. *)
    val make : kind -> Param.t list -> Type.t -> Stmt.t list -> t

    (** Print a lambda. *)
    val print : Format.formatter -> t -> unit
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
  end
