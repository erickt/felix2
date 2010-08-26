type name = string

module Type :
  sig
    type t

    type int_kind =
      | Int_int
      | Int_uint

    type node =
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

module Expr :
  sig
    type t

    type node =
      | Literal of Literal.t
      | Tuple of t list
      | Name of string
      | Sum of t list
      | Product of t list

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

module Stmt :
  sig
    type t

    type node =
      | Noop of string
      | Val of name * Type.t option * Expr.t option

    (** Make a statement. *)
    val make: sr:Flx_srcref.t -> node:node -> t

    (** Return the statement's node. *)
    val node: t -> node

    (** Return the statement's source reference. *)
    val sr: t -> Flx_srcref.t

    (** Print a statement. *)
    val print: Format.formatter -> t -> unit
  end