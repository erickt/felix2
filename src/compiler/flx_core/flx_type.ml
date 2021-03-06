open Format
open Flx_format

(** Raised when we encounter a type error. *)
exception Type_error of Flx_srcref.t * string

type name = string

module Type :
  sig
    type int_kind =
      | Int
      | Uint

    type t = private { node: node }

    and node =
      | Variable of int
      | Integer of int_kind
      | String
      | Name of string
      | Tuple of t list
      | Arrow of t * t

    (** Return a type variable. *)
    val variable: int -> t

    (** Return the int type. *)
    val integer: int_kind -> t

    (** Return the string type. *)
    val string: unit -> t

    (** Return an abstract named type. *)
    val name: string -> t

    (** Return the arrow type. *)
    val arrow: t -> t -> t

    (** Return a tuple type. *)
    val tuple: t list -> t

    (** Return the unit type. *)
    val unit: unit -> t

    (** Recursively map a function over a type. Depth first. *)
    val map: (t -> t) -> t -> t

    (** Recursively iterate a function over a type. Depth first. *)
    val iter: (t -> unit) -> t -> unit

    (** Print a type. *)
    val print: Format.formatter -> t -> unit

    (** Print an integer kind. *)
    val print_int_kind: Format.formatter -> int_kind -> unit

    (** Test the equality of two types. *)
    val equal: t -> t -> bool

    (** Compare two types. *)
    val compare: t -> t -> int

    (** Return a hash of the type. *)
    val hash: t -> int
  end = struct
    type int_kind =
      | Int
      | Uint

    type t = { node: node }

    and node =
      | Variable of int
      | Integer of int_kind
      | String
      | Name of string
      | Tuple of t list
      | Arrow of t * t

    (** Make a type. *)
    let make node = { node }

    (** Return a type variable. *)
    let variable var = make (Variable var)

    (** Return the int type. *)
    let integer kind = make (Integer kind)

    (** Return the string type. *)
    let string () = make String

    (** Return a type alias. *)
    let name name = make (Name name)

    (** Return a tuple type. *)
    let tuple typs = make (Tuple typs)

    (** Return the unit type. *)
    let unit () = tuple []

    (** Return the arrow type. *)
    let arrow lhs rhs = make (Arrow (lhs, rhs))

    (** Recursively map a function over a type. Depth first. *)
    let rec map f typ =
      match typ.node with
      | Variable _ -> f typ
      | Integer _ -> f typ
      | String -> f typ
      | Name _ -> f typ
      | Tuple ts -> f (tuple (List.map f ts))
      | Arrow (lhs, rhs) -> f (arrow (f lhs) (f rhs))

    (** Recursively iterate a function over a type. Depth first. *)
    let rec iter f typ =
      match typ.node with
      | Variable _ -> f typ
      | Integer _ -> f typ
      | String -> f typ
      | Name _ -> f typ
      | Tuple ts ->
          List.iter f ts;
          f typ
      | Arrow (lhs, rhs) ->
          f lhs;
          f rhs;
          f typ

    (** Print a type. *)
    let rec print ppf { node } =
      print_node ppf node

    (** Print a type node. *)
    and print_node ppf = function
      | Variable var -> print_variant1 ppf "Variable" pp_print_int var
      | Integer k -> print_variant1 ppf "Integer" print_int_kind k
      | String -> print_variant0 ppf "String"
      | Name s -> print_variant1 ppf "Name" print_string s
      | Tuple ts -> print_variant1 ppf "Tuple" (Flx_list.print print) ts
      | Arrow (lhs, rhs) -> print_variant2 ppf "Arrow" print lhs print rhs

    (** Print an integer kind. *)
    and print_int_kind ppf = function
      | Int -> print_variant0 ppf "Int"
      | Uint -> print_variant0 ppf "Uint"


    (** Test the equality of two types. *)
    let equal typ1 typ2 =
      let rec aux typ1 typ2 =
        match typ1.node, typ2.node with
        | Variable var1, Variable var2 -> var1 = var2
        | Integer kind1, Integer kind2 -> kind1 = kind2
        | String, String -> true
        | Name name1, Name name2 -> name1 = name2
        | Tuple ts1, Tuple ts2 -> List.for_all2 aux ts1 ts2
        | Arrow (lhs1, rhs1), Arrow (lhs2, rhs2) ->
            aux lhs1 lhs2 && aux rhs1 rhs2
        | _, _ -> false
      in
      try aux typ1 typ2 with Invalid_argument _ -> false

    (** Compare two types. *)
    let compare = Pervasives.compare

    (** Return a hash of the type. *)
    let hash = Hashtbl.hash
  end

module Literal =
  struct
    type int_kind = Type.int_kind

    type t = node

    and node =
      | Integer of Type.int_kind * Big_int.big_int
      | String of string

    (** Make a literal. *)
    let make node = node

    (** Return the literal's node. *)
    let node literal = literal

    (** Return the literal's type. *)
    let typ = function
      | Integer (kind, _) -> Type.integer kind
      | String _ -> Type.string ()

    (** Make a literal integer. *)
    let integer kind num = make (Integer (kind, num))

    (** Make a literal 32 bit integer *)
    let int32 num = make (Integer (Type.Int, Big_int.big_int_of_int32 num))

    (** Make a literal string. *)
    let string s = make (String s)

    (** Print a literal. *)
    let print ppf = function
      | Integer (k,i) ->
          print_variant2 ppf "Integer"
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
    val tuple: ?sr:Flx_srcref.t -> Expr.t list -> t

    (** Return the unit expression. *)
    val unit: ?sr:Flx_srcref.t -> unit -> t

    (** Return a lambda expression. *)
    val lambda: ?sr:Flx_srcref.t -> Lambda.t -> t

    (** Print an expression. *)
    val print: Format.formatter -> t -> unit
  end = struct
    type t = { sr: Flx_srcref.t; typ: Type.t; node: node }

    and node =
      | Literal of Literal.t
      | Tuple of t list
      | Name of string
      | Sum of t * t
      | Product of t * t
      | Lambda of Lambda.t

    (** Make an expression. *)
    let make ?(sr=Flx_srcref.dummy_sr) typ node = { sr; typ; node }

    (** Return the expression's source reference. *)
    let sr { sr } = sr

    (** Return the expression's type. *)
    let typ { typ } = typ

    (** Return the expression's node. *)
    let node { node } = node

    (** Return a literal expression. *)
    let literal ?sr literal = make ?sr (Literal.typ literal) (Literal literal)

    (** Return a tuple expression. *)
    let tuple ?sr es = make ?sr (Type.tuple (List.map typ es)) (Tuple es)

    (** Return the unit expression. *)
    let unit ?sr () = tuple ?sr []

    (** Return a lambda expression. *)
    let lambda ?sr lambda = make ?sr (Lambda.typ lambda) (Lambda lambda)

    (** Print an expression's node. *)
    let rec print_node ppf = function
      | Literal lit -> print_variant1 ppf "Literal" Literal.print lit
      | Tuple es -> print_variant1 ppf "Tuple" (Flx_list.print print) es
      | Name name -> print_variant1 ppf "Name" print_string name
      | Sum (lhs,rhs) -> print_variant2 ppf "Sum" print lhs print rhs
      | Product (lhs,rhs) -> print_variant2 ppf "Product" print lhs print rhs
      | Lambda lambda -> print_variant1 ppf "Lambda" Lambda.print lambda

    (** Print an expression. *)
    and print ppf { sr; node; typ } =
      (*
      print_record3 ppf
        "sr" Flx_srcref.print sr
      *)
      print_record2 ppf
        "typ" Type.print typ
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
    val make: kind:kind -> name:name -> typ:Type.t -> default:Expr.t option -> t

    (** Print a parameter. *)
    val print: Format.formatter -> t -> unit
  end = struct
    type kind = Val

    type t = {
      kind: kind;
      name: name;
      typ: Type.t;
      default: Expr.t option }

    (** Make a parameter. *)
    let make ~kind ~name ~typ ~default = { kind; name; typ; default }

    (** Print a parameter kind. *)
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

    (** Return the param's type. *)
    val typ : t -> Type.t

    (** Print a param. *)
    val print: Format.formatter -> t -> unit
  end = struct
    type t = {
      parameters: Parameter.t list;
      precondition: Expr.t option }

    (** Make a param. *)
    let make ?precondition parameters = { parameters; precondition }

    (** Return the param's type. *)
    let typ { parameters } =
      match parameters with
      | [parameter] -> parameter.Parameter.typ
      | _ -> Type.tuple (List.map (fun p -> p.Parameter.typ) parameters)

    (** Print a param. *)
    let print ppf { parameters; precondition } =
      print_record2 ppf
        "parameters" (Flx_list.print Parameter.print) parameters
        "precondition" (print_opt Expr.print) precondition
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
  end = struct
    type kind =
      | Function

    type t = {
      kind: kind;
      params: Param.t list;
      return_typ: Type.t;
      stmts: Stmt.t list }

    (** Make a lambda. *)
    let make kind params return_typ stmts =
      { kind; params; return_typ; stmts }

    (** Return the lambda's type. *)
    let typ { params; return_typ } =
      List.fold_right begin fun param typ ->
        Type.arrow (Param.typ param) typ
      end
      params
      return_typ

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
      | Value of name * Expr.t
      | GlobalVariable of name * Expr.t
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

    (** Return a global variable statement. *)
    val global_variable: ?sr:Flx_srcref.t -> name -> Expr.t -> t

    (** Return a curry-able function definition statement. *)
    val curry: ?sr:Flx_srcref.t -> name -> Lambda.t -> t

    (** Return a return statement. *)
    val return: ?sr:Flx_srcref.t -> Expr.t -> t

    (** Print a statement. *)
    val print: Format.formatter -> t -> unit
  end = struct
    type t = { sr: Flx_srcref.t; typ: Type.t; node: node; }

    and node =
      | Noop of string
      | Value of name * Expr.t
      | GlobalVariable of name * Expr.t
      | Curry of name * Lambda.t
      | Return of Expr.t

    (** Make a statement. *)
    let make ?(sr=Flx_srcref.dummy_sr) typ node = { sr; typ; node }

    (** Return the statement's source reference. *)
    let sr { sr } = sr

    (** Return the statement's type. *)
    let typ { typ } = typ

    (** Return the statement's node. *)
    let node { node } = node

    (** Return a value definition statement. *)
    let value ?sr name expr = make ?sr (Expr.typ expr) (Value (name, expr))

    (** Return a global variable statement. *)
    let global_variable ?sr name expr =
      make ?sr (Expr.typ expr) (GlobalVariable (name, expr))

    (** Return a no-op statement. *)
    let noop ?sr s = make ?sr (Type.unit ()) (Noop s)

    (** Return a curry-able function declaration. *)
    let curry ?sr name lambda =
      make ?sr (Lambda.typ lambda) (Curry (name, lambda))

    (** Return a return statement. *)
    let return ?sr expr = make ?sr (Expr.typ expr) (Return expr)

    (** Print a statement node. *)
    let rec print_node ppf = function
      | Noop s -> print_variant1 ppf "Noop" print_string s
      | Value (name,expr) ->
          print_variant2 ppf "Value"
            print_string name
            Expr.print expr
      | GlobalVariable (name,expr) ->
          print_variant2 ppf "GlobalVariable"
            print_string name
            Expr.print expr
      | Curry (name,lambda) ->
          print_variant2 ppf "Curry"
            print_string name
            Lambda.print lambda
      | Return expr ->
          print_variant1 ppf "Return"
            Expr.print expr

    (** Print a statement. *)
    and print ppf { sr; node; typ } =
      (*
      print_record3 ppf
        "sr" Flx_srcref.print sr
        *)
      print_record2 ppf
        "node" print_node node
        "typ" Type.print typ
  end
