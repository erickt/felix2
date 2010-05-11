open Batteries

type anote_t = string

type ntprio_t = [
  | `No_prio
  | `Eq_prio of string
  | `Less_prio of string
  | `Lesseq_prio of string
  | `Greater_prio of string
  | `Greatereq_prio of string
]

type token_t =
  | ERRORTOKEN of string
  | ENDMARKER
  | NEWLINE
  | SLOSH
  | NAME of string
  | NONTERMINAL of (string * ntprio_t)
  | INTEGER of (string * Big_int.big_int)
  | FLOAT of (string * string)
  | STRING of string
  | CSTRING of string
  | FSTRING of string
  | QSTRING of string
  | WSTRING of string
  | USTRING of string
  | USER_KEYWORD of string
  | HASH_INCLUDE_FILES of string list
  | DUMMY
  | QUEST
  | LPAR
  | RPAR
  | LSQB
  | RSQB
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | STAR
  | VBAR
  | LESS
  | GREATER
  | EQUAL
  | EQEQUAL
  | NOTEQUAL
  | LESSEQUAL
  | GREATEREQUAL
  | UNDERSCORE

type prio_t = [`Default | `Priority of string]

type rule_t = string * prio_t * token_t list * string * anote_t * Flx_srcref.t

type dssl_t = {
  prios : string list list;
  rules : rule_t list;
  deps : string list;
  privacy : string Map.StringMap.t; (* string -> string *)
}

type local_data_t = {
  dssls : dssl_t Map.StringMap.t;
  loaded_dssls : string list;
  scm : (Flx_srcref.t * string) list;
}

type global_data_t = {
  handle_stmt : Sex_types.t -> unit;
  pcounter : int ref;
  env : Ocs_types.env;
  pdebug : bool ref;
}

(** An empty domain-specific sub-language. *)
val empty_dssl : dssl_t

(** Return the string name of the token. *)
val name_of_token : token_t -> string

(** Return the string name of the token. *)
val string_of_token : token_t -> string
