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
  handle_stmt : Flx_sexp.t -> unit;
  pcounter : int ref;
  env : Ocs_types.env;
  pdebug : bool ref;
}

(** An empty domain-specific sub-language. *)
let empty_dssl =
  { prios = [];
    rules = [];
    deps = [];
    privacy = Map.StringMap.empty; }

(** Return the string name of the token. *)
let name_of_token = function
  | DUMMY -> "DUMMY"
  | NAME _ -> "NAME"
  | NONTERMINAL _ -> "NONTERMINAL"
  | INTEGER _ -> "INTEGER"
  | FLOAT _ -> "FLOAT"
  | STRING _ -> "STRING"
  | CSTRING _ -> "CSTRING"
  | FSTRING _ -> "FSTRING"
  | QSTRING _ -> "QSTRING"
  | WSTRING _ -> "WSTRING"
  | USTRING _ -> "USTRING"
  | USER_KEYWORD s -> s
  | HASH_INCLUDE_FILES _ -> "HASH_INCLUDE_FILES"
  | QUEST _ -> "QUEST"
  | LPAR -> "LPAR"
  | RPAR -> "RPAR"
  | LSQB -> "LSQB"
  | RSQB -> "RSQB"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | COMMA -> "COMMA"
  | PLUS -> "PLUS"
  | STAR -> "STAR"
  | VBAR -> "VBAR"
  | LESS -> "LESS"
  | GREATER -> "GREATER"
  | EQUAL -> "EQUAL"
  | EQEQUAL -> "EQEQUAL"
  | NOTEQUAL -> "NOTEQUAL"
  | LESSEQUAL -> "LESSEQUAL"
  | GREATEREQUAL -> "GREATEREQUAL"
  | UNDERSCORE -> "UNDERSCORE"
  | NEWLINE -> "NEWLINE"
  | ENDMARKER -> "ENDMARKER"
  | ERRORTOKEN _ -> "ERRORTOKEN"
  | SLOSH -> "SLOSH"

let string_of_string s = "\"" ^  Flx_string.c_quote_of_string s ^ "\""

let sp p = match p with
  | `No_prio -> ""
  | `Eq_prio p -> "[=" ^ p ^ "]"
  | `Less_prio p -> "[<" ^p ^ "]"
  | `Lesseq_prio p -> "[<=" ^ p ^ "]"
  | `Greater_prio p -> "[>" ^ p ^ "]"
  | `Greatereq_prio p -> "[>=" ^ p ^ "]"

(** Return the string represenation of the token. *)
let string_of_token = function
  | DUMMY -> "DUMMY"
  | NAME s -> s
  | NONTERMINAL (s,p) -> s ^ sp p
  | INTEGER (t,i) -> Big_int.string_of_big_int i
  | FLOAT (t,v) -> v
  | STRING s -> Flx_string.c_quote_of_string s
  | CSTRING s -> Flx_string.c_quote_of_string s
  | FSTRING s -> Flx_string.c_quote_of_string s
  | QSTRING s -> Flx_string.c_quote_of_string s
  | WSTRING s -> Flx_string.c_quote_of_string s
  | USTRING s -> Flx_string.c_quote_of_string s
  | USER_KEYWORD s -> s
  | HASH_INCLUDE_FILES fs -> "include_files(" ^ String.concat "," fs ^ ")"
  | QUEST _ -> "?"
  | LPAR _ -> "("
  | RPAR _ -> ")"
  | LSQB _ -> "["
  | RSQB _ -> "]"
  | LBRACE _ -> "{"
  | RBRACE _ -> "}"
  | COMMA _ -> ","
  | PLUS _ -> "+"
  | STAR _ -> "*"
  | VBAR _ -> "|"
  | LESS _ -> "<"
  | GREATER _ -> ">"
  | EQUAL _ -> "="
  | EQEQUAL _ -> "=="
  | NOTEQUAL _ -> "!="
  | LESSEQUAL _ -> "<="
  | GREATEREQUAL _ -> ">="
  | UNDERSCORE _ -> "_"
  | NEWLINE -> "<NEWLINE>"
  | ENDMARKER -> "<<EOF>>"
  | ERRORTOKEN s -> "<<ERROR '"^ s ^"'>>"
  | SLOSH -> "\\"
