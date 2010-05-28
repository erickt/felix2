type parser_state_t

type lexbuf

(** Create a new parser state. *)
val make_parser_state: unit -> parser_state_t

(** Make a lexbuf from a channel. *)
val lexbuf_from_channel:
  ?name:string ->       (** The optional name for this channel. *)
  in_channel ->         (** The channel to parse. *)
  lexbuf

(** Make a lexbuf from a string. *)
val lexbuf_from_string:
  ?name:string ->       (** The optional name for this string. *)
  string ->             (** The string to parse. *)
  lexbuf

(** Make a lexbuf from a function. *)
val lexbuf_from_function:
  ?name:string ->           (** The optional name for this string. *)
  (string -> int -> int) -> (** The lexer function generator. *)
  lexbuf

(** Clear the lexbuf. *)
val flush_input: lexbuf -> unit

(** Parse a lexbuf and return the new parser state. *)
val parse_lexbuf:
  parser_state_t ->      (** The state for the felix parser. *)
  lexbuf ->
  parser_state_t

(** Parse a file and return the new parser state. *)
val parse_file:
  ?include_dirs: string list ->
  parser_state_t ->  (** The state for the felix parser. *)
  string ->          (** The filename to parse. *)
  parser_state_t
