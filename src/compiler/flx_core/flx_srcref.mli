(** {6 Routines to extract source reference from terms}
 *
 * Source reference manipulators. *)

(** type of a span between two positions in one file *)
type t =
  string * (* filename *)
  int * (* starting line number, 1 origin *)
  int * (* starting column, 1 origin *)
  int * (* ending line number, 1 origin *)
  int   (* ending column, 1 origin *)

val make:
  string * (* filename *)
  int * (* starting line number, 1 origin *)
  int * (* starting column, 1 origin *)
  int * (* ending line number, 1 origin *)
  int   (* ending column, 1 origin *)
  -> t

val make_dummy: string -> t

val rsrange: t -> t -> t

(** A dummy source reference. *)
val dummy_sr: t

(** Convert the source reference to a string. *)
val to_string : t -> string

(** Print the source reference to a pretty printer. *)
val print : Format.formatter -> t -> unit
