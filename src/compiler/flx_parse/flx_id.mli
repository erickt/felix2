(** Internationalised identifier support *)

val ucs_id_ranges : (int * int) list
val utf8_to_ucn : Flx_srcref.t -> string -> string
