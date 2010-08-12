(** A wrapper function to profile a function call. *)
val call :
  string ->         (** The name of the profile. *)
  (unit -> 'a) ->   (** The function. *)
  'a

(** Print out our gathered statistics. *)
val print : Format.formatter -> unit
