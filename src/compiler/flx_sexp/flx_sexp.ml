open Ocs_types
open Flx_format

type t =
  | Int of string
  | Str of string
  | Sym of string
  | Id of string
  | Lst of t list

(* Convert from ocs to an s-expression. *)
let rec of_ocs = function
  | Sunbound -> failwith "unmapped ocs type Sunbound"
  | Seof -> failwith "unmapped ocs type Seof"
  | Sreal _ -> failwith "unmapped ocs type Sreal"
  | Scomplex _ -> failwith "unmapped ocs type Scomplex"
  | Srational _ -> failwith "unmapped ocs type Srational"
  | Schar _ -> failwith "unmapped ocs type Schar"
  | Sport _ -> failwith "unmapped ocs type Sport"
  | Sprim _ -> failwith "unmapped ocs type Sprim"
  | Svalues _ -> failwith "unmapped ocs type Svalues"
  | Sesym _ -> failwith "unmapped ocs type Sesym"
  | Swrapped _ -> failwith "unmapped ocs type Swrapped"
  | Sunspec -> failwith "unmapped ocs type Sunspec"
  | Spromise _ -> failwith "unmapped ocs type Spromise"
  | Sproc _ -> failwith "unmapped ocs type Sproc"
  | Snull -> Lst []
  | Strue -> Id "true"
  | Sfalse -> Id "false"
  | Sstring s -> Str s
  | Ssymbol s -> Id s
  | Sint i -> Int (string_of_int i)
  | Sbigint i -> Int (Big_int.string_of_big_int i)
  | Spair _ as s -> Lst (List.map of_ocs (Ocs_misc.list_to_caml s))
  | Svector a -> Lst (List.map of_ocs (Array.to_list a))

(** Prints out the s-expression to the formatter. *)
let rec print ppf = function
  | Int s -> print_variant1 ppf "Int" print_string s
  | Str s -> print_variant1 ppf "Str" print_string s
  | Sym s -> print_variant1 ppf "Sym" print_string s
  | Id s -> print_variant1 ppf "Id" print_string s
  | Lst ss -> print_variant1 ppf "Lst" (Flx_list.print print) ss
