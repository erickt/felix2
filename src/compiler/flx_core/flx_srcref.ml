(** type of a span between two positions in one file*)
type t =
  string * (* filename *)
  int * (* starting line number, 1 origin *)
  int * (* starting column, 1 origin *)
  int * (* ending line number, 1 origin *)
  int   (* ending column, 1 origin *)

let make srcref = srcref

let make_dummy name = make (name, 0, 0, 0, 0)

(** Generic source reference manipulation.
 *
 * Note the special hack of forgetting the second filename when creating a
 * range: the alternative would be to record a complete list of lines. *)

let dummy_sr = make_dummy "[flx_srcref] generated"

(** get range from first and last ranges *)
let rsrange (f1,sl1,sc1,_,_) (_,_,_,el2,ec2) =
  (f1,sl1,sc1,el2,ec2)

(** {6 Type specific operations} *)

let to_string (f,l1,c1,l2,c2) =
  if l1 = l2
  then
    f ^ ": line " ^ string_of_int l1 ^
    ", cols " ^ string_of_int c1 ^ " to " ^ string_of_int c2
  else
    f ^ ": line " ^ string_of_int l1 ^
    " col " ^ string_of_int c1 ^ " to " ^
    " line " ^ string_of_int l2 ^ " col " ^ string_of_int c2

let print ppf (f,l1,c1,l2,c2) =
  Flx_format.print_tuple5 ppf
    Flx_format.print_string f
    Format.pp_print_int l1
    Format.pp_print_int c1
    Format.pp_print_int l2
    Format.pp_print_int c2
