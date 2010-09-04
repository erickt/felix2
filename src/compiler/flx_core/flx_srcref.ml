open Format
open Flx_format

(** type of a span between two positions in one file*)
type t =
  { filename:   string; (* filename *)
    start_line: int;    (* starting line number, 1 origin *)
    start_col:  int;    (* starting column, 1 origin *)
    end_line:   int;    (* ending line number, 1 origin *)
    end_col:    int     (* ending column, 1 origin *)
  }

let make filename start_line start_col end_line end_col =
  { filename; start_line; start_col; end_line; end_col }

let make_dummy filename = make filename 0 0 0 0

(** Generic source reference manipulation.
 *
 * Note the special hack of forgetting the second filename when creating a
 * range: the alternative would be to record a complete list of lines. *)
let dummy_sr = make_dummy "[flx_srcref] generated"

(** join the ranges of two source references. *)
let join sr1 sr2 =
  (* Make sure we're joining the srcrefs from the same file. *)
  assert (sr1.filename = sr2.filename);

  (* Figure out the starting line and col. *)
  let start_line, start_col =
    if sr1.start_line < sr2.start_line then
      sr1.start_line, sr1.start_col
    else if sr1.start_line > sr2.start_line then
      sr2.start_line, sr2.start_col
    else
      sr1.start_line, min sr1.start_col sr2.start_col
  in

  (* Figure out the ending line and col. *)
  let end_line, end_col =
    if sr1.end_line < sr2.end_line then
      sr2.end_line, sr2.end_col
    else if sr1.end_line > sr2.end_line then
      sr1.end_line, sr1.end_col
    else
      sr1.end_line, max sr1.end_col sr2.end_col
  in

  { filename=sr1.filename; start_line; start_col; end_line; end_col }

(** {6 Type specific operations} *)
let to_string { filename; start_line; start_col; end_line; end_col } =
  if start_line = end_line then
    Printf.sprintf "%s: line %d, cols %d to %d"
      filename start_line start_col end_col
  else
    Printf.sprintf "%s: line %d col %d to line %d col %d"
      filename start_line start_col end_line end_col

let print ppf { filename; start_line; start_col; end_line; end_col } =
  print_record5 ppf
    "filename" Flx_format.print_string filename
    "start_line" Format.pp_print_int start_line
    "start_col" Format.pp_print_int start_col
    "end_line" Format.pp_print_int end_line
    "end_col" Format.pp_print_int end_col
