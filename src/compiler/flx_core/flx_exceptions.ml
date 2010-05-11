exception LexError of string
exception ParseError of string
exception ClientError of Flx_srcref.t list * string

let lex_error format =
  Format.ksprintf (fun s -> raise (LexError s)) format

let parse_error format =
  Format.ksprintf (fun s -> raise (ParseError s)) format

let client_errorN srs format =
  Format.ksprintf (fun s -> raise (ClientError (srs, s))) format

let client_error sr format = client_errorN [sr] format

let client_error2 sr1 sr2 format = client_errorN [sr1; sr2] format
