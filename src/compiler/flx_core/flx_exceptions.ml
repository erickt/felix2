exception Lex_error of Flx_srcref.t * string
exception Parse_error of Flx_srcref.t * string
exception Syntax_error of Flx_srcref.t * string
exception Client_error of Flx_srcref.t list * string

let lex_error sr format =
  Format.ksprintf (fun s -> raise (Lex_error (sr, s))) format

let parse_error sr format =
  Format.ksprintf (fun s -> raise (Parse_error (sr, s))) format

let syntax_error sr format =
  Format.ksprintf (fun s -> raise (Syntax_error (sr, s))) format

let client_errorN srs format =
  Format.ksprintf (fun s -> raise (Client_error (srs, s))) format

let client_error sr format = client_errorN [sr] format

let client_error2 sr1 sr2 format = client_errorN [sr1; sr2] format
