(** Interactive REPL driver code. *)

open Batteries
open Format

(* Preparse all the imported libraries. *)
let parse_imports parser_state init imports =
  let parser_state, stmts =
    List.fold_left begin fun (parser_state, init) name ->
      let parser_state, stmts =
        Flx_parse.parse_file
          ~include_dirs:!Options.include_dirs
          parser_state
          name
      in
      parser_state, stmts :: init
    end
    (parser_state, init)
    (List.rev imports)
  in
  parser_state, List.flatten stmts

(* Parse a statement and print it out. *)
let parse_stmt stmt stmts =
  match stmt with
  | None -> stmts
  | Some stmt ->
      printf "... PARSED: %a@." Flx_sexp.print stmt;
      stmt :: stmts

(* Parse stdin *)
let parse_stdin parser_state handle_stmt init =
  (* Create a buffer we'll save all our input to. *)
  let buffer = Buffer.create 512 in

  let lexbuf = Flx_parse.lexbuf_from_function 
    ~name:"<input>"
    begin fun s n ->
      (* Cache the line in our buffer so that we can do proper error
       * messages. *)
      let n = input stdin s 0 n in
      Buffer.add_substring buffer s 0 n;
      Format.printf "read: %S@." (String.sub s 0 n);
      n
    end
  in

  (* Loop over each statement until we exit. *)
  let rec aux parser_state init =
    printf ">>> @?";

    match
      begin try
        let parser_state, stmt = Flx_parse.toplevel_phrase
          parser_state
          lexbuf
        in
        Some (parser_state, handle_stmt stmt init)
      with
      | Flx_exceptions.Syntax_error ((_,l1,c1,l2,c2) as sr, e) ->
          printf "@.%s@." (Flx_srcref.to_string sr);
          printf "%s@." (Flx_io.get_lines
            (IO.input_string (Buffer.contents buffer))
            l1 c1 l2 c2);
          printf "Error: %s@." e;

          (* Ignore the rest of the line. *)
          Flx_parse.flush_input lexbuf;

          Some (parser_state, init)

      | IO.No_more_input ->
          None
      end
    with
    | None -> init
    | Some (parser_state, init) -> aux parser_state init
  in

  aux parser_state init

let main () =
  Options.parse_args "Usage: flxi <options> <files>\nOptions are:";

  (* Create the state needed for parsing. *)
  let parser_state = Flx_parse.make_parser_state () in

  (* Parse all the imported files. *)
  let parser_state, stmts = parse_imports parser_state [] !Options.imports in

  (* Parse stdin and compile the input. *)
  begin match !Options.phase with
  | Options.Parse ->
      ignore (parse_stdin parser_state parse_stmt stmts)
  end;

  (* Exit without error. *)
  0
;;

exit (main ())
