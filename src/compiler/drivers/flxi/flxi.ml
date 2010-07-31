(** Interactive REPL driver code. *)

open Batteries
open Format

open Flx_ast

(* Preparse all the imported libraries. *)
let parse_imports parser_state imports handle_stmt =
  let parser_state =
    List.fold_left begin fun parser_state name ->
      let parser_state, stmts = Flx_parse.parse_file
        ~include_dirs:!Options.include_dirs
        parser_state
        name
      in
      List.iter (handle_stmt ~print:false) stmts;
      parser_state
    end
    parser_state
    (List.rev imports)
  in
  parser_state


(* Parse the channel *)
let parse_channel ~name ~print parser_state handle_stmt channel args =
  (* Create a buffer we'll save all our input to. *)
  let buffer = Buffer.create 512 in

  let lexbuf = Flx_parse.lexbuf_from_function ~name
    begin fun s n ->
      (* Cache the line in our buffer so that we can do proper error
       * messages. *)
      let n = input channel s 0 n in
      Buffer.add_substring buffer s 0 n;
      n
    end
  in

  (* Loop over each statement until we exit. *)
  let rec aux parser_state =
    if print then printf ">>> @?";

    match
      begin try
        let parser_state, stmt = Flx_parse.toplevel_phrase
          parser_state
          lexbuf
        in
        handle_stmt ~print:true stmt;

        Some parser_state
      with
      | Flx_exceptions.Syntax_error ((_,l1,c1,l2,c2) as sr, e) ->
          printf "@.%s@." (Flx_srcref.to_string sr);
          printf "%s@." (Flx_io.get_lines
            (IO.input_string (Buffer.contents buffer))
            l1 c1 l2 c2);
          printf "Error: %s@." e;

          (* Ignore the rest of the line. *)
          Flx_parse.flush_input lexbuf;

          Some parser_state

      | Flx_sexp.Sexp_error (sexp, e) ->
          printf "Error: %s: %a@." e Flx_sexp.print sexp;

          (* Ignore the rest of the line. *)
          Flx_parse.flush_input lexbuf;

          Some parser_state

      | IO.No_more_input ->
          None
      end
    with
    | None -> ()
    | Some parser_state -> aux parser_state
  in

  aux parser_state



(* Parse a scheme block and print it out. *)
let print_scheme ~print ocs =
  if print then printf "... PARSED: %s@." (Ocs_print.string_of_ocs ocs);
  ()

(* Parse a sexp block and print it out. *)
let print_sexp ~print ocs =
  if print then printf "... PARSED: %a@." Flx_sexp.print (Flx_sexp.of_ocs ocs);
  ()

(* Parse a s-expression and print it out. *)
let print_ast ~print ocs =
  let sexp = Flx_sexp.of_ocs ocs in
  if print then printf "... PARSED: %a@." Stmt.print (Flx_sexp.to_stmt sexp);
  ()

let main () =
  Options.parse_args "Usage: flxi <options> <files>\nOptions are:";

  (* Create the state needed for parsing. *)
  let parser_state = Flx_parse.make_parser_state () in

  let handle_stmt =
    match !Options.phase with
    | Options.Parse_scheme -> print_scheme
    | Options.Parse_sexp -> print_sexp
    | Options.Parse_ast -> print_ast
  in

  (* Parse all the imported files. *)
  let parser_state = parse_imports
    parser_state
    !Options.imports
    handle_stmt
  in

  begin match !Options.args with
  | [] ->
      (* If no files were specified, parse stdin. *)
      parse_channel
        ~name:"<stdin>"
        ~print:true
        parser_state handle_stmt stdin [""]

  | (name :: _) as args ->
      (* Otherwise, parse the files. *)
      File.with_file_in name begin fun file ->
        parse_channel
          ~name
          ~print:false
          parser_state handle_stmt file args
      end
  end;

  (* Exit without error. *)
  0
;;

exit (main ())
