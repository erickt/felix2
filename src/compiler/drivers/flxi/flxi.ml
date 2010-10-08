(** Interactive REPL driver code. *)

open Batteries
open Format

(* Preparse all the imported libraries. *)
let parse_imports parser_state imports handle_stmt =
  List.fold_left begin fun parser_state name ->
    printf "parse_imports: %s@." name;
    let handle_stmt = handle_stmt ~name ~print:false in
    Flx_parse.parse_file
      ~include_dirs:!Options.include_dirs
      parser_state
      (fun _ sr stmt -> handle_stmt sr stmt)
      name
  end
  parser_state
  (List.rev imports)


(* Parse the channel *)
let parse_channel ~name ~print parser_state handle_stmt channel args =
  (* Create a buffer we'll save all our input to. *)
  let buffer = Buffer.create 512 in

  (* Mark when we've finished processing a line. *)
  let first_line = ref true in

  let lexbuf =
    Flx_parse.lexbuf_from_function ~name begin fun s n ->
      (* Optionally print the prompt. *)
      if print then printf "%s @?" (if !first_line then ">>>" else "...");

      let n = input channel s 0 n in

      (* Cache the line in our buffer so that we can do proper error
       * messages. *)
      Buffer.add_substring buffer s 0 n;

      first_line := false;

      n
    end
  in

  (* Cache the last valid statement parser state in case we run into a syntax
   * error later. If we do, we'll use this parser state to recover the latest
   * syntax. *)
  let old_parser_state = ref parser_state in

  let handle_stmt = handle_stmt ~name in

  (* Wrap handling a statement so we can tell when we've finished processing a
   * statement. *)
  let handle_stmt parser_state sr stmt =
    (* Cache the current parser state. *)
    old_parser_state := parser_state;

    (* We hit a statement, so we can print the standard prompt next time. *)
    first_line := true;

    (* Now, actually call the function. *)
    handle_stmt ~print:true sr stmt
  in

  (* Wrapper to simplify error reporting. *)
  let print_error format =
    Flx_format.ksprintf begin fun s ->
      (* Reset our state. *)
      first_line := true;

      eprintf "%s@." s;

      (* Ignore the rest of the line. *)
      Flx_parse.flush_input lexbuf
    end format
  in

  (* Prints the error in the source. *)
  let print_error_srs ppf srs =
    let file = IO.input_string (Buffer.contents buffer) in

    match srs with
    | [] -> ()
    | [sr] -> Flx_srcref.print_error file ppf sr
    | [sr1; sr2] ->
        let sr = Flx_srcref.join sr1 sr2 in
        Flx_srcref.print_error file ppf sr
    | _ -> ()
  in

  (* Loop over each statement until we exit. *)
  let rec aux parser_state =
    match
      begin try
        let parser_state =
          Flx_profile.call "Flx_parse.toplevel" begin fun () ->
            Flx_parse.toplevel parser_state handle_stmt lexbuf
          end
        in

        Some parser_state
      with
      (*
      | Failure s ->
          if Printexc.backtrace_status () then begin
            eprintf "%s@." (Printexc.get_backtrace ());
          end;

          printf "Fatal error: %s@." s;
          None
          *)

      | Flx_exceptions.Syntax_error (sr, e) ->
          if Printexc.backtrace_status () then begin
            eprintf "%s@." (Printexc.get_backtrace ());
          end;

          print_error "%a%s@." print_error_srs [sr] e;

          Some !old_parser_state

      | Flx_sexp.Sexp_error (sexp, e) ->
          if Printexc.backtrace_status () then begin
            eprintf "%s@." (Printexc.get_backtrace ());
          end;

          print_error "Sexp error: %s:@,%a@." e Flx_sexp.print sexp;

          Some !old_parser_state

      | Flx_type.Type_error (sr, e) ->
          if Printexc.backtrace_status () then begin
            eprintf "%s@." (Printexc.get_backtrace ());
          end;

          print_error "%a@.Type error: %s@." print_error_srs [sr] e;

          Some !old_parser_state

      | Flx_bind.Error (sr, e) ->
          if Printexc.backtrace_status () then begin
            eprintf "%s@." (Printexc.get_backtrace ());
          end;

          print_error "%a@.Type error: %a@."
            print_error_srs [sr]
            Flx_bind.print_error e;

          Some !old_parser_state

      | IO.No_more_input ->
          None
      end
    with
    | None -> ()
    | Some parser_state -> aux parser_state
  in

  aux parser_state


(* Parse a scheme block and print it out. *)
let print_scheme ~name ~print _ ocs =
  if print then printf "PARSED: %s@." (Ocs_print.string_of_ocs ocs);
  ()

(* Parse a sexp block and print it out. *)
let print_sexp ~name ~print _ ocs =
  if print then printf "PARSED: %a@." Flx_sexp.print (Flx_sexp.of_ocs ocs);
  ()

(* Parse a s-expression and print it out. *)
let print_ast ~name ~print _ ocs =
  Flx_profile.call "Flxi.print_ast" begin fun () ->
    let open Flx_ast in
    let sexp = Flx_sexp.of_ocs ocs in
    if print then printf "PARSED: %a@." Stmt.print (Flx_sexp.to_stmt sexp);
    ()
  end

(* Parse a type tree and print it out. *)
let print_typecheck ~name =
  let env = ref Flx_env.empty in
  let tve = ref Flx_tve.empty in

  fun ~print _ ocs ->
  Flx_profile.call "Flxi.print_ast" begin fun () ->
    let open Flx_type in
    let sexp = Flx_sexp.of_ocs ocs in
    let stmt = Flx_sexp.to_stmt sexp in
    let env', tve', stmt = Flx_bind.bind_stmt !env !tve stmt in
    env := env';
    tve := tve';

    if print then printf "SEMA: %a@.ENV: %a@.TVE: %a@."
      Stmt.print stmt
      (Flx_env.print Type.print) !env
      Flx_tve.print !tve;

    ()
  end

(* *)
let print_codegen ~name =
  printf "print_codegen: %s@." name;

  let env = ref Flx_env.empty in
  let tve = ref Flx_tve.empty in
  let codegen_state = ref (Flx_codegen.make_state name 3) in

  fun ~print _ ocs ->
  Flx_profile.call "Flxi.run_code" begin fun () ->
    let open Flx_type in
    let sexp = Flx_sexp.of_ocs ocs in
    let stmt = Flx_sexp.to_stmt sexp in
    let env', tve', stmt = Flx_bind.bind_stmt !env !tve stmt in
    env := env';
    tve := tve';

    let codegen_state' =
      (* Don't generate no-ops. *)
      match Stmt.node stmt with
      | Stmt.Noop _ -> !codegen_state
      | _ ->
        let codegen_state', _ =
          Flx_codegen.codegen_toplevel_stmt
            !codegen_state
            stmt
        in

        Llvm.dump_module codegen_state'.Flx_codegen.the_module;

        codegen_state'
    in

    codegen_state := codegen_state';

    ()
  end

(* *)
let print_execute ~name ~print _ ocs =
  assert false


let main () =
  Options.parse_args "Usage: flxi <options> <files>\nOptions are:";

  (* Create the state needed for parsing. *)
  let parser_state = Flx_parse.make_parser_state () in

  let handle_stmt =
    match !Options.phase with
    | Options.Parse_scheme -> print_scheme
    | Options.Parse_sexp -> print_sexp
    | Options.Parse_ast -> print_ast
    | Options.Typecheck -> print_typecheck
    | Options.Generate_code -> print_codegen
    | Options.Execute_code -> print_execute
  in

  (* Parse all the imported files. *)
  let parser_state =
    Flx_profile.call "Flxi.parse_imports" begin fun () ->
      parse_imports parser_state !Options.imports handle_stmt
    end
  in

  begin match !Options.args with
  | [] ->
      (* If no files were specified, parse stdin. *)
      Flx_profile.call "Flxi.parse_channel" begin fun () ->
        parse_channel
          ~name:"<stdin>"
          ~print:true
          parser_state handle_stmt stdin [""]
      end

  | (name :: _) as args ->
      (* Otherwise, parse the files. *)
      File.with_file_in name begin fun file ->
        Flx_profile.call "Flxi.parse_channel" begin fun () ->
          parse_channel
            ~name
            ~print:false
            parser_state handle_stmt file args
        end
      end
  end;

  Flx_profile.print Format.err_formatter;

  (* Exit without error. *)
  0
;;

exit (main ())
