type phase_t =
  | Parse_scheme
  | Parse_sexp
  | Parse_ast
  | Typecheck
  | Generate_code
  | Execute_code

let include_dirs = ref []
let args = ref []
let imports = ref []
let phase = ref Generate_code
let optimize = ref 0

let parse_args usage =
  let options = [
    ("-I", Arg.String (fun i -> include_dirs := i :: !include_dirs),
      "Add <dir> to the list of include directories");
    ("--import", Arg.String (fun i -> imports := i :: !imports),
      "Add <dir> to the list of include directories");
      ("--phase", Arg.Symbol (
        [ "parse_scheme";
          "parse_sexp";
          "parse";
          "typecheck";
          "codegen";
          "execute" ],
        (function
          | "parse_scheme" -> phase := Parse_scheme
          | "parse_sexp" -> phase := Parse_sexp
          | "parse" -> phase := Parse_ast
          | "typecheck" -> phase := Typecheck
          | "codegen" -> phase := Generate_code
          | "execute" -> phase := Execute_code
          | phase -> failwith ("unknown phase: " ^ phase)
        )), "Select phase of compilation to run";);
    ("-O", Arg.Set_int optimize, "Select optimization level")
  ] in
  let anonymous arg = args := arg :: !args in
  Arg.parse options anonymous usage
