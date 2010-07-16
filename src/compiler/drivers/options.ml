type phase_t =
  | Parse_scheme
  | Parse_sexp
  | Parse_ast

let include_dirs = ref []
let files = ref []
let imports = ref []
let phase = ref Parse_ast
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
          "parse" ],
        (function
          | "parse_scheme" -> phase := Parse_scheme
          | "parse_sexp" -> phase := Parse_sexp
          | "parse" -> phase := Parse_ast
          | phase -> failwith ("unknown phase: " ^ phase)
        )), "Select phase of compilation to run";);
    ("-O", Arg.Set_int optimize, "Select optimization level")
  ] in
  let anonymous file = files := file :: !files in
  Arg.parse options anonymous usage
