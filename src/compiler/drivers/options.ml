type phase_t =
  | Parse

let include_dirs = ref []
let files = ref []
let imports = ref []
let phase = ref Parse
let optimize = ref 0

let parse_args usage =
  let options = [
    ("-I", Arg.String (fun i -> include_dirs := i :: !include_dirs),
      "Add <dir> to the list of include directories");
    ("--import", Arg.String (fun i -> imports := i :: !imports),
      "Add <dir> to the list of include directories");
      ("--phase", Arg.Symbol
        (["parse"; "macro"; "desugar"; "bind"; "optimize"; "lower"; "compile";
          "run"],
        (function
          | "parse" -> phase := Parse
          | _ -> ()
        )), "Select phase of compilation to run";);
    ("-O", Arg.Set_int optimize, "Select optimization level")
  ] in
  let anonymous file = files := file :: !files in
  Arg.parse options anonymous usage
