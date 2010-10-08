open Batteries
open Flx_type
open Flx_format


module StringMap = Map.StringMap


type state_t =
  { context: Llvm.llcontext;
    the_module: Llvm.llmodule;
    the_fpm: [`Function] Llvm.PassManager.t;
    the_ee: Llvm_executionengine.ExecutionEngine.t;
    values: Llvm.llvalue StringMap.t;
    global_values: Llvm.llvalue StringMap.t; 
    counter: int }


(** Convenience function to add value to the state. *)
let add_value ~global state name value =
  if global
  then { state with values=StringMap.add name value state.values }
  else { state with global_values=StringMap.add name value state.global_values }


(** Convenience function to find a value in the state. *)
let find_value { values; global_values } name =
  try StringMap.find name values
  with Not_found ->
    (* Fall back on searching global values. *)
    try StringMap.find name global_values
    with Not_found ->
      (* We got bigger problems, so error out. *)
      Flx_exceptions.internal_error
        []
        "cannot find a value named '%s'" name


(** Generate a unique name for an anonymous value. *)
let unique_name state =
  let name = Printf.sprintf "_%d" state.counter in
  { state with counter=state.counter + 1 }, name


let make_state module_name optimization_level =
  let context = Llvm.create_context () in
  let the_module = Llvm.create_module context module_name in

  (* Create the optimizer and execution engine. *)
  let the_fpm = Llvm.PassManager.create_function the_module in
  let the_ee = Llvm_executionengine.ExecutionEngine.create the_module in

  (* Set up the optimizer pipeline. *)
  Llvm_target.TargetData.add
    (Llvm_executionengine.ExecutionEngine.target_data the_ee)
    the_fpm;

  if optimization_level >= 1 then begin
    (* Promote allocas to registers. *)
    Llvm_scalar_opts.add_memory_to_register_promotion the_fpm;

    (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
    Llvm_scalar_opts.add_instruction_combination the_fpm;

    (* reassociate expressions. *)
    Llvm_scalar_opts.add_reassociation the_fpm;

    (* Eliminate Common SubExpressions. *)
    Llvm_scalar_opts.add_gvn the_fpm;

    (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
    Llvm_scalar_opts.add_cfg_simplification the_fpm;
  end;

  ignore (Llvm.PassManager.initialize the_fpm);

  { context = context;
    the_module = the_module;
    the_fpm = the_fpm;
    the_ee = the_ee;
    values = StringMap.empty;
    global_values = StringMap.empty;
    counter = 0 }


(* Convenience function to find the parent of the builder. *)
let builder_parent builder =
  (* First we need to get the current basic block. *)
  let bb = Llvm.insertion_block builder in

  (* Then, return the basic block's parent. *)
  Llvm.block_parent bb


(* Convenience wrapper to creating a gep. *)
let codegen_gep state value args name builder =
  let args = Array.map
    (fun i -> Llvm.const_int (Llvm.i32_type state.context) i)
    args
  in
  Llvm.build_gep value args name builder


(** Convert an integer kind into an llvm type. *)
let lltype_of_int_kind state = function
  | Type.Int
  | Type.Uint -> Llvm.i32_type state.context


(** Convert a type into an llvm type. *)
let rec lltype_of_type state typ =
  let open Type in
  match typ.node with
  | Variable var -> assert false
  | Integer int_kind -> lltype_of_int_kind state int_kind
  | String -> assert false
  | Name name -> assert false
  | Tuple typs ->
      let typs = List.map (lltype_of_type state) typs in
      Llvm.struct_type state.context (Array.of_list typs)
  | Arrow (lhs, rhs) ->
      let lhs =
        match lhs with
        | { node=(Tuple typs) } ->
            Array.of_list (List.map (lltype_of_type state) typs)
        | _ -> [| lltype_of_type state lhs |]
      in
      Llvm.function_type (lltype_of_type state rhs) lhs


(* Flatten the params list down into all the parameters. *)
let flatten_parameters params =
  let parameters =
    List.fold_left begin fun parameters param ->
      List.fold_left
        (fun ps p -> p :: ps)
        parameters
        param.Param.parameters
    end
    []
    params
  in

  (* Reverse the parameters back in order. *)
  List.rev parameters


(** Generate code for a literal. *)
let codegen_literal state builder literal =
  let open Literal in
  match node literal with
  | Integer (int_kind, i) ->
      Llvm.const_int_of_string
        (lltype_of_int_kind state int_kind)
        (Big_int.string_of_big_int i)
        10

  | String s ->
      (* Create a global constant string. *)
      let c = Llvm.const_stringz state.context s in
      let g = Llvm.define_global "" c state.the_module in
      Llvm.set_linkage Llvm.Linkage.Internal g;

      (* Return a gep to the value in order to be the right type. *)
      let zero = Llvm.const_int (Llvm.i32_type state.context) 0 in
      Llvm.build_gep g [| zero; zero |] "" builder


(** Create an alloca instruction in the entry block of the function. This is
 * used for mutable variables and etc. *)
let create_entry_block_alloca state builder typ name =
  (* Get the builder's function. *)
  let the_function = builder_parent builder in

  (* Get a builder at the entry block. *)
  let builder = Llvm.builder_at
    state.context
    (Llvm.instr_begin (Llvm.entry_block the_function))
  in

  (* Return the alloca. *)
  Llvm.build_alloca (lltype_of_type state typ) name builder


(** Find the llvm type kind that corresponds with a felix type. *)
let lltypekind_of_type typ =
  let open Type in
  match typ.node with
  | Variable _ -> assert false
  | Integer _ -> Llvm.TypeKind.Integer
  | String -> Llvm.TypeKind.Pointer
  | Name _ -> assert false
  | Tuple _ -> Llvm.TypeKind.Pointer
  | Arrow (_,_) -> Llvm.TypeKind.Function


(** Generate code for an expression. *)
let rec codegen_expr ?(name="") state builder expr =
  let open Expr in
  match node expr with
  | Literal literal -> codegen_literal state builder literal
  | Tuple exprs -> codegen_struct state builder exprs (typ expr)
  | Name name ->

      (* Find the alloca for the name. *)
      let alloca = find_value state name in

      (* Load the value from the alloca. *)
      Llvm.build_load alloca name builder

  | Sum (lhs, rhs) ->
      let lhs = codegen_expr state builder lhs in
      let rhs = codegen_expr state builder rhs in

      Llvm.build_add lhs rhs name builder

  | Product (lhs, rhs) -> assert false
  | Lambda lambda -> assert false


(** Generate code for a load. *)
and codegen_load ?name state builder expr =
  let open Expr in

  (* First construct the expression. *)
  let e = codegen_expr ?name state builder expr in

  (* Literals aren't dereferenced. *)
  match node expr with
  | Literal _ -> e
  | _ ->
      (* Dereference only if we received a pointer. *)
      begin match Llvm.classify_type (Llvm.type_of e) with
      | Llvm.TypeKind.Pointer -> Llvm.build_load e "" builder
      | _ -> e
      end


(** Generate code for a store. *)
and codegen_store state builder expr pointer =
  let open Expr in

  match node expr with
  | Tuple exprs ->
      (* If the expression is a tuple, just load it directly. *)
      store_struct state builder exprs pointer

  | _ ->
      (* Otherwise, dereference the value and store it in our alloca. *)
      let expr = codegen_load state builder expr in
      Llvm.build_store expr pointer builder


(** Generate code for a structure. *)
and codegen_struct state builder exprs typ =
  let pointer = create_entry_block_alloca state builder typ "" in
  store_struct state builder exprs pointer


(** Store values into a struct. *)
and store_struct state builder exprs pointer =
  (* Add the values to the struct. *)
  Flx_list.iteri begin fun i expr ->
    let lhs = codegen_gep state pointer [| 0; i |] "" builder in
    let rhs = codegen_expr state builder expr in

    ignore (Llvm.build_store rhs lhs builder)
  end exprs;

  (* Return the struct. *)
  pointer 


(** Generate code for a lambda prototype. *)
let codegen_prototype ~global state lambda name =
  let open Lambda in

  let parameters = Array.of_list (flatten_parameters lambda.params) in

  (* Construct the lambda's type. *)
  let typ = Llvm.function_type
    (lltype_of_type state lambda.return_typ)
    (Array.map (fun p -> lltype_of_type state p.Parameter.typ) parameters)
  in

  (* Declare the function, or error out if it already has been declared. *)
  let the_function =
    match Llvm.lookup_function name state.the_module with
    | Some f -> assert false
    | None -> Llvm.declare_function name typ state.the_module
  in

  (* Set the names for the parameters. *)
  Array.iteri begin fun i parameter ->
    Llvm.set_value_name parameter.Parameter.name (Llvm.param the_function i)
  end parameters;

  (* Register the function value. *)
  let state = add_value ~global state name the_function in

  state, the_function


(** Generate code for a lambda. *)
let rec codegen_lambda ~global ?name state lambda =
  let state, name =
    match name with
    | Some name -> state, name
    | None -> unique_name state
  in

  (* Declare the function. *)
  let state, the_function = codegen_prototype ?global state lambda name in

  (* Create the entry basic block. *)
  let bb = Llvm.append_block state.context "entry" the_function in

  (* And a builder for the function. *)
  let builder = Llvm.builder_at_end state.context bb in

  (* Create allocas for each of the arguments. *)
  let state =
    Llvm.fold_left_params begin fun state param ->
      let name = Llvm.value_name param in
      let alloca = Llvm.build_alloca (Llvm.type_of param) name builder in

      (* Store the argument in the alloca. *)
      ignore (Llvm.build_store param alloca builder);

      (* And finally, bind the name to the alloca. *)
      add_value ~global state name alloca
    end
    state
    the_function
  in
  
  (* We're all ready, so generate the code for the lambda's statements. *)
  let state' =
    List.fold_left
      (fun state stmt -> codegen_stmt state builder stmt)
      state
      lambda.Lambda.stmts
  in

  (* We throw away most of the lambda's state so that we don't have any names
   * leaking out of their scope. We only want to save any global values. *)
  let state = { state with global_values=state'.global_values } in

  state, the_function


(** Generate code for a statement. *)
and codegen_stmt ?(global=false) state builder stmt =
  let open Stmt in

  match node stmt with
  | Noop msg -> state

  | Value (name, expr) ->
      (* Allocate some memory for our value. *)
      let alloca = create_entry_block_alloca
        state
        builder
        (Expr.typ expr)
        name
      in

      (* Store the expression in the memory. *)
      ignore (codegen_store state builder expr alloca);

      (* Finally, update our value map with our name. *)
      add_value ~global state name alloca

  | GlobalVariable (name, expr) ->
      (* Create a global variable for our expression. *)
      let global = Llvm.define_global
        name
        (codegen_load state builder expr)
        state.the_module
      in

      (* Finally, update our value map with our name. *)
      add_value ~global:true state name global

  | Curry (name, lambda) ->
      let state, _ = codegen_lambda ~global ~name state lambda in
      state

  | Return expr ->
      let expr = codegen_load state builder expr in
      ignore (Llvm.build_ret expr builder);
      state


(** Generate code for a toplevel statement, which actually stores the statement
 * in a function, and puts any results in a global variable. *)
let codegen_toplevel_stmt state stmt =
  let open Stmt in
  let open Llvm_executionengine in

  (* Convert values into global variables. *)
  let stmt =
    match node stmt with
    | Value (name, expr) -> global_variable ~sr:(sr stmt) name expr
    | _ -> stmt
  in

  (* Make a function to hold our stmt. *)
  let return_stmt = Stmt.return (Expr.unit ()) in
  let lambda = Lambda.make
    Lambda.Function
    []
    (Stmt.typ return_stmt)
    [stmt; return_stmt]
  in

  (* Make our function *)
  let state, the_function = codegen_lambda ~global:false state lambda in

  (* Make sure the function is valid. *)
  Llvm_analysis.assert_valid_function the_function;

  (* Optimize the function. *)
  ignore (Llvm.PassManager.run_function the_function state.the_fpm);

  state, the_function


(** Code generate and execute a statement. *)
let codegen_and_run state stmt =
  let open Llvm_executionengine in
  let open Type in

  let state, the_function = codegen_toplevel_stmt state stmt in

  (* Execute the function. *)
  let value = ExecutionEngine.run_function the_function [| |] state.the_ee in

  (* Try to convert the value back into felix terms. *)
  let value =
    match (Stmt.typ stmt).node with
    | Variable _ -> assert false
    | Integer int_kind ->
        let literal = Literal.int32 (GenericValue.as_int32 value) in
        Some (Expr.literal literal)
    | String -> assert false
    | Name _ -> assert false
    | Tuple _ -> assert false
    | Arrow _ -> assert false
  in

  state, value
