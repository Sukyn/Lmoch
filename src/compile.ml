(* Importing necessary modules and types from Goblint-Cil and other libraries *)
open GoblintCil.Cil
open Typed_ast
open Asttypes
open Ident

module StringMap = Map.Make(String)

(* Global reference to keep track of the component number *)
let component_number = ref 0

(* Function to generate a unique name for a component *)
let get_next_component_name () =
  let name = "comp_" ^ (string_of_int !component_number) in
  incr component_number;
  name

(* Function to translate a Typed_Ast type to a CIL type *)
let translate_type_to_cil = function
  | Tbool -> TInt (IInt, [])
  | Tint -> TInt (IInt, [])
  | Treal -> TFloat (FFloat, [])

(* Function to translate a Typed_ast constant to a CIL constant *)
let translate_const_to_cil = function
  | Cbool b ->
    let cilint = mkCilint IInt (if b then 1L else 0L) in
    CInt (cilint, IInt, None)
  | Cint i ->
    let cilint = mkCilint IInt (Int64.of_int i) in
    CInt (cilint, IInt, None)
  | Creal f ->
    CReal (f, FFloat, None)

(* Function to create a CIL structure based on a list of variables *)
let make_struct variable_list =
  (* Create a new component with a unique name *)
  let component = mkCompInfo true (get_next_component_name ()) (fun _ -> []) [] in
  (* Translate each variable into a CIL field and add it to the component's fields *)
  let fields = List.map (fun ({ name; _ }, typ) ->
      { fcomp = component; fname = name; ftype = translate_type_to_cil typ; fbitfield = None; fattr = []; floc = locUnknown })
    variable_list
  in
  component.cfields <- fields;
  component

(* Function to create a CIL function declaration based on a variable information *)
let make_function_decl var_info = {
  svar = var_info;
  sformals = [];
  slocals = [];
  smaxid = 0;
  sbody = mkBlock [];
  smaxstmtid = None;
  sallstmts = [];
}

(* Returns a local varinfo, and struct list *)
let compile_output_type output =
  if List.length output > 1 then
    let structure = make_struct output in
    TComp (structure, [])
  else
    let variable = List.hd output in
    snd variable |> translate_type_to_cil

(* Function to create a local variable and struct list based on output type *)
let compile_output_step function_decl type_ output =
  if List.length output > 1 then
    makeLocalVar function_decl "res" type_
  else
    let variable = List.hd output in
    let name = let identifier = fst variable in identifier.name in
    makeLocalVar function_decl name type_

let mk_mem_struct mem_structs fby_mem node_mem =
  ()

(* Function to compile a single node in the AST to a CIL function *)
let compile_node globals node =
  (* Extracting input variables and their types from the node *)
  let function_args = List.map (fun ({ name; _ }, typ) ->
      name, translate_type_to_cil typ, []) node.tn_input in
  (* Creating the output type for the function *)
  let output_type = compile_output_type node.tn_output in
  (* Creating the function type *)
  let function_type = TFun (output_type, Some function_args, false, []) in
  (* Creating the global variable for the function *)
  let global_var = makeGlobalVar node.tn_name.name function_type in
  (* Creating the function declaration *)
  let function_decl = make_function_decl global_var in
  (* Creating the local variable for the function's output *)
  let output_step = compile_output_step function_decl output_type node.tn_output in
  (* Creating the function body *)
  let function_block = mkBlock [mkStmt (Return (Some (Lval (Var output_step, NoOffset)), locUnknown))] in
  let _locals =
    List.map
      (fun ({name; _}, typ) ->
         makeLocalVar function_decl name (translate_type_to_cil typ))
      node.tn_local
    |> List.fold_left (fun locals local ->
        StringMap.add local.vname local locals)
      StringMap.empty
  in
  (* Assigning the function body to the function declaration *)
  function_decl.sbody <- function_block;
  (* Creating the CIL function *)
  GFun (function_decl, locUnknown)

(* Function to compile the entire AST *)
let compile ast =
  (* Compiling each node in the AST to a CIL function *)
  let globals = List.map (compile_node []) ast in
  let open GoblintCil.Errormsg in
  (* Logging the generated CIL code for each function *)
  List.iter (log "%a" d_global) globals;
  (* Returning the original AST *)
  ast

(* Print *)
let pp fmt _ = Format.fprintf fmt "Pass"

let write_out c_ast out =
  Format.fprintf out "%a" pp c_ast