open GoblintCil.Cil
open Imp_ast
open Asttypes
open Ident

module M = Map.Make(String)

(*************)
(*** UTILS ***)
(*************)
    
(* Create an integer expression *)
let mk_int_aux i = CInt (mkCilint IInt (Int64.of_int i), IInt, None)
let mk_int_exp i = Const (mk_int_aux i)

(* Translate a constant to its CIL representation *)
let rec trans_const t = function
  | BooleanConstant b -> CInt (mkCilint IInt (if b then 1L else 0L), IInt, None)
  | IntegerConstant i -> mk_int_aux i
  | RealConstant f -> CReal (f, FFloat, None)
  | EnumeratedDataConstant (s, Some e) ->
      let enum = match mk_enum t s with
        | TEnum (e, _) -> e
        | _ -> assert false 
      in
      let _, exp, _ = List.find (fun (n, _, _) -> n = String.uppercase_ascii e) enum.eitems in
      CEnum (exp, enum.ename, enum)
  | EnumeratedDataConstant (s, None) ->
      let ts = List.find (fun Asttypes.{ name; _ } -> name = s) t in
      trans_const t (EnumeratedDataConstant (s, Some (List.hd ts.constructors)))
      
(* Create an enumeration type for the given name *)
and mk_enum =
  let h = Hashtbl.create 20 in
  fun types ename ->
    try Hashtbl.find h ename with Not_found ->
      let ts = List.find (fun { name; constructors } -> name = ename) types in
      let eitems = List.mapi (fun i c ->
        let ename = String.uppercase_ascii c in
        let exp = Const (trans_const types (IntegerConstant i)) in
        ename, exp, locUnknown) ts.constructors in
      let enuminfo = { ename; eitems; ekind = IInt; eattr = []; ereferenced = false; } in
      let enum = TEnum (enuminfo, []) in
      Hashtbl.add h ename enum;
      enum

(* Translate a type to its CIL representation *)
let rec translate_type types = function
  | BooleanType -> TInt (IInt, [])
  | IntegerType -> TInt (IInt, [])
  | RealType -> TFloat (FFloat, [])
  | EnumeratedDataType s -> mk_enum types s

(* Type definitions for basic types *)
let bool_t = TInt (IInt, [])
let int_t = TInt (IInt, [])
let real_t = TFloat (FFloat, [])

(* Clean a name by replacing single quotes with double underscores *)
let clean_name name = Str.global_replace (Str.regexp {|'|}) "__" name

(* Create a struct type with the given name and field list *)
let mk_struct types name var_l =
  let comp = mkCompInfo true name (fun _ -> []) [] in
  let trans_field (var, typ, _) = { fcomp = comp; fname = clean_name var.name; ftype = translate_type types typ; fbitfield = None; fattr = []; floc = locUnknown } in
  { comp with cfields = List.map trans_field var_l }
  
(* Find a field in a list of fields by name *)
let find_field_list name = List.find (fun f -> f.fname = clean_name name) 

(* Find a global component by name *)
let rec find_gcomp name = function
  | GCompTag (compinfo, _) :: tl when compinfo.cname = name -> compinfo
  | _ :: tl -> find_gcomp name tl
  | [] -> raise Not_found

(* Find a field in a global component by name *)
let find_field_globals comp field globals =
  find_field_list field (find_gcomp comp globals).cfields
  
(* Create a function declaration from a varinfo *)
let mk_fundec varinfo =
  List.fold_left (fun fundec (n, typeList, _) -> ignore(makeFormalVar fundec n typeList); fundec)
    { svar = varinfo;
      sformals = [];
      slocals = [];
      smaxid = 0;
      sbody = mkBlock [];
      smaxstmtid = None;
      sallstmts = []; }
    (match varinfo.vtype with
    | TFun (_, Some args, _, _) -> args
    | TFun (_, None, _, _) -> []
    | _ -> raise (Invalid_argument "Not a function type"))

(* Find a formal parameter in a function by name *)
let find_formal fundec name =
  List.find (fun { vname; _ } -> vname = clean_name name) fundec.sformals
  
(* Append a list of statements to a block *)
let append_stmts stmts block = {block with bstmts = block.bstmts @ stmts}

(* Find a function by name in a list of global declarations *)
let rec find_fun name = function
  | GFun ({ svar = { vname; _ }; _ } as f, _) :: tl when vname = clean_name name -> f
  | _ :: tl -> find_fun name tl
  | [] -> raise Not_found

(* Find a local variable by name in a function *)
let find_local fundec name =
  List.find (fun { vname; _ } -> vname = clean_name name) fundec.slocals
  
(* Global variables for standard C library functions *)
let printf_lval = Lval (Var (makeGlobalVar "printf" (TFun (TInt (IInt, []), Some ["format", charConstPtrType, []], true, []))), NoOffset)
let sleep_lval = Lval (Var (makeGlobalVar "usleep" (TFun (TInt (IInt, []), Some ["microseconds", TInt (IInt, []), []], true, []))), NoOffset)
let fflush_lval = Lval (Var (makeGlobalVar "fflush" (TFun (TInt (IInt, []), Some ["status", TInt (IInt, []), []], true, []))), NoOffset)
let atoi_lval = Lval (Var (makeGlobalVar "atoi" (TFun (TInt (IInt, []), Some ["str", charConstPtrType, []], true, []))), NoOffset)
let exit_lval = Lval (Var (makeGlobalVar "exit" (TFun (TVoid [], Some ["status", TInt (IInt, []), []], true, []))), NoOffset)

(* Global variables for custom read functions *)
let int_read_info = makeGlobalVar "int_read" (TFun (TInt (IInt, []), None, true, []))
let int_read_lval = Lval (Var int_read_info, NoOffset)

(* Helper function to generate a unique name *)
let gen_unique_name prefix counter =
  incr counter;
  prefix ^ "_" ^ (string_of_int !counter)

(* Helper functions to generate unique names for various types *)
let gen_eq_ty_name = let eq_ty_cnt = ref 0  in fun () -> gen_unique_name "eq_ty" eq_ty_cnt
let gen_tuple_ty_name = let tuple_ty_cnt = ref 0  in fun () -> gen_unique_name "tuple_ty" tuple_ty_cnt
let gen_tuple_field_name = let tuple_field_cnt = ref 0 in fun () -> gen_unique_name "tuple_field" tuple_field_cnt
let gen_call_name = let call_cnt = ref 0  in fun () -> gen_unique_name "call" call_cnt
let gen_switch_name = let switch_cnt = ref 0 in fun () -> gen_unique_name "switch" switch_cnt

(* Helper function to append a statement to the function body *)
let append_stmt_to_body fundec stmt = fundec.sbody <- append_stmts [stmt] fundec.sbody

(* Helper function to find field in globals and add its address to arguments *)
let add_field_address_to_args file node fundec mem_field compiledArgs =
  try
    AddrOf (Mem (Lval (Var (find_formal fundec "mem"), NoOffset)),
            Field (find_field_globals (node.INode.name.name ^ "_mem") mem_field.name file.globals, NoOffset)) :: compiledArgs
  with Not_found ->
    compiledArgs

(* Function to set an instruction and append it to the function body *)
let set_instr_and_append_to_body fundec lval expr =
  append_stmt_to_body fundec (mkStmtOneInstr (Set (lval, expr, locUnknown, locUnknown)))

  
(* Helper function to get the lval *)
let get_lval fundec node name file =
  try
    Var (find_local fundec name.name), NoOffset
  with Not_found ->
    Var (find_local fundec "ret_"), Field (find_field_globals (node.INode.name.name ^ "_ret") name.name file.globals, NoOffset)
  
(*************************)  
(*** COMPILE FUNCTIONS ***)
(*************************)  
let compile_mem_comp types globals node =
  let m_comp = (mk_struct types (node.INode.name.name^"_mem") node.INode.mem.fby) in 
  let n_mem = node.INode.mem.node_mem in
  { m_comp with cfields = List.filter_map 
    (fun ({name = f}, {name = n}) -> 
    Some {
      fcomp = m_comp;
      fname = clean_name f;
      ftype = TComp (find_gcomp (n ^ "_mem") globals, []);
      fbitfield = None;
      fattr = [];
      floc = locUnknown;
    }) n_mem @ m_comp.cfields }

let compile_return_type types file node =
  match node.INode.output_step with
  | [] -> file, TVoid ([])
  | [_, typeList, _] -> file, translate_type types typeList
  | l -> 
      let ret_name = node.INode.name.name ^ "_ret" in
      file.globals <- GCompTag (mk_struct types ret_name l, locUnknown) :: file.globals;
      (file, TComp (mk_struct types ret_name l, []))

(* Compiles initialization *)
let compile_init types file node mem_comp =
  let fundec = mk_fundec (makeGlobalVar (node.INode.name.name ^ "_init") (TFun (TVoid [], Some ["mem", TPtr (TComp (mem_comp, []), []), []], false, []))) in
  let append_init_stmts = List.fold_right append_stmts in
  let mem_lval = Lval (Var (find_formal fundec "mem"), NoOffset) in

  fundec.sbody <- append_init_stmts 
    [node.INode.init.Init.node |> List.filter_map (fun (f_id, init_id) ->
    try Some (mkStmtOneInstr (Call (None, Lval (Var (find_fun (init_id.name ^ "_init") file.globals).svar, NoOffset), [AddrOf (Mem mem_lval, Field (find_field_list (f_id.name) mem_comp.cfields, NoOffset))], locUnknown, locUnknown)))
    with Not_found -> None)] 
    (append_init_stmts
      [node.INode.init.Init.fby |> List.map (fun (id, atom) ->
      mkStmtOneInstr (match atom with
      | Atom.Ident _ -> failwith "Not implemented"
      | Const c -> Set ((Mem mem_lval, Field (find_field_list (id.name) mem_comp.cfields, NoOffset)), Const (trans_const types c), locUnknown, locUnknown))) ] fundec.sbody);
  file.globals <- (GFun (fundec, locUnknown)) :: file.globals

(* Recursively compiles expressions *)
let rec compile_expr types file node fundec expr =
  match expr.IExpr.idesc with
  | IE_const c -> let cst, typ = compile_const types c in 
                    file, cst, typ
  | IE_ident id -> compile_ident file fundec node id
  | IE_mem id -> compile_mem file node fundec id
  | IE_op (operator, el) -> compile_op types file node fundec (operator, el)
  | IE_tuple el -> compile_tuple types file node fundec el
  | IE_app (n_name, mem_field, args) -> compile_app types file node fundec (n_name, mem_field, args)
  | IE_reset (n_name, mem_field, args, control) -> compile_reset types file node fundec (n_name, mem_field, args, control)
  | IE_case (e, cases) -> compile_case types file node fundec (e, cases)
  | IE_prim (n, el) -> failwith "Not used"
  
and compile_const types c = 
  let determine_const_type types = function
  | Asttypes.BooleanConstant _ -> bool_t
  | IntegerConstant _ -> int_t
  | RealConstant _ -> real_t
  | EnumeratedDataConstant (s, _) -> translate_type types (EnumeratedDataType s)
  in
  GoblintCil.Const (trans_const types c), determine_const_type types c
  
(* Compiles identifiers *)
and compile_ident file fundec node id =
  let find_variable fundec name =
    try Some (find_formal fundec name)
    with Not_found ->
      try Some (find_local fundec name)
      with Not_found -> None
  in
  match find_variable fundec id.name with
  | Some var -> file, Lval (Var var, NoOffset), var.vtype
  | None ->
    let field = find_field_globals (node.INode.name.name^"_ret") id.name file.globals in
    file, Lval (Var (find_local fundec "ret_"), Field (field, NoOffset)), field.ftype

(* Compiles memory *)
and compile_mem file node fundec id =
  let find_field_and_var fundec name id_name globals =
    try Some (find_field_globals (name ^ "_mem") id_name globals, find_formal fundec "mem")
    with Not_found -> None
  in
  match find_field_and_var fundec node.INode.name.name id.name file.globals with
  | Some (field_info, var) ->
    file, Lval (Mem (Lval (Var var, NoOffset)), Field (field_info, NoOffset)), field_info.ftype
  | None ->
    let field = find_field_globals (node.INode.name.name ^ "_ret") id.name file.globals in
    file, Lval (Var (find_local fundec "ret_"), Field (field, NoOffset)), field.ftype
    
and compile_op types file node fundec = function 
  | op, [e1; e2] ->
    let file, e1', _ = compile_expr types file node fundec e1 in
    let file, e2', _ = compile_expr types file node fundec e2 in
    let typeList = translate_type types @@ List.hd e1.IExpr.itype in
    let binOp, resultType =
      let conv_op = (function
      | Op_eq -> Eq, bool_t
      | Op_neq -> Ne, bool_t
      | Op_lt -> Lt, bool_t
      | Op_le -> Le, bool_t
      | Op_gt -> Gt, bool_t
      | Op_ge -> Ge, bool_t
      | Op_add -> PlusA, int_t
      | Op_sub -> MinusA, int_t 
      | Op_mul -> Mult, int_t
      | Op_div -> Div, int_t
      | Op_mod -> Mod, int_t
      | Op_and -> LAnd, bool_t
      | Op_or -> LOr, bool_t
      | _ -> assert false
      ) in
      let op, ty = conv_op op in
      BinOp (op, e1', e2', typeList), ty
    in
    file, binOp, resultType
    | op, [e1] ->
      let file, e1', _ = compile_expr types file node fundec e1 in
      let typeList = translate_type types @@ List.hd e1.IExpr.itype in
      let unOp, resultType =
        match op with
        | Op_not -> UnOp (LNot, e1', typeList), bool_t
        | Op_sub -> UnOp (Neg, e1', typeList), int_t
        | _ -> assert false
      in
      file, unOp, resultType
    
  | _, [e1; e2; e3] ->
    let file, e1', _ = compile_expr types file node fundec e1 in
    let file, e2', _ = compile_expr types file node fundec e2 in
    let file, e3', _ = compile_expr types file node fundec e3 in
    let typeList = translate_type types @@ List.hd e2.IExpr.itype in
    file, Question (e1', e2', e3', typeList), typeList
  | _ -> assert false

(* Function to compile a tuple *)
and compile_tuple types file node fundec exprList =
  let tuple_comp = mk_struct types (gen_tuple_ty_name ()) (List.map (fun x -> x.IExpr.itype) exprList |> List.flatten |> List.map (fun typeList ->  Ident.make (gen_tuple_field_name ()) Stream, typeList, ())) in
  file.globals <- (GCompTag (tuple_comp, locUnknown))::file.globals;
  let tuple_var = makeLocalVar fundec (tuple_comp.cname^"__") (TComp (tuple_comp, [])) in
  let file, compiledExprs = compile_exprs types file node fundec exprList in
  let set_instructions_for_tuple_fields fundec tuple_var tuple_comp compiledExprs =
    List.iter2
      (fun fieldinfo expr ->
        append_stmt_to_body fundec (mkStmtOneInstr (Set ((Var tuple_var, Field (fieldinfo, NoOffset)), expr, locUnknown, locUnknown))))
      tuple_comp.cfields
      compiledExprs
  in
  set_instructions_for_tuple_fields fundec tuple_var tuple_comp compiledExprs;
  file, Lval (Var tuple_var, NoOffset), TComp (tuple_comp, [])
  
(* Helper function to compile a list of expressions *)
and compile_exprs types file node fundec =
  List.fold_left_map (fun file expr ->
      let file, compiledExpr, _ = compile_expr types file node fundec expr in
      file, compiledExpr)
    file

(* Function to compile an application *)
and compile_app types file node fundec (node_name, mem_field, args) =
  let file, compiledArgs = compile_exprs types file node fundec args in
  let compiledArgs = add_field_address_to_args file node fundec mem_field compiledArgs in
  let callee = find_fun (node_name.name) file.globals in
  let return_type =
    match callee.svar.vtype with
    | TFun (return_type, _, _, _) -> return_type
    | _ -> assert false
  in
  let result_var = makeLocalVar fundec (gen_call_name ()) return_type in
  fundec.sbody <- append_stmts [(mkStmtOneInstr (Call (Some (Var result_var, NoOffset), Lval (Var callee.svar, NoOffset), compiledArgs, locUnknown, locUnknown)))] fundec.sbody;
  file, Lval (Var result_var, NoOffset), return_type

(* Function to compile a reset *)
and compile_reset types file node fundec (node_name, mem_field, args, control) =
  let file, compiledArgs = compile_exprs types file node fundec args in
  let compiledArgs = add_field_address_to_args file node fundec mem_field compiledArgs in
  let callee = find_fun (node_name.name) file.globals in
  let result_var = makeLocalVar fundec (gen_call_name ()) (match callee.svar.vtype with
    | TFun (return_type, _, _, _) -> return_type
    | _ -> assert false) in
  let call_stmt = mkStmtOneInstr (Call (Some (Var result_var, NoOffset), Lval (Var callee.svar, NoOffset), compiledArgs, locUnknown, locUnknown)) in
  let file, stmts = try
        let file, expr, _ = compile_expr types file node fundec control in
        file, [mkStmt (If (expr, mkBlock [mkStmtOneInstr (Call (None, Lval (Var (find_fun (node_name.name^"_init") file.globals).svar, NoOffset), [AddrOf (Mem (Lval (Var (find_formal fundec "mem"), NoOffset)), Field (find_field_globals (node.INode.name.name^"_mem") (mem_field.name) file.globals, NoOffset))], locUnknown, locUnknown))], mkBlock [], locUnknown, locUnknown)); call_stmt]
      with Not_found ->
        file, [call_stmt]
    in
  List.iter (fun stmt -> fundec.sbody <- append_stmts [stmt] fundec.sbody) stmts;

  file, Lval (Var result_var, NoOffset), (match callee.svar.vtype with
    | TFun (return_type, _, _, _) -> return_type
    | _ -> assert false)

and find_var fundec id file node =
  try Lval (Var (find_formal fundec (id.name)), NoOffset)
  with Not_found ->
    try Lval (Var (find_local fundec (id.name)), NoOffset)
    with Not_found ->
      Lval (Var (find_local fundec "ret_"), Field (find_field_globals (node.INode.name.name^"_ret") id.name file.globals, NoOffset))

and compile_case types file node fundec (e, cases) =
  let compile_expr_and_ignore_ty expr = 
    let file, e', _ = compile_expr types file node fundec expr in
    file, e'
  in
  let res_ty =
    match (fst (List.hd cases)).IExpr.itype with
    | [typeList] -> translate_type types typeList
    | tl ->
      let compinfo = mk_struct types (gen_tuple_ty_name ()) (List.map (fun typeList -> Ident.make (gen_tuple_field_name ()) Stream, typeList, ()) tl) in
      file.globals <- (GCompTag (compinfo, locUnknown))::file.globals;
      TComp (compinfo, []) in
  let switch_res = makeLocalVar fundec (gen_switch_name ()) res_ty in
  let switch_lval = Var switch_res, NoOffset in
  let file, switch_stmts =
    List.fold_left (fun (file, stmts) (case, e) ->
        let file, case' = compile_expr_and_ignore_ty case in
        let file, e' = compile_expr_and_ignore_ty e in
        let set_instr = Set (switch_lval, e', locUnknown, locUnknown) in
        let stmt = mkStmtOneInstr set_instr in
        stmt.labels <- (Case (case', locUnknown, locUnknown))::stmt.labels;
        let brk_stmt = mkStmt (Break (locUnknown)) in
        file, brk_stmt::stmt::stmts)
      (file, [])
      cases
  in
  let switch_stmts = List.rev switch_stmts in
  let switch_block = mkBlock switch_stmts in
  let file, e =
    match e with
    | {IExpr.idesc = IE_ident id; _} -> file, find_var fundec id file node
    | _ -> compile_expr_and_ignore_ty e
  in
  let switch_stmt = mkStmt (Switch (e, switch_block, switch_stmts, locUnknown, locUnknown)) in
  fundec.sbody <- append_stmts [switch_stmt] fundec.sbody;
  file, Lval (Var switch_res, NoOffset), res_ty

(* Function to compile an equation *)
let rec compile_equation types file node fundec ({IEquation.pattern = pattern; IEquation.expr = expr}) =
  begin
    match pattern with
    | [] -> assert false
    | [name, _, _] ->
      let file, expr, _ = compile_expr types file node fundec expr in
      set_instr_and_append_to_body fundec (get_lval fundec node name file) expr
    | patternList ->
      let file, expr, expr_type = compile_expr types file node fundec expr in
      List.iter2 (fun (patternName, _, _) expr_field ->
          set_instr_and_append_to_body fundec (get_lval fundec node patternName file) (match expr with
            | Lval (Var v, _) ->
              Lval (Var v, Field (expr_field, NoOffset))
            | _ -> assert false))
        patternList
        (match expr_type with
        | TComp ({cfields; _}, _) -> cfields
        | _ -> assert false);
  end;
  file

(* Function to compile an atom *)
let rec compile_atom types file node fundec = function
  | Atom.Const c -> GoblintCil.Const (trans_const types c)
  | Ident id -> Lval (get_lval fundec node id file)

let compile_function_declaration types file node mem_comp =

  let compile_init_locals types file fundec init_locals =
    List.fold_left
      (fun (fundec, locals) (({name; _}, typeList, _), init_value) ->
         fundec, (match init_value with
                  | Some init_value -> makeLocalVar ~init:(SingleInit (Const (trans_const types init_value))) fundec (clean_name name) (translate_type types typeList)
                  | None -> makeLocalVar fundec (clean_name name) (translate_type types typeList))::locals)
      (fundec, []) init_locals
  in
  let compile_func_type types file node return_type mem_comp =
    let create_params types node =
      let params =
        List.map (fun (id, typeList, _) ->
          let name = clean_name id.name in
          let typ = translate_type types typeList in
          (name, typ, []))
          node.INode.input_step
      in
      function
      | Some mem_comp when mem_comp.cfields <> [] ->
        ("mem", TPtr (TComp (mem_comp, []), []), []) :: params
      | _ -> params
    in
    let create_func_type return_type l =
      TFun (return_type, (if l = [] then None else Some l), false, [])
    in
    file, create_func_type return_type (create_params types node mem_comp)
  in  
  let file, return_type = compile_return_type types file node in
  let file, func_type = compile_func_type types file node return_type mem_comp in
  let func_var = makeGlobalVar (node.INode.name.name) func_type in
  let fundec, _ = compile_init_locals types file (mk_fundec func_var) node.INode.local in
  
  let var_name = match return_type with
    | TComp (compinfo, _) -> "ret_"
    | _ -> let name, _, _ = List.hd node.INode.output_step in name.name
  in
  ignore @@ makeLocalVar fundec var_name return_type;
    
  List.iter (fun equation -> ignore (compile_equation types file node fundec equation)) node.INode.compute;
  let fundec = 
    (if node.INode.update <> [] then
      List.iter (fun (field, atom) ->
          set_instr_and_append_to_body fundec 
            (Mem (Lval (Var (find_formal fundec "mem"), NoOffset)), Field (find_field_list field.name (find_gcomp (node.INode.name.name^"_mem") file.globals).cfields, NoOffset)) 
            (compile_atom types file node fundec atom))
        node.INode.update;
    fundec)
  in
  append_stmt_to_body fundec (mkStmt (Return (Some (Lval (Var (find_local fundec (match node.INode.output_step with
    | [(name, _, _)] -> name.name
    | _ -> "ret_")), NoOffset)), locUnknown)));
  file, fundec

(* Function to compile a node *)
let compile_node types file node =
  let compile_memory_component types file node =
    (if not node.INode.need_mem then None 
    else
      let mem_comp = compile_mem_comp types file.globals node in
      file.globals <- (GCompTag (mem_comp, locUnknown))::file.globals;
      compile_init types file node mem_comp;
      Some mem_comp)
  in
  let _, fundec = compile_function_declaration types file node (compile_memory_component types file node) in
  { file with globals = (GFun (fundec, locUnknown))::file.globals }

let compile_main file ast main_node no_sleep =
  let main_node_imp =
    List.find (fun { INode.name; _ } -> name.name = main_node) ast.IFile.nodes
  in
  let fundec =
    mk_fundec
      (makeGlobalVar "main" (TFun (TInt (IInt, []), Some ["argc", TInt (IInt, []), []; "argv", TArray (TPtr (TInt (IChar, []), []), None, []), []], false, [])))
  in
  let allocate_memory_if_needed main_node_imp fundec file main_node =
    if not main_node_imp.INode.need_mem then [], fundec
    else
      let mem = makeLocalVar fundec "mem" (TComp (find_gcomp (main_node ^ "_mem") file.globals, [])) in
      append_stmt_to_body fundec (mkStmtOneInstr
      (Call
        ( None,
          Lval (Var (find_fun (main_node ^ "_init") file.globals).svar, NoOffset),
          [AddrOf (Var mem, NoOffset)],
          locUnknown,
          locUnknown )));
      [AddrOf (Var mem, NoOffset)], fundec
  in
  let addr_mem_lval, fundec = allocate_memory_if_needed main_node_imp fundec file main_node in
  let input_step_length = List.length main_node_imp.INode.input_step in
  let atoied_vars_lvals =
    List.init input_step_length (fun i ->
        Lval (Var (makeLocalVar fundec (Format.sprintf "argv_%d" i) (TInt (IInt, []))), NoOffset))
  in
  let atois =
    List.mapi
      (fun i -> function
        | Lval e ->
            Call
              ( Some e,
                int_read_lval,
                [],
                locUnknown,
                locUnknown )
        | _ -> failwith "Unexpected pattern")
      atoied_vars_lvals in
      
  let atoi_block =
    List.map mkStmtOneInstr atois
  in
  let step_fun = find_fun main_node file.globals in
  let res_lval =
    Var (makeLocalVar fundec "res" (match step_fun.svar.vtype with TFun (v, _, _, _) -> v | _ -> failwith "Unexpected pattern")), NoOffset
  in
  let step_call = Call (Some res_lval, Lval (Var step_fun.svar, NoOffset), addr_mem_lval @ atoied_vars_lvals, locUnknown, locUnknown) in

let rec get_type_str = function
  | TEnum _ | TInt _ -> "%d"  (* Handling enums and integers *)
  | TFloat _ -> "%f"          (* Handling floating-point numbers *)
  | TComp (ci, _) ->          (* Handling composite types (tuples/structs) *)
      String.concat ", " (List.map (fun fi -> get_type_str fi.ftype) ci.cfields)  (* Concatenating the types of the fields *)
  | _ -> failwith "Not Implemented"  (* Handling other cases *)

in
  (* Format the output *)
  let str_fmt =
    match step_fun.svar.vtype with
    | TFun (v, _, _, _) ->
      let type_str = get_type_str v in
      GoblintCil.Const (CStr (Format.sprintf "\"%s%s\"" type_str "\\n", No_encoding))
    | _ -> failwith "Unexpected pattern"
  in
  let while_block =
    mkBlock
      (atoi_block
        @ [mkStmtOneInstr step_call; mkStmtOneInstr (Call (None, printf_lval, [str_fmt; Lval res_lval], locUnknown, locUnknown));
          mkStmtOneInstr (Call (None, fflush_lval, [mk_int_exp 0], locUnknown, locUnknown));
          mkStmtOneInstr (Call (None, sleep_lval, [mk_int_exp 333333], locUnknown, locUnknown))])
  in
  { file with globals = (GFun ({ fundec with sbody = append_stmts [mkStmt (Loop (while_block, locUnknown, locUnknown, None, None))] fundec.sbody }, locUnknown)) :: file.globals }


let compile_enums types =
  List.map (fun Asttypes.{name; constructors} ->
    GType ({ tname = name; ttype = mk_enum types name; treferenced = false}, locUnknown)) types
    
let compile ast main_node file_name no_sleep =
  let file = compile_main (List.fold_left (compile_node ast.IFile.types) { fileName = file_name; globals = []; globinit = None; globinitcalled = false; } ast.IFile.nodes) ast main_node no_sleep  
  in
  file.globals <- 
    List.append
      [ GText "#include <stdlib.h>"
      ; GText "#include <printf.h>"
      ; GText "#include <unistd.h>"
      ; GText "#include <stdio.h>"
      ; GText  "int int_read() {\n  int var;\n  scanf(\"%d\", &var);\n  return var;\n}"
      ]
      (compile_enums ast.IFile.types @ (List.rev file.globals));
  file