open Asttypes
open Parse_ast
open Typed_ast
open Format
open Print_utils

module S = Set.Make(Ident)
module M = Map.Make(String)

type error =
  | Clash of string
  | ExpectedBase of typeList
  | ExpectedNum of typeList
  | ExpectedType of typeList * typeList
  | FlatTuple
  | InputVar of string
  | NotATuple
  | NotExhaustiveMerge of string
  | Other of string
  | TooFewArguments
  | UnboundNode of string
  | UnboundVar of string
  | UndefinedOutputs of string list
  | UnknownEnumType of string
  | Unreachable of string

exception Error of positionRange * error
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let nil = function
  | BooleanType -> (Expr.TE_const (BooleanConstant false))
  | IntegerType -> TE_const (IntegerConstant 0)
  | RealType -> TE_const (RealConstant 0.0)
  | EnumeratedDataType s -> TE_const (EnumeratedDataConstant (s, None))

let error loc e = raise (Error (loc, e))

(* Function to print a type *)
let print_type fmt = function
  | [] -> fprintf fmt "empty tuple"
  | [t] -> print_base_type fmt t
  | (t::tl) ->
    fprintf fmt "(";
    print_base_type fmt t;
    List.iter (fun t -> fprintf fmt " * %a" print_base_type t) tl;
    fprintf fmt ")"

let print_list fmt list = List.iter (fun x -> fprintf fmt " %s" x) list

let reportErrorFormat fmt = function
  | Clash varId -> fprintf fmt "The variable '%s' is defined multiple times in the same scope" varId
  | ExpectedBase typeList -> fprintf fmt "The expression has the type '%a', but a simple type was expected" print_type typeList
  | ExpectedNum typeList -> fprintf fmt "The expression has the type '%a', but 'int' or 'real' was expected" print_type typeList
  | ExpectedType (actualType, expectedType) -> fprintf fmt "The expression has type '%a', but is expected to have type '%a'" print_type actualType print_type expectedType
  | FlatTuple -> fprintf fmt "Nested tuples are forbidden in this context"
  | InputVar varId -> fprintf fmt "The variable '%s' is an input variable and cannot be modified" varId
  | NotATuple -> fprintf fmt "The expression is not a tuple"
  | NotExhaustiveMerge mergeType -> fprintf fmt "The merge operation is not exhaustive over the type [%s]" mergeType
  | Other message -> fprintf fmt "An error occurred: %s" message
  | TooFewArguments -> fprintf fmt "Too few arguments were provided to the function"
  | UnboundNode nodeId -> fprintf fmt "The node '%s' is not defined in the current scope" nodeId
  | UnboundVar varId -> fprintf fmt "The variable '%s' is not defined in the current scope" varId
  | UndefinedOutputs outputList -> fprintf fmt "These output variables are undefined: %a" print_list outputList
  | UnknownEnumType adtType -> fprintf fmt "The type '%s' was not found" adtType
  | Unreachable point -> fprintf fmt "An internal error occurred (this point should be unreachable): [%s]" point

(* Module Delta: provides functionality for handling primitive types and nodes *)
module Delta = struct

  (* Hashtable for storing nodes *)
  let nodes = Hashtbl.create 97

  (* Function to find a node. Returns the node and a boolean indicating if it's a primitive *)
  let find node =
    match Hashtbl.find_opt nodes node with
    | Some n -> n
    | None -> raise Not_found

  (* Function to add a node to the hashtable *)
  let add node_name node_type =
    let node_id = Ident.make node_name Ident.Node in
    Hashtbl.replace nodes node_name (node_id, node_type);
    node_id

  (* Function to save the current state of the hashtable *)
  let save () = Hashtbl.fold (fun key (_,typeList) env -> (key,typeList)::env) nodes []
end

type varType = InputVar | PatternVar

(* Gamma module represents a symbol table for variable bindings *)
module Gamma = struct

  (* t represents the type of the symbol table *)
  type t = (Ident.t * basicType * varType) M.t

  (* empty creates an empty symbol table *)
  let empty = M.empty

  (* add inserts a new variable into the symbol table *)
  let add location symbol_table variable type_ varType =
    let kind = Ident.Stream in
    let new_variable = Ident.make variable kind in
    M.add variable (new_variable, type_, varType) symbol_table

  (* adds inserts a list of variables into the symbol table *)
  let adds location varType =
    List.fold_left (fun symbol_table (variable, type_) -> add location symbol_table variable type_ varType)

  (* find retrieves a variable from the symbol table *)
  let find location symbol_table variable = 
    match M.find_opt variable symbol_table with
    | Some value -> value
    | None -> error location (UnboundVar variable)

  (* patts_vars retrieves all pattern variables from the symbol table *)
  let patts_vars symbol_table =
    M.fold (fun _ (variable, _, varType) set -> if varType=PatternVar then S.add variable set else set) symbol_table S.empty

  (* in_vars retrieves all input variables from the symbol table *)
  let in_vars symbol_table =
    M.fold (fun _ (variable, _, varType) set -> if varType=InputVar then S.add variable set else set) symbol_table S.empty

end

type env = { vars : (Ident.t * basicType * varType) M.t; types : enumeratedDataType list}

let base_type_of_type location type_list =
  match type_list with
  | [single_type] -> single_type
  | _ -> error location (ExpectedBase type_list)

let is_type_compatible actual_type_list expected_type_list =
  try
    List.for_all2 (=) actual_type_list expected_type_list
  with Invalid_argument _ -> 
    error dummy_loc (Other "The actual and expected type lists must have the same length")

let type_constant = function
  | BooleanConstant _ -> [BooleanType]
  | IntegerConstant _ -> [IntegerType]
  | RealConstant _ -> [RealType]
  | EnumeratedDataConstant (s, _) -> [EnumeratedDataType s]

let rec type_expr env { PExpr.desc; loc } =
  let desc, t = type_expr_desc env loc desc in
  { tdesc = desc; ttype = t; Expr.tloc = loc; }

and type_two_exprs env e1 e2 =
  (type_expr env e1, type_expr env e2)

and type_check_unary_and_create_node env expr operator targetType =
  (Expr.TE_op (operator, [expected_type env expr targetType]), targetType)

and type_check_binary_and_create_node env expr1 expr2 operator targetType =
  (Expr.TE_op (operator, [expected_type env expr1 targetType; 
                          expected_type env expr2 targetType]), targetType)

and type_op env loc = function 
  | (Op_not, [expr]) -> type_check_unary_and_create_node env expr Op_not [BooleanType]
  | ((Op_and | Op_or | Op_impl as logicalOperator), [expr1; expr2]) ->
      type_check_binary_and_create_node env expr1 expr2 logicalOperator [BooleanType]
  | (Op_mod, [e1; e2]) ->
      type_check_binary_and_create_node env e1 e2 Op_mod [IntegerType]
  | (Op_sub, [expr]) ->
      let typedExpr = type_expr env expr in
      begin match typedExpr.ttype with
        | [IntegerType] -> TE_op (Op_sub, [typedExpr]) , [IntegerType]
        | [RealType] -> TE_op (Op_sub, [typedExpr]) , [RealType]
        | typeList -> error expr.loc (ExpectedNum (typeList))
      end
  | ((Op_add | Op_sub | Op_mul | Op_div as operator), [e1; e2]) ->
      let typedExpr1, typedExpr2 = type_two_exprs env e1 e2 in
      begin match typedExpr1.Expr.ttype, typedExpr2.Expr.ttype with
        | [IntegerType], [IntegerType] ->
          TE_op (operator, [typedExpr1; typedExpr2]), [IntegerType]
        | [RealType], [RealType] ->
          TE_op(operator, [typedExpr1 ;typedExpr2 ]), [RealType]
        | [(IntegerType | RealType)], typeList -> error e2.loc (ExpectedNum (typeList))
        | typeList, _ -> error e1.loc (ExpectedNum (typeList))
      end
  | (Op_eq | Op_neq as operator, [e1; e2]) ->
      let typedExpr1, typedExpr2 = type_two_exprs env e1 e2 in
      begin match typedExpr1.Expr.ttype, typedExpr2.Expr.ttype with
        | [t1], [t2] when t1 = t2 ->
          TE_op (operator, [typedExpr1; typedExpr2]), [BooleanType]
        | _ ->
          error loc (ExpectedType (typedExpr2.Expr.ttype, typedExpr1.Expr.ttype))
      end
  | (Op_lt | Op_le | Op_gt | Op_ge as operator, [e1; e2]) ->
      let typedExpr1, typedExpr2 = type_two_exprs env e1 e2 in
      begin match typedExpr1.Expr.ttype, typedExpr2.Expr.ttype with
        | [IntegerType], [IntegerType]
        | [RealType], [RealType] ->
          TE_op (operator, [typedExpr1; typedExpr2]), [BooleanType]
        | _ ->
          error loc (ExpectedType (typedExpr2.Expr.ttype, typedExpr1.Expr.ttype))
      end
    | (Op_if, [e1; e2; e3]) ->
        let te1 = expected_type env e1 ([BooleanType]) in
        let te2 = type_expr env e2 in
        let te3 = type_expr env e3 in
        let well_typed = is_type_compatible [te2.Expr.ttype] [te3.Expr.ttype] in
        if well_typed then
          let tt = te2.Expr.ttype in
          TE_op(Op_if, [te1; te2; te3]), tt
        else
         error loc (ExpectedType (te3.Expr.ttype, te2.Expr.ttype))
  
  | _ -> error loc TooFewArguments

and type_expr_desc env loc = function
  | Const constant -> TE_const constant, type_constant constant
  | Ident identifier -> 
    let identifier, typeList, _ = Gamma.find loc env.vars identifier in
    TE_ident identifier, [typeList]
  | Op (op, params) -> type_op env loc (op, params)
  | App (functionName, exprList) ->
      begin try
          let (functionName, (inputType, outputType)) = Delta.find functionName in
          let typedExprList = type_args env loc inputType exprList in
          let appNode = Expr.TE_app(functionName, typedExprList) in
          appNode, begin match outputType with
                        | [] -> assert false
                        | _ -> outputType
                   end
        with Not_found ->
          error loc (UnboundNode functionName)
      end
  | Arrow (e1, e2) ->
      let typedExpr1, typedExpr2 = type_two_exprs env e1 e2 in
      if is_type_compatible typedExpr1.Expr.ttype typedExpr2.Expr.ttype then
        let trueConst = {Expr.tdesc = TE_const (BooleanConstant (true)); Expr.ttype = [BooleanType]; Expr.tloc = dummy_loc} in
        let falseConst = {Expr.tdesc = TE_const (BooleanConstant (false)); Expr.ttype = [BooleanType]; Expr.tloc = dummy_loc} in
        let condition = {Expr.tdesc = TE_fby (trueConst, falseConst); Expr.ttype = [BooleanType]; Expr.tloc = dummy_loc} in
        TE_op (Op_if, [condition; typedExpr1; typedExpr2;]), typedExpr1.Expr.ttype
      else 
        error typedExpr2.Expr.tloc (ExpectedType (typedExpr2.Expr.ttype, typedExpr1.Expr.ttype))
  | Pre e ->
    let typedExpr = type_expr env e in
    let typeListExpr = typedExpr.Expr.ttype in
    let nilNode = {Expr.tdesc = nil (List.hd typedExpr.Expr.ttype); Expr.ttype = typeListExpr; Expr.tloc = typedExpr.Expr.tloc} in
    TE_fby (nilNode, typedExpr), typeListExpr
  | When (e1, c, e2) ->
      let typedExpr1, typedExpr2 = type_two_exprs env e1 e2 in
      if typedExpr2.Expr.ttype = [BooleanType] then 
        TE_when (typedExpr1, c, typedExpr2), typedExpr1.Expr.ttype
      else begin
          match List.find_opt (fun {name; constructors} -> List.mem c constructors) env.types with
          | Some e when [EnumeratedDataType e.name] = typedExpr2.Expr.ttype -> TE_when (typedExpr1, c, typedExpr2), typedExpr1.Expr.ttype
          | _ -> error typedExpr2.Expr.tloc (ExpectedType(typedExpr2.Expr.ttype, [BooleanType])) 
        end
  | Tuple exprList ->
    let typedExprList = List.map (type_expr env) exprList in
    TE_tuple typedExprList,
    (List.map (fun e -> base_type_of_type e.Expr.tloc e.Expr.ttype) typedExprList)
  | Merge (x, mergeList) ->
      let mergeBody = List.map (fun (l, r) -> type_expr env l, type_expr env r) mergeList in
      let matchType, exprType = ref None, ref None in
      let check_type_compatibility type_ prevType loc = 
        if not (is_type_compatible type_ prevType) then
          error loc (ExpectedType (prevType, type_)) in

      List.iter (fun ({Expr.ttype = type1; Expr.tloc = loc1; _}, {Expr.ttype = type2; Expr.tloc = loc2; _}) ->
        match !matchType, !exprType with
          | None, None ->
            matchType := Some type1; exprType := Some type2;
          | (Some prevMatchType, Some prevExprType) ->
            check_type_compatibility type1 prevMatchType loc1;
            check_type_compatibility type2 prevExprType loc2;
          | _ -> assert false)
        mergeBody;
      let typedIdentifier, typeListExpr, _ = Gamma.find x.loc env.vars (match x.desc with Ident id -> id 
                                                                                          | _ -> assert false) in
      begin match typeListExpr with
        | BooleanType -> ()
        | EnumeratedDataType typeName ->
            verif_mergebody env typeName mergeBody x.loc
        | _ -> error x.loc (ExpectedType ([typeListExpr], [EnumeratedDataType "?t"]))
      end;
      TE_merge ({ Expr.tdesc = TE_ident typedIdentifier;
                  Expr.ttype = [typeListExpr];
                  Expr.tloc  = x.loc }, mergeBody), (snd @@ List.hd mergeBody).Expr.ttype
  | Reset (functionId, exprList, expr) ->
      try
        let (functionName, (inputType, outputType)) = Delta.find functionId in
        let typedExprList = type_args env loc inputType exprList in
        let typedExpr = type_expr env expr in
        if typedExpr.Expr.ttype <> [BooleanType] then
          error loc (ExpectedType ([BooleanType], typedExpr.Expr.ttype));
        TE_reset (functionName, typedExprList, typedExpr), match outputType with
          | [] -> assert false
          | _ -> outputType
      with Not_found ->
        error loc (UnboundNode functionId)

and compare_constructors a b = match a, b with
  | {Expr.tdesc = TE_const (EnumeratedDataConstant (_, Some id1)); _}, id -> id1 = id
  | _ -> assert false

and throw_error loc tname = error loc (NotExhaustiveMerge tname)

and verif_mergebody env tname l id_loc =
  let tl = match List.find_opt (fun {name; _} -> name = tname) env.types with
    | None -> error id_loc (UnknownEnumType tname)
    | Some e -> e in
  let compare_f a b = match a, b with
    | {Expr.tdesc = TE_const (EnumeratedDataConstant (_, Some id1)); _},
      {Expr.tdesc = TE_const (EnumeratedDataConstant (_, Some id2)); _} -> compare id1 id2
    | _ -> assert false
  in
  let sl = List.map fst l |> List.sort compare_f in
  let stl = List.sort compare tl.constructors in

  try
    if not @@ List.for_all2 compare_constructors sl stl then throw_error id_loc tname
  with Invalid_argument _ ->
    throw_error id_loc tname

and type_args env loc params_ty el =
  let tel = List.map (type_expr env) el in
  let actual_types = List.flatten (List.map (fun te -> te.Expr.ttype) tel) in
  let well_typed = is_type_compatible actual_types params_ty in
  if well_typed then tel
  else error loc (ExpectedType (actual_types, params_ty));

and expected_type env e tt =
  let te = type_expr env e in
  let typeList = te.Expr.ttype in
  if typeList = tt then te
  else error e.loc (ExpectedType (typeList, tt))

and expected_base_type env e =
  match type_expr env e with
  | {Expr.ttype = [_]; _} as te -> te
  | {Expr.ttype; _} -> error e.loc (ExpectedBase ttype)

let rec type_patt env { PPattern.desc; PPattern.loc } =
  let tdesc, ttype = type_patt_desc env loc desc in
  { Patt.tdesc; Patt.ttype; Patt.tloc = loc; }
  
and find_pattern_var loc env x =
  match Gamma.find loc env.vars x with
  | x, typeList, PatternVar -> x, typeList
  | _  -> error loc (InputVar x)

and type_patt_desc env loc patt =
  match patt with
  | PPattern.Ident x ->
      let x, typeList = find_pattern_var loc env x in
      [x], [typeList]
  | PPattern.Tuple pl ->
      let pl_tyl = List.map (find_pattern_var loc env) pl in
      List.split pl_tyl

let type_equation env = function
  | Eq eq ->
      let patt = type_patt env eq.pattern in
      let expr = type_expr env eq.expr in
      if is_type_compatible expr.Expr.ttype patt.Patt.ttype then
        { Equation.tpatt = patt; Equation.texpr = expr; }
      else
        error eq.expr.loc (ExpectedType (expr.Expr.ttype, patt.Patt.ttype))
  | Match _ ->
      error dummy_loc (Other "Pattern matching is not implemented")
  | Automaton {loc_automaton = loc; _} ->
      error loc (Unreachable "Uncompiled automaton")
      
let add_vars_of_patt loc s eq =
  let add x s =
    if S.mem x s then error loc (Clash x.Ident.name);
    S.add x s
  in
  List.fold_left (fun s x -> add x s) s eq.Equation.tpatt.Patt.tdesc

let check_outputs loc env equs =
  let s = List.fold_left (add_vars_of_patt loc) S.empty equs in
  let undefined_outputs = S.diff (Gamma.patts_vars env.vars) s in
  if not (S.is_empty undefined_outputs) then
    error loc (UndefinedOutputs
                 (List.map (fun x -> x.Ident.name) (S.elements undefined_outputs)))

let map_vars loc env vars =
  List.map (fun (x, typeList) -> let x', _, _ = Gamma.find loc env.vars x in (x', typeList)) vars

let type_node ptypes n =
  let env = Gamma.adds n.node_location PatternVar Gamma.empty (n.node_outputs@n.node_localvars) in
  let env = Gamma.adds n.node_location InputVar env n.node_inputs in
  let env = { vars = env; types = ptypes } in
  let equs = List.map (type_equation env) n.node_equs in
  check_outputs n.node_location env equs;
  let t_in = List.map (fun (_, typeList) -> typeList) n.node_inputs in
  let t_out = List.map (fun (_, typeList) -> typeList) n.node_outputs in
  let name = Delta.add n.node_name (t_in,t_out) in
  let input = map_vars n.node_location env n.node_inputs in
  let output = map_vars n.node_location env n.node_outputs in
  let local =
    List.map
      (fun (x, typeList) -> let x', _, _ = Gamma.find n.node_location env.vars x in (x', typeList), None)
      n.node_localvars
  in
  let node =
    { Node.tname = name;
      Node.tinput = input;
      Node.toutput = output;
      Node.tlocal = local;
      Node.teqs = equs;
      Node.tloc = n.node_location; }
  in
  node

let check_main ft main =
  match Delta.find main with
  | exception Not_found -> error dummy_loc (UnboundNode main)
  | _ -> ()

let type_file f main =
  let ft = List.map (type_node f.additional_types) f.nodes in
  if main <> "" then check_main ft main;
  { File.tnodes = ft; File.ttypes = f.additional_types }
