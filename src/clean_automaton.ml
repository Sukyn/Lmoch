open Parse_ast
open Asttypes

type err = UnexpPat of string
exception Error of positionRange * err

(* Utility functions *)
let raise_err loc e = raise (Error (loc, e))
let reportErrorFormat fmt = function
  | UnexpPat id -> Format.fprintf fmt "Undefined type constructor : %s" id

(* Unique name generation *)
let counter = ref 0
let uniq_name base =
  incr counter;
  Printf.sprintf "%s__%d" base !counter

(* Constructors for AST nodes *)
let mk_expr desc loc = { PExpr.desc = desc; loc }
let mk_pat desc loc = { PPattern.desc = desc; loc }
let mk_constr_expr tname cid = mk_expr (PExpr.Const (Asttypes.EnumeratedDataConstant (tname, Some cid)))
let mk_constr_exprs type_info loc = List.map (fun id -> mk_constr_expr type_info.name id loc) type_info.constructors
let mk_merge merge_over constrs eqs = mk_expr (PExpr.Merge (merge_over, List.combine constrs eqs))

(* Translation function *)
let translate { loc_automaton; core_automaton } =
  let constr_names, eq_list =
    List.split (List.map (fun { case_automaton = { case_type; case_equation } } ->
      case_type, case_equation) core_automaton) in
  let type_info = Asttypes.{ name = uniq_name "typ"; constructors = constr_names } in
  let init =
    match constr_names with
    | first_name::_ -> mk_constr_expr (type_info.name) first_name loc_automaton
    | [] -> raise_err loc_automaton (UnexpPat "No constructors found") in

  let check_pattern pattern =
    if not (List.mem pattern constr_names) then raise_err loc_automaton (UnexpPat pattern) in
  List.iter (fun { out_automaton } -> List.iter check_pattern out_automaton) core_automaton;

  let state_name = uniq_name "state" in
  let state_init = state_name, Asttypes.EnumeratedDataType type_info.name in

  let extract_expr = function
    | Eq eq -> Some eq.expr
    | _ -> raise_err loc_automaton (UnexpPat "") in

  let eq_vars = List.filter_map extract_expr eq_list in
  let (cond_locals, vars), eqs_state =
    List.fold_left_map
      (fun (cond_acc, vars_acc)
           { cond_automaton; out_automaton; case_automaton = { case_type; case_location; _ } } ->
        let exprs, names, pats =
          List.fold_right (fun (expr, name, pat) (exprs, names, pats) ->
            (expr::exprs, name::names, pat::pats))
            (List.map (fun _ ->
              let name = uniq_name "cond" in
              mk_expr (Ident name) case_location, name, mk_pat (PPattern.Ident name) case_location) out_automaton)
            ([], [], []) in
        let mk_constr output = mk_constr_expr type_info.name output loc_automaton in
        let out_auto = List.map mk_constr out_automaton in
        ((List.map2 (fun pat expr -> Eq { pattern = pat; expr }) pats cond_automaton) @ cond_acc, names @ vars_acc),
        if List.length exprs <> List.length out_auto then
          raise_err loc_automaton (UnexpPat "Mismatch in build_cond")
        else
          List.fold_right2
            (fun var out acc -> mk_expr (Op (Asttypes.Op_if, [var; out; acc])) loc_automaton)
            exprs out_auto (mk_constr case_type))
      ([], []) core_automaton in

  let over = mk_expr (Ident state_name) loc_automaton in
  let main_var =
    match eq_list with
    | Eq eq :: _ -> eq.pattern
    | _ -> raise_err loc_automaton (UnexpPat "Expected Eq pattern in eq_list") in

  let merge_var =
    Eq { pattern = main_var;
         expr = mk_merge over (mk_constr_exprs type_info loc_automaton) eq_vars loc_automaton } in

  let merge_state =
    let exprs = mk_constr_exprs type_info loc_automaton in
    let merge_expr = mk_merge over exprs eqs_state loc_automaton in
    Eq { pattern = mk_pat (PPattern.Ident state_name) loc_automaton;
         expr = mk_expr (PExpr.Arrow (init, mk_expr (Pre merge_expr) loc_automaton)) loc_automaton } in

  type_info, vars, state_init, cond_locals @ [merge_state; merge_var]

(* Clean function *)
let clean { nodes; additional_types } =
  let acc_types, proc_nodes =
    List.fold_left_map (fun acc node ->
      let new_types, trans_node =
        let (local_vars, type_info), node_eqs =
          let proc_eq acc = function
            | Automaton automaton ->
              let type_info, variables, state, trans_auto = translate automaton in
              let updated_vars = List.map (fun var -> (var, Asttypes.BooleanType)) variables in
              ((state :: updated_vars @ fst acc, type_info :: snd acc), trans_auto)
            | eq -> (acc, [eq]) in
          let acc, processed = List.fold_left_map proc_eq ([], []) node.node_equs in
          acc, List.concat processed in
        type_info, { node with node_equs = node_eqs; node_localvars = local_vars @ node.node_localvars } in
      (new_types @ acc, trans_node)) [] nodes in
  { nodes = proc_nodes; additional_types = additional_types @ acc_types }
