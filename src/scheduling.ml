open Clocked_ast
open Format

type error = Causality
exception Error of error
let reportErrorFormat fmt = function
  | Causality -> fprintf fmt "A causality error occurred, indicating a circular dependency"
 
module IdentifierSet = Set.Make(Ident)
module DependencyGraph = Set.Make(struct
  type t = Ident.t * IdentifierSet.t * CEquation.t
  let compare (id1, _, _) (id2, _, _) = Ident.compare id1 id2
end)

let rec collect_vars set {CExpr.cdesc = expression} =
  match expression with
  | CE_const _ -> set
  | CE_ident id -> IdentifierSet.add id set
  | CE_pre expr -> set
  | CE_op (_, exprs) | CE_app (_, exprs) | CE_prim (_, exprs) | CE_tuple exprs -> 
    List.fold_left collect_vars set exprs
  | CE_merge (case_id, case_exprs) ->
      List.fold_left
        (fun acc (_, expr) -> collect_vars acc expr)
        (collect_vars set case_id)
        case_exprs
  | CE_fby _ -> set
  | CE_when (expr, _, _) -> collect_vars set expr
  | CE_reset (_, exprs, expr) -> collect_vars (List.fold_left collect_vars set exprs) expr

let add_to_graph node deps eq graph = DependencyGraph.add (node, deps, eq) graph

let schedule_equations inputs equations =
  let init_graph = List.fold_left (fun acc eq ->
    let vars = collect_vars IdentifierSet.empty eq.CEquation.cexpr in
    let pattern_vars = List.fold_right IdentifierSet.add eq.CEquation.cpattern.cdesc IdentifierSet.empty in
    IdentifierSet.fold (fun pv acc -> add_to_graph pv vars eq acc) pattern_vars acc
  ) DependencyGraph.empty equations in
  let input_set = List.fold_left (fun acc (input, _, _) -> IdentifierSet.add input acc) IdentifierSet.empty inputs in
  
  let rec order_graph ordered graph = match DependencyGraph.is_empty graph with
    | true -> List.rev ordered
    | _ -> 
      let (no_deps, remaining) = DependencyGraph.partition (fun (_, deps, _) -> IdentifierSet.is_empty deps) graph in
      if DependencyGraph.is_empty no_deps then raise (Error Causality);
      let scheduled = DependencyGraph.fold (fun (n, _, _) acc -> IdentifierSet.add n acc) no_deps IdentifierSet.empty in
      let new_graph = DependencyGraph.fold (fun (n, deps, eq) acc -> add_to_graph n (IdentifierSet.diff deps scheduled) eq acc) remaining DependencyGraph.empty in
      let ordered = DependencyGraph.fold (fun (_, _, eq) acc -> if List.mem eq acc then acc else eq :: acc) no_deps ordered in
      order_graph ordered new_graph
  in
  order_graph [] (DependencyGraph.fold (fun (n, deps, eq) acc -> add_to_graph n (IdentifierSet.diff deps input_set) eq acc) init_graph DependencyGraph.empty)
  
let schedule_node node = { node with CNode.equs = schedule_equations node.CNode.input node.CNode.equs; }
let schedule file = { file with CFile.nodes = List.map schedule_node file.CFile.nodes; }