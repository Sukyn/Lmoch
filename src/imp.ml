(* Module imports *)
open Imp_ast
open Clocked_ast
open Ident

(* Flatten_tuple function: Flattens nested tuples into a single list of elements *)
let rec flatten_tuple elements =
  List.concat_map (function
    | { CExpr.cdesc = CE_tuple el_list; _ } -> flatten_tuple el_list
    | el -> [el]
  ) elements

(* Generate a unique identifier with a prefix *)
let gen_id prefix =
  let counter = ref 0 in
  fun node ->
    incr counter;
    Ident.make (node.name ^ "_" ^ prefix ^ string_of_int !counter) Ident.Stream

(* Specialized identifier generators for 'next' and 'mem' *)
let gen_next_id, gen_mem_id = gen_id "next", gen_id "mem"

(* Hashtable for storing nodes *)
let hashtable = Hashtbl.create 11

(* Convert clock types to a list *)
let rec list_of_clock_type = function
  | Ck ck -> [ck]
  | Cprod (Ck ct :: cls) -> ct :: list_of_clock_type (Cprod cls)
  | _ -> []

(* Compile a pattern *)
let compile_pattern { CPatt.cdesc; ctype; cclock; _ } =
  List.map2 (fun (id, typeList) cl -> (id, typeList, cl))
    (List.combine cdesc ctype) (list_of_clock_type cclock)

(* Handle CE_merge case *)
let rec handle_ce_merge_case constant expr =
  let case = match constant.CExpr.cdesc with
    | CE_const (EnumeratedDataConstant (lty, Some name)) ->
      { IExpr.idesc = IE_const (EnumeratedDataConstant (lty, Some name)); IExpr.itype = constant.CExpr.ctype }
    | CE_const (BooleanConstant b) ->
      { IExpr.idesc = IE_const (EnumeratedDataConstant ("inductive_bool", Some (if b then "True" else "False")));
        IExpr.itype = [EnumeratedDataType "inductive_bool"] }
    | _ -> failwith "Unexpected pattern in CE_merge"
  in
  (case, compile_base_expr expr)

(* Compile base expressions *)
and compile_base_expr { CExpr.cdesc; ctype; _ } =
  let idesc = match cdesc with
    | CE_const c -> IExpr.IE_const c
    | CE_ident id -> IE_ident id
    | CE_op (op, exprList) -> IE_op (op, List.map compile_base_expr exprList)
    | CE_tuple exprList -> IE_tuple (List.map compile_base_expr exprList)
    | CE_prim (f, exprList) -> IE_prim (f, List.map compile_base_expr exprList)
    | CE_when (e, _, _) -> (compile_base_expr e).idesc
    | CE_merge (id, caseList) -> IE_case (compile_base_expr id, List.map (fun (cnst, e) -> handle_ce_merge_case cnst e) caseList)
    | _ -> assert false
  in
  { idesc; itype = ctype }

(* Compile atomic expressions *)
let compile_atom = function
  | { CExpr.cdesc = CE_const c; _ } -> Atom.Const c
  | { cdesc = CE_ident id; _ } -> Ident id
  | _ -> failwith "Unexpected pattern in compile_atom"

(* Compile a list of atomic expressions *)
let compile_atoms atoms = match atoms.CExpr.cdesc with
  | CE_tuple atomList -> List.map compile_atom atomList
  | CE_const _ | CE_ident _ -> [compile_atom atoms]
  | _ -> failwith "Unexpected pattern in compile_atoms"

(* Convert clock types to a list *)
let rec ck_of_ct = function
  | Ck ck -> [ck]
  | Cprod ((Ck ck) :: cls) -> ck :: ck_of_ct (Cprod cls)
  | _ -> failwith "Unexpected pattern in ck_of_ct"

(* Compile fby (followed by) expressions *)
let compile_fby (e1, e2) patt vars mem init compi updatei =
  let id, typeList, ck = match patt.CPatt.cdesc, patt.CPatt.ctype, patt.CPatt.cclock with
    | [id], [typeList], Ck ck -> (id, typeList, ck)
    | _ -> assert false
  in
  let next_id = gen_next_id id in
  let cnext_id = (next_id, typeList, ck) in
  let fby_node = (next_id, compile_atom e1) in
  let ce = { IExpr.idesc = IE_mem next_id; IExpr.itype = [typeList]; } in
  let comp = { IEquation.pattern = vars; IEquation.expr = ce } in
  let update = (next_id, compile_atom e2) in
  ({ mem with Mem.fby = cnext_id :: mem.Mem.fby },
   { init with Init.fby = fby_node :: init.Init.fby },
   comp :: compi, update :: updatei)

(* Empty memory and initialization structures *)
let empty_mem =  { Mem.fby = []; node_mem = [] } 
let empty_init = { Init.fby = []; Init.node = [] }

(* Handle memory initialization *)
let handle_mem_init node_name mem_id = match Hashtbl.find_opt hashtable node_name with
  | Some mem -> if mem = empty_mem then [] else [mem_id, node_name]
  | None -> failwith "Unexpected node reference"

(* Compile application expressions *)
let compile_app (node_name, els) patt vars mem init compi update =
  let mem_id = gen_mem_id node_name in
  let node_mem = handle_mem_init node_name mem_id in
  let step_in = flatten_tuple els in
  let node_init = handle_mem_init node_name mem_id in
  let comp = { IEquation.pattern = vars; IEquation.expr = { IExpr.idesc = IE_app (node_name, mem_id, List.map compile_base_expr step_in); IExpr.itype = List.map (function { CExpr.ctype = [t]; _ } -> t | _ -> assert false) step_in; } } in
  ({ mem with Mem.node_mem = node_mem @ mem.Mem.node_mem },
   { init with Init.node = node_init @ init.Init.node },
   comp :: compi, update)

(* Compile reset expressions *)
let compile_reset (id, els, e) patt vars mem init compi update =
  let mem_id = gen_mem_id id in
  let node_mem = [mem_id, id] in
  let step_in = flatten_tuple els in
  let node_init = [mem_id, id] in
  let comp = { IEquation.pattern = vars; IEquation.expr = { IExpr.idesc = IE_reset (id, mem_id, List.map compile_base_expr step_in, compile_base_expr e); IExpr.itype = List.map (function { CExpr.ctype = [t]; _ } -> t | _ -> assert false) step_in; } } in
  ({ mem with Mem.node_mem = node_mem @ mem.Mem.node_mem },
    { init with Init.node = node_init @ init.Init.node },
    comp :: compi, update)
  
(* Compile individual equations *)
let compile_equation ({ CEquation.cpattern = patt; cexpr = e }) (mem, init, comp, update) =
  let vars = compile_pattern patt in
  match e.cdesc with
  | CE_fby (e1, e2) -> compile_fby (e1, e2) patt vars mem init comp update
  | CE_app (n, els) -> compile_app (n, els) patt vars mem init comp update
  | CE_reset (id, els, e) -> compile_reset (id, els, e) patt vars mem init comp update
  | _ -> let eq = { IEquation.pattern = vars; IEquation.expr = compile_base_expr e } in (mem, init, eq :: comp, update)

(* Compile all equations *)
let compile_equations eqs = List.fold_right compile_equation eqs (empty_mem, empty_init, [], [])

(* Compile a node *)
let compile_node { CNode.name; input; output; equs; local; _ } =
  let mem, init, compute, update = compile_equations equs in
  Hashtbl.add hashtable name mem;
  { INode.name; input_step = input; output_step = output; local; mem; init; compute; update; need_mem = mem <> empty_mem; }

(* Compile the entire file *)
let compile file =
  { IFile.nodes = List.map compile_node file.CFile.nodes; IFile.types = file.CFile.types }






















