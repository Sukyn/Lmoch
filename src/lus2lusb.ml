open Typed_ast
open Ident 

module M = Map.Make(String)

let rec tr_expr env = function
  | TE_const n -> (match n with  
                 | Cbool true -> TE_const (Cint 1)
                 | Cbool false -> TE_const (Cint 0)
                 | n -> TE_const n 
                 )     
  | TE_ident x -> (* let value, is_cst = M.find x.name !env in 
                  if is_cst then value 
                  else TE_ident x *)
                  TE_ident x
  | TE_op (o, l) -> 
        (match o with 
        | Op_eq     -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cbool (i = j)))
                                    | TE_const (Creal i), TE_const (Creal j) -> tr_expr env (TE_const (Cbool (i = j)))
                                    | _ -> TE_op (o, l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )               
        | Op_neq    ->  (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cbool (i != j)))
                                    | TE_const (Creal i), TE_const (Creal j) -> tr_expr env (TE_const (Cbool (i != j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_lt     -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cbool (i < j)))
                                    | TE_const (Creal i), TE_const (Creal j) -> tr_expr env (TE_const (Cbool (i < j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_le     -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cbool (i <= j)))
                                    | TE_const (Creal i), TE_const (Creal j) -> tr_expr env (TE_const (Cbool (i <= j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_gt     -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cbool (i > j)))
                                    | TE_const (Creal i), TE_const (Creal j) -> tr_expr env (TE_const (Cbool (i > j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_ge     -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cbool (i >= j)))
                                    | TE_const (Creal i), TE_const (Creal j) -> tr_expr env (TE_const (Cbool (i >= j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_add    -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cint (i + j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_sub    -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cint (i - j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_mul    -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cint (i * j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_div    -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cint (i / j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_mod    -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cint (i mod j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_add_f  -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Creal i), TE_const (Creal j) -> tr_expr env (TE_const (Creal (i +. j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_sub_f  -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Creal i), TE_const (Creal j) -> tr_expr env (TE_const (Creal (i -. j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_mul_f  -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Creal i), TE_const (Creal j) -> tr_expr env (TE_const (Creal (i *. j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_div_f  -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Creal i), TE_const (Creal j) -> tr_expr env (TE_const (Creal (i /. j)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_not    -> (match l with 
                        | [n] -> (match tr_expr env n.texpr_desc with 
                                    | TE_const (Cint i) -> tr_expr env (TE_const (Cbool (i = 0)))
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        ) 
        | Op_and    -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cint (i * j)))
                                    | TE_const (Cint 1), n -> n
                                    | TE_const (Cint 0), _ -> TE_const (Cint 0)
                                    | n, TE_const (Cint 1) -> n
                                    | _, TE_const (Cint 0) -> TE_const (Cint 0)
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )   
        | Op_or     -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cbool (i + j > 0)))
                                    | TE_const (Cint 0), n -> n
                                    | TE_const (Cint 1), _ -> TE_const (Cint 1)
                                    | n, TE_const (Cint 0) -> n
                                    | _, TE_const (Cint 1) -> TE_const (Cint 1)
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )
        | Op_impl   -> (match l with 
                        | [n; m] -> (match tr_expr env n.texpr_desc, tr_expr env m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> tr_expr env (TE_const (Cbool (i > j)))
                                    | TE_const (Cint 0), _ -> TE_const (Cint 1) 
                                    | TE_const (Cint 1), n -> n
                                    | n, TE_const (Cint 0) -> n
                                    | _, TE_const (Cint 1) -> TE_const (Cint 1)
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )
        | Op_if     -> (match l with 
                        | [b; n; m] -> (match tr_expr env b.texpr_desc with 
                                    | TE_const (Cint 1) -> tr_expr env n.texpr_desc
                                    | TE_const (Cint 0) -> tr_expr env m.texpr_desc
                                    | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                                    )
                        | _ -> TE_op (o, List.map (fun v -> {v with texpr_desc = tr_expr env v.texpr_desc}) l)
                        )
        )
  | TE_app (x, l) -> TE_app (x, l)
  | TE_prim (x, l) -> TE_prim (x, l)
  | TE_arrow (e, e') -> TE_arrow (e, e')
  | TE_pre e -> TE_pre e
  | TE_tuple l -> TE_tuple l

let tr_eq eq env =
    let patt = eq.teq_patt in
    let expr = eq.teq_expr in 
    let vd = tr_expr env expr.texpr_desc in
    (* env := List.fold_left (fun e v -> M.add v.name (vd, false) e) !env patt.tpatt_desc; *)
    { eq with teq_expr = {expr with texpr_desc = vd}}


let tr_node n = 

    let env = ref M.empty in

    let name = n.tn_name in
    let input = n.tn_input in
    let output = n.tn_output in
    (*
    env := List.fold_left (fun e (v, _) -> M.add v.name ((TE_ident v), false) e) !env input;
    let equs = List.filter (fun v -> List.compare_length_with (List.filter (fun h -> List.mem h (List.map (fun (x, _) -> x) output)) v.teq_patt.tpatt_desc) 0 <> 0)
                            (List.map (fun eq -> tr_eq eq env) n.tn_equs) in
    *)
    let equs = List.map (fun eq -> tr_eq eq env) n.tn_equs in
    let loc = n.tn_loc in
    let node = 
    { tn_name = name;
      tn_input = input;
      tn_output = output;
      tn_local = [];
      tn_equs = equs;
      tn_loc = loc; }
    in 
    node

let tr_program prog main = 
    let lusb = List.map tr_node prog in 
    lusb 