open Lusb_ast
open Ident 
open Asttypes
open Typed_ast 

exception Invalid_behavior of string

let rec tr_expr loc = function
  | Typed_ast.TE_const n -> (match n with  
                 | Cbool true -> Const (Cint 1)
                 | Cbool false -> Const (Cint 0)
                 | n -> Const n 
                 )     
  | Typed_ast.TE_ident x -> Id x
  | Typed_ast.TE_op (o, l) -> 
        (match o, l with 
        | Op_eq, [n; m]  -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> if i = j then Const (Cint 1) else Const (Cint 0)
                                    | Const (Creal i), Const (Creal j) -> if i = j then Const (Cint 1) else Const (Cint 0)
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )              
        | Op_neq, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> if i <> j then Const (Cint 1) else Const (Cint 0)
                                    | Const (Creal i), Const (Creal j) -> if i <> j then Const (Cint 1) else Const (Cint 0)
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )  
        | Op_lt, [n; m]  -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> if i < j then Const (Cint 1) else Const (Cint 0)
                                    | Const (Creal i), Const (Creal j) -> if i < j then Const (Cint 1) else Const (Cint 0)
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    ) 
        | Op_le, [n; m]  -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> if i <= j then Const (Cint 1) else Const (Cint 0)
                                    | Const (Creal i), Const (Creal j) -> if i <= j then Const (Cint 1) else Const (Cint 0)
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_gt, [n; m]  -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> if i > j then Const (Cint 1) else Const (Cint 0)
                                    | Const (Creal i), Const (Creal j) -> if i > j then Const (Cint 1) else Const (Cint 0)
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_ge, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> if i >= j then Const (Cint 1) else Const (Cint 0)
                                    | Const (Creal i), Const (Creal j) -> if i >= j then Const (Cint 1) else Const (Cint 0)
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_add, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i+j))
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_sub, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i-j))
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_mul, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i*j))
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_div, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i/j))
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_mod, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i mod j))
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_add_f, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i+.j))
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_sub_f, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i-.j))
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_mul_f, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i*.j))
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_div_f, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i/.j))
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_not, [n] -> (match tr_expr loc n.texpr_desc with 
                                    | Const (Cint i) -> Const (Cint ((i+1) mod 2))
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_and, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i * j))
                                    | Const (Cint 1), n -> n
                                    | Const (Cint 0), _ -> Const (Cint 0)
                                    | n, Const (Cint 1) -> n
                                    | _, Const (Cint 0) -> Const (Cint 0)
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_or, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> if i + j > 0 then Const (Cint 1) else Const (Cint 0) 
                                    | Const (Cint 0), n -> n
                                    | Const (Cint 1), _ -> Const (Cint 1)
                                    | n, Const (Cint 0) -> n
                                    | _, Const (Cint 1) -> Const (Cint 1)
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_impl, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> if i > j then Const (Cint 1) else Const (Cint 0) 
                                    | Const (Cint 0), _ -> Const (Cint 1) 
                                    | Const (Cint 1), n -> n
                                    | n, Const (Cint 0) -> n
                                    | _, Const (Cint 1) -> Const (Cint 1)
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | Op_if, [b; n; m] -> (match tr_expr loc b.texpr_desc with 
                                    | Const (Cint 1) -> tr_expr loc n.texpr_desc
                                    | Const (Cint 0) -> tr_expr loc m.texpr_desc
                                    | _ -> Ope (o, List.map (fun v -> tr_ex v) l)
                                    )
        | _, _ -> raise (Invalid_behavior "Operation not found")
        )
  | Typed_ast.TE_app (x, l) -> App (x, List.map (fun v -> tr_ex v) l)
  | Typed_ast.TE_prim (x, l) -> Prim (x, List.map (fun v -> tr_ex v) l)
  | Typed_ast.TE_arrow (e, e') -> (match tr_expr loc e.texpr_desc, tr_expr loc e'.texpr_desc with 
                                  | (n, m) when n = m -> n
                                  | _ -> 
                                  Ope(Op_if, [{ texpr_desc = Fby({ texpr_desc = Const (Cint 1);
                                                                   texpr_type = [Tbool];
                                                                   texpr_loc  = loc; }
                                                                 ,
                                                                  { texpr_desc = Const (Cint 0);
                                                                   texpr_type = [Tbool];
                                                                   texpr_loc  = loc;});
                                                texpr_type = [Tbool];
                                                texpr_loc  = loc; }; tr_ex e; tr_ex e']))
  | Typed_ast.TE_pre e -> (match tr_expr loc e.texpr_desc with 
                          | Const n -> Const n
                          | _ ->  Fby({ texpr_desc = Nil;
                                texpr_type = e.texpr_type;
                                texpr_loc  = loc; }
                                ,
                                tr_ex e))
  | Typed_ast.TE_tuple l -> Tuple (List.map (fun v -> tr_ex v) l)

and tr_ex eq = 
    let l = eq.texpr_loc in 
    let vd = tr_expr l eq.texpr_desc in
    { texpr_desc = vd;
      texpr_type = eq.texpr_type;
      texpr_loc = l;}

let tr_eq eq =
    let expr = eq.teq_expr in 
    let t = eq.teq_patt in
    let p = 
    { patt_desc = t.tpatt_desc;
      patt_type = t.tpatt_type;
      patt_loc = t.tpatt_loc; } in

    { eq_expr = tr_ex expr; 
      eq_patt = p;}


let tr_node n = 

    let name = n.tn_name in
    let input = n.tn_input in
    let output = n.tn_output in
    let local = n.tn_local in 
    let equs = List.map (fun eq -> tr_eq eq) n.tn_equs in
    let loc = n.tn_loc in
    let node = 
    { t_name = name;
      t_input = input;
      t_output = output;
      t_local = local;
      t_equs = equs;
      t_loc = loc; }
    in 
    node

let tr_program prog main = 
    let lusb = List.map tr_node prog in 
    lusb 