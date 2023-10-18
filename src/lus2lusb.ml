open Lusb_ast
open Ident 
open Asttypes
open Typed_ast 
open Clocking

exception Invalid_behavior of string

let rec tr_expr_op loc o l = match o, l with 
        | Op_eq, [n; m]  -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i = j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i = j))
                                    | Const (Cbool i), Const (Cbool j) -> Const (Cbool (i = j))
                                    | Const (Cbool true), e -> e
                                    | e, Const (Cbool true) -> e
                                    | Const (Cbool false), e -> Ope (Op_not, [tr_ex m])
                                    | e, Const (Cbool false) -> Ope (Op_not, [tr_ex n])
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )              
        | Op_neq, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i <> j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i <> j))
                                    | Const (Cbool i), Const (Cbool j) -> Const (Cbool (i <> j))
                                    | Const (Cbool false), e -> e
                                    | e, Const (Cbool false) -> e
                                    | Const (Cbool true), e -> Ope (Op_not, [tr_ex m])
                                    | e, Const (Cbool true) -> Ope (Op_not, [tr_ex n])
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )     
        | Op_lt, [n; m]  -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i < j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i < j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    ) 
        | Op_le, [n; m]  -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i <= j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i <= j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_gt, [n; m]  -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i > j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i > j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_ge, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i >= j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i >= j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_add, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i+j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_sub, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i-j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_mul, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i*j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_div, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i/j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_mod, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i mod j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_add_f, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i+.j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_sub_f, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i-.j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_mul_f, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i*.j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_div_f, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i/.j))
                                    | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_not, [n] -> (match tr_expr loc n.texpr_desc with 
                                    | Const (Cbool i) -> Const (Cbool (not i))
                                    | Ope(Op_not, [{ texpr_desc = Const (Cbool i);
                                                    texpr_type = _;
                                                    texpr_loc  = _c; }]) -> Const (Cbool (not i))
                                    | _ -> Ope (o, [tr_ex n])
                                    )
        | Op_and, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cbool i), Const (Cbool j) -> Const (Cbool (i && j))
                                    | Const (Cbool true), n -> n
                                    | Const (Cbool false), _ -> Const (Cbool false)
                                    | n, Const (Cbool true) -> n
                                    | _, Const (Cbool false) -> Const (Cbool false)
                                    | n', m' -> if n' = m' then n' else Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_or, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cbool i), Const (Cbool j) -> Const (Cbool (i || j))
                                    | Const (Cbool false), n -> n
                                    | Const (Cbool true), _ -> Const (Cbool true)
                                    | n, Const (Cbool false) -> n
                                    | _, Const (Cbool true) -> Const (Cbool true)
                                    | n', m' -> if n' = m' then n' else Ope (o, [tr_ex n; tr_ex m])
                                    )
        | Op_impl, [n; m] -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                    | Const (Cbool i), Const (Cbool j) -> Const (Cbool ((not i) || j))
                                    | Const (Cbool false), _ -> Const (Cbool true) 
                                    | Const (Cbool true), n -> n
                                    | n, Const (Cbool false) -> Ope(Op_not, [{ texpr_desc = n;
                                                                    texpr_type = [Tbool];
                                                                    texpr_loc  = loc; }])
                                    | _, Const (Cbool true) -> Const (Cbool true)
                                    | n', m' -> if n' = m' then Const (Cbool true)
                                                            else Ope(Op_or, [{ texpr_desc = Ope (Op_not, [tr_ex n]);
                                                                    texpr_type = [Tbool];
                                                                    texpr_loc  = loc; }; tr_ex m])
                                    )
        | Op_if, [b; n; m] -> (match tr_expr loc b.texpr_desc with 
                                    | Const (Cbool true) -> tr_expr loc n.texpr_desc
                                    | Const (Cbool false) -> tr_expr loc m.texpr_desc
                                    | b' -> (match tr_expr loc n.texpr_desc, tr_expr loc m.texpr_desc with 
                                            | Const (Cbool false), Const (Cbool false) -> Const (Cbool false)
                                            | Const (Cbool false), Const (Cbool true) -> Ope (Op_not, [tr_ex b])
                                            | Const (Cbool true), Const (Cbool true) -> Const (Cbool true)
                                            | Const (Cbool true), Const (Cbool false) -> b'
                                            | Const (Cbool false), e -> if e = b' then Const (Cbool false)
                                                                        else Ope(Op_and, [{ texpr_desc = Ope (Op_not, [tr_ex b]);
                                                                                       texpr_type = [Tbool];
                                                                                       texpr_loc  = loc; }; tr_ex m])
                                            | e, Const (Cbool true) -> if e = b' then Const (Cbool true)
                                                                       else Ope(Op_or, [{ texpr_desc = Ope (Op_not, [tr_ex b]);
                                                                                    texpr_type = [Tbool];
                                                                                    texpr_loc  = loc; }; tr_ex n])
                                            | Const (Cbool true), e -> if e = b' then e
                                                                       else Ope(Op_or, [tr_ex b; tr_ex m])
                                            | e, Const (Cbool false) -> if e = b' then e
                                                                        else Ope(Op_and, [tr_ex b; tr_ex n])
                                            | _ -> Ope (o, [tr_ex n; tr_ex m])
                                    )
                                    )
        | _ -> raise (Invalid_behavior "Operation not found")

and tr_expr loc = function
  | Typed_ast.TE_const n -> Const n 
  | Typed_ast.TE_ident x -> Id x
  | Typed_ast.TE_op (o, l) -> tr_expr_op loc o l
  | Typed_ast.TE_app (x, l) -> App (x, List.map (fun v -> tr_ex v) l)
  | Typed_ast.TE_prim (x, l) -> Prim (x, List.map (fun v -> tr_ex v) l)
  | Typed_ast.TE_arrow (e, e') -> (match tr_expr loc e.texpr_desc, tr_expr loc e'.texpr_desc with 
                                  | (n, m) when n = m -> n
                                  | _ -> 
                                  Ope(Op_if, [{ texpr_desc = Fby({ texpr_desc = Const (Cbool true);
                                                                   texpr_type = [Tbool];
                                                                   texpr_loc  = loc; }
                                                                 ,
                                                                  { texpr_desc = Const (Cbool false);
                                                                   texpr_type = [Tbool];
                                                                   texpr_loc  = loc;});
                                                texpr_type = [Tbool];
                                                texpr_loc  = loc; }; tr_ex e; tr_ex e']))
  | Typed_ast.TE_fby (e, e') -> (match tr_expr loc e.texpr_desc, tr_expr loc e'.texpr_desc with 
                                  | (n, m) when n = m -> n
                                  | _ -> Fby(tr_ex e, tr_ex e')
                                )
  | Typed_ast.TE_when (e, x) -> When(tr_ex e, x)
  | Typed_ast.TE_whenot (e, x) -> Whenot(tr_ex e, x)
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

    let name = n.cn_name in
    let input = n.cn_input in
    let output = n.cn_output in
    let local = n.cn_local in 
    let equs = List.map (fun eq -> tr_eq eq) n.cn_equs in
    let loc = n.cn_loc in
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