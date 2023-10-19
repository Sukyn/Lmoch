open Lusb_ast
open Ident 
open Asttypes
open Typed_ast 
open Clocking

exception Invalid_behavior of string

let rec tr_expr_op = function 
    | Const n -> Const n 
    | Id x -> Id x 
    | Ope(o, l) -> (match o, List.map tr_expr_op l with 
    
        | Op_eq, [n; m]  -> (match n, m with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i = j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i = j))
                                    | Const (Cbool i), Const (Cbool j) -> Const (Cbool (i = j))
                                    | Const (Cbool true), e -> tr_expr_op e
                                    | e, Const (Cbool true) -> tr_expr_op e
                                    | Const (Cbool false), e -> tr_expr_op (Ope (Op_not, [m]))
                                    | e, Const (Cbool false) -> tr_expr_op (Ope (Op_not, [n]))
                                    | e, e' -> if e = e' then Const (Cbool true) 
                                               else if e = Ope(Op_not, [e']) || e' = Ope(Op_not, [e]) then Const (Cbool false)
                                               else Ope (o, [n; m])
                                    )              
        | Op_neq, [n; m] -> (match n, m with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i <> j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i <> j))
                                    | Const (Cbool i), Const (Cbool j) -> Const (Cbool (i <> j))
                                    | Const (Cbool false), e -> tr_expr_op e
                                    | e, Const (Cbool false) -> tr_expr_op e
                                    | Const (Cbool true), e -> tr_expr_op (Ope (Op_not, [m]))
                                    | e, Const (Cbool true) -> tr_expr_op (Ope (Op_not, [n]))
                                    | _ -> Ope (o, [n; m])
                                    )     
        | Op_lt, [n; m]  -> (match n, m with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i < j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i < j))
                                    | _ -> Ope (o, [n; m])
                                    ) 
        | Op_le, [n; m]  -> (match n, m with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i <= j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i <= j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_gt, [n; m]  -> (match n, m with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i > j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i > j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_ge, [n; m] -> (match n, m with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cbool (i >= j))
                                    | Const (Creal i), Const (Creal j) -> Const (Cbool (i >= j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_add, [n; m] -> (match n, m with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i+j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_sub, [n; m] -> (match n, m with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i-j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_mul, [n; m] -> (match n, m with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i*j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_div, [n; m] -> (match n, m with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i/j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_mod, [n; m] -> (match n, m with 
                                    | Const (Cint i), Const (Cint j) -> Const (Cint (i mod j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_add_f, [n; m] -> (match n, m with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i+.j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_sub_f, [n; m] -> (match n, m with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i-.j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_mul_f, [n; m] -> (match n, m with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i*.j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_div_f, [n; m] -> (match n, m with 
                                    | Const (Creal i), Const (Creal j) -> Const (Creal (i/.j))
                                    | _ -> Ope (o, [n; m])
                                    )
        | Op_not, [n] -> (match n with 
                                    | Const (Cbool i) -> Const (Cbool (not i))
                                    | Ope(Op_not, [e]) -> tr_expr_op e
                                    | Ope(Op_or, [n; m]) -> tr_expr_op (Ope(Op_and, [Ope(Op_not, [n]); Ope(Op_not, [m]);]))
                                    | Ope(Op_and, [n; m]) -> tr_expr_op (Ope(Op_or, [Ope(Op_not, [n]); Ope(Op_not, [m]);]))
                                    | _ -> Ope (o, [n])
                                    )
        | Op_and, [n; m] -> (match n, m with 
                                    | Const (Cbool i), Const (Cbool j) -> Const (Cbool (i && j))
                                    | Const (Cbool true), n -> n
                                    | Const (Cbool false), _ -> Const (Cbool false)
                                    | n, Const (Cbool true) -> n
                                    | _, Const (Cbool false) -> Const (Cbool false)
                                    | n', m' -> if n' = m' then n' 
                                                else if n' = Ope(Op_not, [m']) || m' = Ope(Op_not, [n']) then Const (Cbool false)
                                                else Ope (o, [n; m])
                                    )
        | Op_or, [n; m] -> (match n, m with 
                                    | Const (Cbool i), Const (Cbool j) -> Const (Cbool (i || j))
                                    | Const (Cbool false), n -> n
                                    | Const (Cbool true), _ -> Const (Cbool true)
                                    | n, Const (Cbool false) -> n
                                    | _, Const (Cbool true) -> Const (Cbool true)
                                    | n', m' -> if n' = m' then n' 
                                                else if n' = Ope(Op_not, [m']) || m' = Ope(Op_not, [n']) then Const (Cbool true)
                                                else Ope (o, [n; m])
                                    )
        | Op_impl, [n; m] -> (match n, m with 
                                    | Const (Cbool i), Const (Cbool j) -> Const (Cbool ((not i) || j))
                                    | Const (Cbool false), _ -> Const (Cbool true) 
                                    | Const (Cbool true), n -> n
                                    | n, Const (Cbool false) -> Ope(Op_not, [n])
                                    | _, Const (Cbool true) -> Const (Cbool true)
                                    | n', m' -> if n' = m' then Const (Cbool true)
                                                            else tr_expr_op (Ope(Op_or, [Ope (Op_not, [n]); m]))
                                    )
        | Op_if, [b; n; m] -> (match b with 
                                    | Const (Cbool true) -> n
                                    | Const (Cbool false) -> m
                                    | b' -> (match n, m with 
                                            | Const (Cbool false), Const (Cbool false) -> Const (Cbool false)
                                            | Const (Cbool false), Const (Cbool true) -> tr_expr_op (Ope (Op_not, [b]))
                                            | Const (Cbool true), Const (Cbool true) -> Const (Cbool true)
                                            | Const (Cbool true), Const (Cbool false) -> b'
                                            | Const (Cbool false), e -> if e = b' then Const (Cbool false)
                                                                        else tr_expr_op (Ope(Op_and, [Ope (Op_not, [b]); m]))
                                            | e, Const (Cbool true) -> if e = b' then Const (Cbool true)
                                                                       else tr_expr_op (Ope(Op_or, [Ope (Op_not, [b]); n]))
                                            | Const (Cbool true), e -> if e = b' then e
                                                                       else tr_expr_op (Ope(Op_or, [b; m]))
                                            | e, Const (Cbool false) -> if e = b' then e
                                                                        else tr_expr_op (Ope(Op_and, [b; n]))
                                            | _ -> Ope (o, [n; m])
                                    )
                                    )
            | _, _ -> raise (Invalid_behavior "Operation not found")
        )
        | _ -> raise (Invalid_behavior "Operation not found")

and tr_expr = function
  | Typed_ast.TE_const n -> Const n 
  | Typed_ast.TE_ident x -> Id x
  | Typed_ast.TE_op (o, l) -> tr_expr_op (Ope(o, (List.map (fun v -> tr_expr v.texpr_desc) l)))
  | Typed_ast.TE_merge (x, e, e') -> Merge(x, tr_expr e.texpr_desc, tr_expr e'.texpr_desc)
  | Typed_ast.TE_app (x, l) -> App (x, List.map (fun v -> tr_expr v.texpr_desc) l)
  | Typed_ast.TE_prim (x, l) -> Prim (x, List.map (fun v -> tr_expr v.texpr_desc) l)
  | Typed_ast.TE_arrow (e, e') -> (match tr_expr e.texpr_desc, tr_expr e'.texpr_desc with 
                                  | (n, m) when n = m -> n
                                  | eb, eb' -> tr_expr_op (Ope(Op_if, [Fby(Const (Cbool true), Const (Cbool false)); eb; eb'])))
  | Typed_ast.TE_fby (e, e') -> (match tr_expr e.texpr_desc, tr_expr e'.texpr_desc with 
                                  | (n, m) when n = m -> n
                                  | eb, eb' -> Fby(eb, eb')
                                )
  | Typed_ast.TE_when (e, x) -> When(tr_expr e.texpr_desc, x)
  | Typed_ast.TE_whenot (e, x) -> Whenot(tr_expr e.texpr_desc, x)
  | Typed_ast.TE_pre e -> (match tr_expr e.texpr_desc with 
                          | Const n -> Const n
                          | eb ->  Fby(Nil, eb))
  | Typed_ast.TE_tuple l -> Tuple (List.map (fun v -> tr_expr v.texpr_desc) l)
  

and tr_ex eq = 
    let l = eq.texpr_loc in 
    let vd = tr_expr eq.texpr_desc in
    { ttexpr_desc = vd;
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