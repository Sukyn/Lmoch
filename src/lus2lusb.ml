open Typed_ast 
open Asttypes

exception Invalid_behavior of string

let nil loc = function
  | Tbool -> { texpr_desc= TE_const (Cbool false);
               texpr_type=  [Tbool];
               texpr_loc= loc; }
  | Tint -> { texpr_desc= TE_const (Cint 0);
              texpr_type=  [Tint];
              texpr_loc= loc; }
  | Treal -> { texpr_desc= TE_const (Creal 0.0);
              texpr_type=  [Treal];
              texpr_loc= loc; }

let rec tr_expr_op = function 
    | TE_const n -> TE_const n 
    | TE_ident x -> TE_ident x 
    | TE_op(o, l) -> (match o, l with 
    
        | Op_eq, [n; m]  -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> TE_const (Cbool (i = j))
                                    | TE_const (Creal i), TE_const (Creal j) -> TE_const (Cbool (i = j))
                                    | TE_const (Cbool i), TE_const (Cbool j) -> TE_const (Cbool (i = j))
                                    | TE_const (Cbool true), e -> tr_expr_op e
                                    | e, TE_const (Cbool true) -> tr_expr_op e
                                    | TE_const (Cbool false), e -> tr_expr_op (TE_op (Op_not, [{m with texpr_desc = e}]))
                                    | e, TE_const (Cbool false) -> tr_expr_op (TE_op (Op_not, [{n with texpr_desc = e}]))
                                    | e, e' -> if e = e' then TE_const (Cbool true) 
                                               else if e = TE_op(Op_not, [{m with texpr_desc = e'}]) || e' = TE_op(Op_not, [{n with texpr_desc = e}]) then TE_const (Cbool false)
                                               else TE_op (o, [{n with texpr_desc = e}; {m with texpr_desc = e'}])
                            )              
        | Op_neq, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> TE_const (Cbool (i <> j))
                                    | TE_const (Creal i), TE_const (Creal j) -> TE_const (Cbool (i <> j))
                                    | TE_const (Cbool i), TE_const (Cbool j) -> TE_const (Cbool (i <> j))
                                    | TE_const (Cbool false), e -> tr_expr_op e
                                    | e, TE_const (Cbool false) -> tr_expr_op e
                                    | TE_const (Cbool true), e -> tr_expr_op (TE_op (Op_not, [{m with texpr_desc = e}]))
                                    | e, TE_const (Cbool true) -> tr_expr_op (TE_op (Op_not, [{n with texpr_desc = e}]))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            )     
        | Op_lt, [n; m]  -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> TE_const (Cbool (i < j))
                                    | TE_const (Creal i), TE_const (Creal j) -> TE_const (Cbool (i < j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            ) 
        | Op_le, [n; m]  -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> TE_const (Cbool (i <= j))
                                    | TE_const (Creal i), TE_const (Creal j) -> TE_const (Cbool (i <= j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            )
        | Op_gt, [n; m]  -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> TE_const (Cbool (i > j))
                                    | TE_const (Creal i), TE_const (Creal j) -> TE_const (Cbool (i > j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            )
        | Op_ge, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> TE_const (Cbool (i >= j))
                                    | TE_const (Creal i), TE_const (Creal j) -> TE_const (Cbool (i >= j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            )
        | Op_add, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> TE_const (Cint (i+j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            )
        | Op_sub, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> TE_const (Cint (i-j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            )
        | Op_mul, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> TE_const (Cint (i*j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            )
        | Op_div, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> TE_const (Cint (i/j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            )
        | Op_mod, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cint i), TE_const (Cint j) -> TE_const (Cint (i mod j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            )
        | Op_add_f, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Creal i), TE_const (Creal j) -> TE_const (Creal (i+.j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                              )
        | Op_sub_f, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Creal i), TE_const (Creal j) -> TE_const (Creal (i-.j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                              )
        | Op_mul_f, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Creal i), TE_const (Creal j) -> TE_const (Creal (i*.j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                              )
        | Op_div_f, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Creal i), TE_const (Creal j) -> TE_const (Creal (i/.j))
                                    | n', m' -> TE_op (o, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                              )
        | Op_not, [n] -> (match n.texpr_desc with 
                                    | TE_const (Cbool i) -> TE_const (Cbool (not i))
                                    | TE_op(Op_not, [e]) -> tr_expr_op e.texpr_desc
                                    | TE_op(Op_or, [n; m]) -> tr_expr_op (TE_op(Op_and, [{ n with texpr_desc = tr_expr_op (TE_op(Op_not, [n]))}; { m with texpr_desc = tr_expr_op (TE_op(Op_not, [m]))};]))
                                    | TE_op(Op_and, [n; m]) -> tr_expr_op (TE_op(Op_or, [{ n with texpr_desc = tr_expr_op (TE_op(Op_not, [n]))}; { m with texpr_desc = tr_expr_op (TE_op(Op_not, [m]))};]))
                                    | TE_fby(n', m') -> tr_expr (TE_fby({n' with texpr_desc = TE_op(Op_not, [n'])}, {m' with texpr_desc = TE_op(Op_not, [m'])}))
                                    | n' -> TE_op (Op_not, [{n with texpr_desc = n'}])
                          )
        | Op_and, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cbool i), TE_const (Cbool j) -> TE_const (Cbool (i && j))
                                    | TE_const (Cbool true), n -> n
                                    | TE_const (Cbool false), _ -> TE_const (Cbool false)
                                    | n, TE_const (Cbool true) -> n
                                    | _, TE_const (Cbool false) -> TE_const (Cbool false)
                                    | n', m' -> if n' = m' then n' 
                                                else if n' = TE_op(Op_not, [{m with texpr_desc = m'}]) || m' = TE_op(Op_not, [{n with texpr_desc = n'}]) then TE_const (Cbool false)
                                                else TE_op (Op_and, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            )
        | Op_or, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cbool i), TE_const (Cbool j) -> TE_const (Cbool (i || j))
                                    | TE_const (Cbool false), n -> n
                                    | TE_const (Cbool true), _ -> TE_const (Cbool true)
                                    | n, TE_const (Cbool false) -> n
                                    | _, TE_const (Cbool true) -> TE_const (Cbool true)
                                    | n', m' -> if n' = m' then n' 
                                                else if n' = TE_op(Op_not, [{m with texpr_desc = m'}]) || m' = TE_op(Op_not, [{n with texpr_desc = n'}]) then TE_const (Cbool true)
                                                else TE_op (Op_or, [{n with texpr_desc = n'}; {m with texpr_desc = m'}])
                            )
        | Op_impl, [n; m] -> (match n.texpr_desc, m.texpr_desc with 
                                    | TE_const (Cbool i), TE_const (Cbool j) -> TE_const (Cbool ((not i) || j))
                                    | TE_const (Cbool false), _ -> TE_const (Cbool true) 
                                    | TE_const (Cbool true), n -> n
                                    | n', TE_const (Cbool false) -> TE_op(Op_not, [{n with texpr_desc = n'}])
                                    | _, TE_const (Cbool true) -> TE_const (Cbool true)
                                    | n', m' -> if n' = m' then TE_const (Cbool true)
                                                            else tr_expr_op (TE_op(Op_or, [{ n with texpr_desc = tr_expr_op (TE_op(Op_not, [{n with texpr_desc = n'}]))}; {m with texpr_desc = m'}]))
                              )
        | Op_if, [b; n; m] -> (match b.texpr_desc with 
                                    | TE_const (Cbool true) -> n.texpr_desc
                                    | TE_const (Cbool false) -> m.texpr_desc
                                    | b' -> (match n.texpr_desc, m.texpr_desc with 
                                            | TE_const (Cbool false), TE_const (Cbool false) -> TE_const (Cbool false)
                                            | TE_const (Cbool false), TE_const (Cbool true) -> tr_expr_op (TE_op (Op_not, [{b with texpr_desc = b'}]))
                                            | TE_const (Cbool true), TE_const (Cbool true) -> TE_const (Cbool true)
                                            | TE_const (Cbool true), TE_const (Cbool false) -> b'
                                            | TE_const (Cbool false), e -> if e = b' then TE_const (Cbool false)
                                                                        else tr_expr_op (TE_op(Op_and, [{ n with texpr_desc = tr_expr_op (TE_op(Op_not, [b]))}; {m with texpr_desc = e}]))
                                            | e, TE_const (Cbool true) -> if e = b' then TE_const (Cbool true)
                                                                       else tr_expr_op (TE_op(Op_or, [{ n with texpr_desc = tr_expr_op (TE_op(Op_not, [b]))}; {n with texpr_desc = e}]))
                                            | TE_const (Cbool true), e -> if e = b' then e
                                                                       else tr_expr_op (TE_op(Op_or, [{b with texpr_desc = b'}; {m with texpr_desc = e}]))
                                            | e, TE_const (Cbool false) -> if e = b' then e
                                                                        else tr_expr_op (TE_op(Op_and, [{b with texpr_desc = b'}; {n with texpr_desc = e}]))
                                            | e, e' -> TE_op (o, [{b with texpr_desc = b'}; {n with texpr_desc = e}; {m with texpr_desc = e'}])
                                            )
                              )
          | x, y -> raise (Invalid_behavior "TE_operation not found")
        )
        | x -> raise (Invalid_behavior "TE_operation not found")

and tr_expr = function
  | Typed_ast.TE_const n -> TE_const n 
  | Typed_ast.TE_ident x -> TE_ident x
  | Typed_ast.TE_op (o, l) -> tr_expr_op (TE_op(o, l))
  | Typed_ast.TE_merge (x, l) -> TE_merge(x, List.map (fun (e, v) -> ({e with texpr_desc = tr_expr e.texpr_desc}, {v with texpr_desc = tr_expr v.texpr_desc})) l)
  | Typed_ast.TE_app (x, l) -> TE_app (x, List.map (fun v -> {v with texpr_desc = tr_expr v.texpr_desc}) l)
  | Typed_ast.TE_prim (x, l) -> TE_prim (x, List.map (fun v -> {v with texpr_desc = tr_expr v.texpr_desc}) l)
  | Typed_ast.TE_arrow (e, e') -> (match tr_expr e.texpr_desc, tr_expr e'.texpr_desc with 
                                  | (n, m) when n = m -> n
                                  | eb, eb' -> tr_expr_op (TE_op(Op_if, [
                                    { texpr_desc=
                                       TE_fby(
                                      { texpr_desc= TE_const (Cbool true);
                                        texpr_type=  [Tbool];
                                        texpr_loc= e.texpr_loc; },
                                      { texpr_desc= TE_const (Cbool false);
                                        texpr_type=  [Tbool];
                                        texpr_loc= e.texpr_loc; });
                                        texpr_type=  [Tbool];
                                        texpr_loc= e.texpr_loc; }
                                        ; {e with texpr_desc = eb}; {e' with texpr_desc = eb'}])))
  | Typed_ast.TE_fby (e, e') -> (match tr_expr e.texpr_desc, tr_expr e'.texpr_desc with 
                                  | (n, m) when n = m -> n
                                  | eb, eb' -> TE_fby({e with texpr_desc = eb}, {e' with texpr_desc = eb'})
                                )
  | Typed_ast.TE_when (e, x, e') -> TE_when({e with texpr_desc = tr_expr e.texpr_desc}, x, {e' with texpr_desc = tr_expr e'.texpr_desc})
  | Typed_ast.TE_pre e -> (match tr_expr e.texpr_desc with 
                          | TE_const n -> TE_const n
                          | eb ->  TE_fby(nil e.texpr_loc (List.hd e.texpr_type), {e with texpr_desc = eb}))
  | Typed_ast.TE_tuple l -> TE_tuple (List.map (fun v -> {v with texpr_desc = tr_expr v.texpr_desc}) l)
  

and tr_ex eq = 
    { eq with texpr_desc = tr_expr eq.texpr_desc;
    }

let tr_eq eq =
    { eq with teq_expr = tr_ex eq.teq_expr; 
    }


let tr_node n = 
    { n with 
      tn_equs = List.map (fun eq -> tr_eq eq) n.tn_equs;
     }

let tr_program prog main = 
    let lusb = List.map tr_node prog in 
    lusb 
    
let file f =
  { f with
      t_nodes = List.map tr_node f.t_nodes }