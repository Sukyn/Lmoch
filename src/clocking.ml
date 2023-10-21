open Typed_ast 

type ct = 
  | Sing of ck 
  | Mul of ct list 

and ck =
  | Cbase 
  | Ccon of clock_guard

and clock_guard = 
  | True of Ident.t 
  | False of Ident.t

type alternate = 
  | T
  | F

type clocked_var = Ident.t * Asttypes.base_ty * ct

type error =
  | Invalid_clock

exception Error of error

let error e = raise (Error (e))

type c_node =
    { cn_name: Ident.t;
      cn_input: clocked_var list;
      cn_output: clocked_var list;
      cn_local: clocked_var list;
      cn_equs: t_equation list;
      cn_loc: Asttypes.location; }

module C = Map.Make(Ident)

module Kappa = struct

    type t = (Ident.t * ck) C.t

    let empty = C.empty

    let add x clock env =
        C.add x clock env

    let find x env = 
        C.find x env
end

let rec clock_eq env k = (match k.texpr_desc with
  | TE_const c -> Sing(Cbase)
  | TE_ident x -> Kappa.find x env
  | TE_op (o, l) -> if List.for_all (fun x -> clock_eq env x = clock_eq env (List.hd l)) (List.tl l) then 
                    clock_eq env (List.hd l)
                    else error (Invalid_clock)
  | TE_app (f,args) -> if List.for_all (fun x -> clock_eq env x = clock_eq env (List.hd args)) (List.tl args) then 
                        clock_eq env (List.hd args)
                        else error (Invalid_clock)
  | TE_prim (f,args) -> if List.for_all (fun x -> clock_eq env x = clock_eq env (List.hd args)) (List.tl args) then 
                        clock_eq env (List.hd args)
                        else error (Invalid_clock)
  | TE_arrow (e, e') -> clock_eq env e'
  | TE_fby (e, e') -> clock_eq env e'
  | TE_when (e, x) -> if clock_eq env e = Kappa.find x env then Sing(Ccon (True(x)))
                        else error (Invalid_clock)
  | TE_whenot (e, x) -> if clock_eq env e = Kappa.find x env then Sing(Ccon (False(x)))
                        else error (Invalid_clock)
  | TE_merge (x, e1, e2) -> if clock_eq env e1 = Sing(Ccon (True(x))) && clock_eq env e2 = Sing(Ccon (False(x)))
                            then Kappa.find x env
                            else
                            (if clock_eq env e1 = Sing(Ccon (False(x))) && clock_eq env e2 = Sing(Ccon (True(x)))
                            then Kappa.find x env
                            else error (Invalid_clock))
  | TE_pre e -> clock_eq env e
  | TE_tuple l -> if List.for_all (fun x -> clock_eq env x = clock_eq env (List.hd l)) (List.tl l) then 
                    clock_eq env (List.hd l)
                    else error (Invalid_clock)
)

let clock_equation env eq = 
    let vars = eq.teq_patt in 
    let expr = eq.teq_expr in
    let clock = clock_eq env expr in
    List.fold_left (fun env x -> Kappa.add x clock env) env vars.tpatt_desc

let clock_node n =
  let env = List.fold_left (fun env x -> Kappa.add x (Sing(Cbase)) env) Kappa.empty (List.map (fun (u, _) -> u) (n.tn_input)) in 
  let env = List.fold_left (fun acc v -> clock_equation acc v) env n.tn_equs in

  let node =
    { cn_name = n.tn_name;
      cn_input = List.map (fun (n, ty) -> (n, ty, Kappa.find n env)) n.tn_input;
      cn_output = List.map (fun (n, ty) -> (n, ty, Kappa.find n env)) n.tn_output;
      cn_local = List.map (fun (n, ty) -> (n, ty, Kappa.find n env)) n.tn_local;
      cn_equs = n.tn_equs;
      cn_loc = n.tn_loc; }
  in
  node




let clock_file f main =
  let ft = List.map clock_node f in
  ft