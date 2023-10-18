open Asttypes
open Ident 

type t_expr_l =
    { texpr_desc: t_expr_desc_l;
      texpr_type:  ty;
      texpr_loc: location; }

and t_expr_desc_l =
  | Nil
  | Const of const
  | Id of Ident.t
  | Ope of op * t_expr_l list
  | App of Ident.t * t_expr_l list
  | Prim of Ident.t * t_expr_l list
  | Fby of t_expr_l * t_expr_l
  | When of t_expr_l * Ident.t
  | Whenot of t_expr_l * Ident.t
  | Tuple of t_expr_l list

type t_patt =
    { patt_desc: Ident.t list;
      patt_type: ty;
      patt_loc: location; }

type t_equation =
    { eq_patt: t_patt;
      eq_expr: t_expr_l; }

type t_node =
    { t_name: Ident.t;
      t_input: Clocking.clocked_var list;
      t_output: Clocking.clocked_var list;
      t_local: Clocking.clocked_var list;
      t_equs: t_equation list;
      t_loc: location; }

type t_file = t_node list