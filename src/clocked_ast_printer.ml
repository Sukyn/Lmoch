open Format
open Asttypes
open Clocked_ast
open Clocking
open Print_utils

(* Helper function to print clock expressions *)
let rec print_clock_expr fmt = function
  | Cbase -> fprintf fmt "Base"
  | Con (c_expr, str_repr, id) ->
      fprintf fmt "@[%a on %s(%a)@]" print_clock_expr c_expr str_repr Ident.print id
  | Cvar {contents = (Clink _)} as linked -> print_clock_expr fmt (ck_repr linked)
  | Cvar {contents = Cindex idx} -> fprintf fmt "@[ck%i@]" idx

(* Helper function to print expressions *)
let rec print_expr fmt expr = 
  match expr.CExpr.cdesc with
  | CE_const c -> print_const fmt c
  | CE_ident id -> fprintf fmt "%a" Ident.print id
  | CE_op (op, exprs) -> fprintf fmt "%a@[(%a)@]" print_operator op print_expr_list exprs
  | CE_app (n, exprs) | CE_prim (n, exprs) -> fprintf fmt "%a(@[%a@])" Ident.print n print_expr_list exprs
  | CE_pre e -> fprintf fmt "pre (@[%a@])" print_expr e
  | CE_tuple exprs -> fprintf fmt "(@[%a@])" print_tuple_exprs exprs
  | CE_merge (m_expr, exprs) -> fprintf fmt "merge %a @\n@[%a@]" print_expr m_expr print_merge_exprs exprs
  | CE_fby (e1, e2) -> fprintf fmt "%a fby %a" print_expr e1 print_expr e2
  | CE_when (e1, str_repr, e_id) -> fprintf fmt "%a when %s(%a)" print_expr e1 str_repr print_expr e_id
  | CE_reset (id, exprs, e) -> fprintf fmt "%a(@[%a]) every %a" Ident.print id print_expr_list exprs print_expr e

(* Helper functions for printing lists of expressions *)
and print_expr_list fmt = pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") print_expr fmt
and print_tuple_exprs fmt = pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") print_expr fmt
and print_merge_exprs fmt = print_list (fun fmt (id, exp) -> fprintf fmt "(%a -> %a)" print_expr id print_expr exp) "" "%a%s@\n%a" false fmt

(* Function to print equations *)
let print_equation fmt { CEquation.cpattern = p; cexpr = e} =
  fprintf fmt "@[(%a) = @[%a@]@]" (pp_print_list ~pp_sep:print_comma Ident.print) p.CPatt.cdesc print_expr e

(* Function to print variables *)
let print_var fmt (var_name, type_list, c_expr) =
  fprintf fmt "@[<v>%a : %a :: %a@]" Ident.print var_name print_base_type type_list print_clock_expr c_expr

(* Function to print variables with initialization *)
let print_var_with_init fmt ((var_name, type_list, c_expr), init_val_opt) =
  let init_val_str = Option.fold ~none:"" ~some:(asprintf " init %a" print_const) init_val_opt in
  fprintf fmt "@[%a : %a :: %a%s@]" Ident.print var_name print_base_type type_list print_clock_expr c_expr init_val_str

(* Function to print nodes *)
let print_node fmt node =
  fprintf fmt "@[node %a(@[%a@])@\n  returns (@[%a@])@\nvar @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]" Ident.print node.CNode.name
    (pp_print_list ~pp_sep:print_comma print_var) node.CNode.input
    (pp_print_list ~pp_sep:print_comma print_var) node.CNode.output
    (pp_print_list ~pp_sep:print_comma print_var_with_init) node.CNode.local
    (pp_print_list ~pp_sep:print_new_line print_equation) node.CNode.equs

(* Function to print the standard file *)
let print_file_standard file =
  Format.printf "%a@\n@\n%a@." print_enumeratedtype file.CFile.types (pp_print_list ~pp_sep:pp_print_newline print_node) file.CFile.nodes
