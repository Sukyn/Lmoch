open Format
open Asttypes
open Parse_ast
open Print_utils

(* This function prints an expression *)
let rec print_expression fmt expr =
  match expr.PExpr.desc with
  | Const const -> print_const fmt const  (* Print a constant *)
  | Ident identifier -> fprintf fmt "%s" identifier  (* Print an identifier *)
  | Op (operator, expr_list) -> fprintf fmt "%a(@[%a@])" print_operator operator print_arg_list expr_list  (* Print an operator with operands *)
  | App (name, expr_list) -> fprintf fmt "%s(@[%a@])" name print_arg_list expr_list  (* Print a function application *)
  | Arrow (left, right) -> fprintf fmt "@[(@[%a@]) -> (@[%a@])@]" print_expression left print_expression right  (* Print an arrow function *)
  | Pre expr -> fprintf fmt "pre (@[%a@])" print_expression expr  (* Print a pre operator *)
  | Tuple expr_list -> fprintf fmt "(@[%a@])" print_arg_list expr_list  (* Print a tuple *)
  | Merge (name, list) -> fprintf fmt "@[merge %a @\n  @[%a@]@]" print_expression name (print_list_with_new_lines (fun fmt (id,exp) -> fprintf fmt "(%a -> %a)" print_expression id print_expression exp)) list  (* Print a merge operator *)
  | When (expr1, c, expr2) -> fprintf fmt "@[%a when %s(%a)@]" print_expression expr1 c print_expression expr2  (* Print a when operator *)
  | Reset (id, expr_list, expr) -> fprintf fmt "%s(@[%a@]) every %a" id print_arg_list expr_list print_expression expr  (* Print a reset operator *)

(* This function prints a list of arguments *)
and print_arg_list fmt expr_list =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") print_expression fmt expr_list

(* This function prints a list of constant expressions *)
and print_const_exp fmt const_expr_list =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") print_const fmt const_expr_list
  
(* This function prints an identifier *)
let print_identifier fmt = function
  | PPattern.Ident s -> Format.fprintf fmt "%s" s
  | PPattern.Tuple t -> Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") Format.pp_print_string fmt t

(* This function prints an equation *)
let print_equation fmt = function
  | Eq { pattern = { PPattern.desc = pattern }; expr } ->
    fprintf fmt "@[(%a) = @[%a@]@]" print_identifier pattern print_expression expr
  | Automaton _ | Match _ ->
    failwith "Not implemented"

(* This function prints a variable declaration *)
let print_var_declaration fmt (name, typeList) =
  fprintf fmt "%s : %a" name print_base_type typeList

(* This function prints a list of variable declarations *)
let print_var_declaration_list fmt var_decl_list =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";") print_var_declaration fmt var_decl_list
  
(* This function prints a node *)
let print_node fmt node =
    fprintf fmt
      "@[node %s(@[%a@]) returns (@[%a@]);@\nvar @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
      node.node_name
      print_var_declaration_list node.node_inputs
      print_var_declaration_list node.node_outputs
      print_var_declaration_list node.node_localvars
      (print_list_combined print_equation ";") node.node_equs

(* This function prints a list of nodes *)
let print_node_list_standard fmt node_list =
    List.iter (fun node -> Format.fprintf fmt "%a@\n@." print_node node) node_list


(* This function prints a file *)
let print_file_standard {additional_types; nodes} =
    Format.printf "%a@\n@\n%a"
      print_enumeratedtype additional_types
      print_node_list_standard nodes
 