(* Importing required modules *)
open Format
open Asttypes
open Typed_ast
open Print_utils

(* This function prints a list of elements separated by a specified separator. *)
let rec print_list_with_separators f sep fmt = print_list f sep "%a%s@ %a" false fmt

(* Function to print expressions *)
let rec print_expr fmt expr = match expr.Expr.tdesc with
  | TE_const const -> print_const fmt const 
  | TE_ident identifier -> fprintf fmt "%a" Ident.print identifier 
  | TE_op (operator, expr_list) -> fprintf fmt "%a(%a)" print_operator operator print_arg_list expr_list 
  | TE_app (name, expr_list) | TE_prim (name, expr_list) -> fprintf fmt "%a(@[%a@])" Ident.print name print_arg_list expr_list
  | TE_arrow (left, right) -> fprintf fmt "@[(@[%a@]) -> (@[%a@])@]" print_expr left print_expr right
  | TE_pre expr -> fprintf fmt "pre (@[%a@])" print_expr expr
  | TE_tuple expr_list -> fprintf fmt "(@[%a@])" print_tuple_arg_list expr_list
  | TE_merge (expr, list) -> fprintf fmt "@[merge %a @\n  @[%a@]@]" print_expr expr
                             (print_list_with_new_lines (fun fmt (id,exp) -> fprintf fmt "(%a -> %a)" print_expr id print_expr exp)) list
  | TE_fby (expr1, expr2) -> fprintf fmt "@[%a fby %a@]" print_expr expr1 print_expr expr2
  | TE_when (expr1, str, expr2) -> fprintf fmt "@[%a when %s(%a)@]" print_expr expr1 str print_expr expr2
  | TE_reset (id, expr_list, expr) -> fprintf fmt "(%a(@[%a@])) every %a" Ident.print id print_arg_list expr_list print_expr expr

and print_arg_list fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_expr x
  | head :: tail -> fprintf fmt "%a,@ %a" print_expr head print_arg_list tail

and print_tuple_arg_list fmt = function
  | [] -> failwith "Empty tuple argument list"
  | [x] -> fprintf fmt "%a" print_expr x
  | head :: tail -> fprintf fmt "%a,@ %a" print_expr head print_arg_list tail

and print_const_expression_list fmt = function
  | [] -> failwith "Empty constant expression list"
  | [c] -> fprintf fmt "%a" print_const c
  | head :: tail -> fprintf fmt "%a,@ %a" print_const head print_const_expression_list tail

let print_equation fmt { Equation.tpatt = { Patt.tdesc }; Equation.texpr } =
  fprintf fmt "@[(%a) = @[%a@]@]"
    (print_list_with_separators Ident.print ",") tdesc
    print_expr texpr

let print_variable_declaration fmt (name, type_list) =
  fprintf fmt "%a : %a" Ident.print name print_base_type type_list

let print_variable_initialization_declaration fmt ((name, type_list), init) =
  fprintf fmt "%a: %a%s" Ident.print name print_base_type type_list
    (match init with
     | None -> ""
     | Some const -> Format.asprintf " init %a" print_const const)

let rec print_variable_declaration_list = print_list_with_separators print_variable_declaration ";"
let rec print_variable_initialization_declaration_list = print_list_with_separators print_variable_initialization_declaration ";"

let print_node fmt { Node.tname; tinput; toutput; tlocal; teqs } =
  fprintf fmt
    "@[node %a(@[%a@]) returns (@[%a@])@\nvar @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    Ident.print tname
    print_variable_declaration_list tinput
    print_variable_declaration_list toutput
    print_variable_initialization_declaration_list tlocal
    (print_list_combined print_equation ";") teqs

let print_node_list_std fmt node_list =
  List.iter (fprintf fmt "%a@\n@." print_node) node_list

let print_file_std {File.ttypes; File.tnodes} =
  Format.printf "%a@\n@\n%a"
    print_enumeratedtype ttypes
    print_node_list_std tnodes