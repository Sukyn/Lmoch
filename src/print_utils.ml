open Format
open Asttypes
open Parse_ast

(* Function to print a list of elements with a specified separator. 
   The 'template' parameter specifies the format of the output, 
   and 'print_last' determines whether the last element should be 
   followed by the separator *)
let rec print_list f sep template print_last fmt = function
  | [] -> ()
  | [x] -> if print_last then fprintf fmt "%a%s" f x sep else f fmt x
  | h :: t -> fprintf fmt template f h sep (print_list f sep template print_last) t

(* This function prints a list of elements, each on a new line. *)
let rec print_list_with_new_lines f fmt = print_list f "" "%a%s@\n%a" false fmt

(* This function prints a list of elements separated by a specified separator, each on a new line. *)
let rec print_list_combined f sep fmt = print_list f sep "%a%s@\n%a" true fmt
  
(* Function to print a constant value *)
let print_const fmt = function
  | BooleanConstant b -> fprintf fmt "%b" b
  | IntegerConstant i -> fprintf fmt "%d" i
  | RealConstant f -> fprintf fmt "%f" f
  | EnumeratedDataConstant (s, None) -> fprintf fmt "default(%s)" s
  | EnumeratedDataConstant (s, Some v) -> fprintf fmt "%s'%s" v s

(* Function to print an operator *)
let print_operator fmt = function
  | Op_eq -> fprintf fmt "eq"
  | Op_neq -> fprintf fmt "neq"
  | Op_lt -> fprintf fmt "lt"
  | Op_le -> fprintf fmt "le"
  | Op_gt -> fprintf fmt "gt"
  | Op_ge -> fprintf fmt "ge"
  | Op_add -> fprintf fmt "add"
  | Op_sub -> fprintf fmt "sub"
  | Op_mul -> fprintf fmt "mul"
  | Op_div -> fprintf fmt "div"
  | Op_mod -> fprintf fmt "mod"
  | Op_not -> fprintf fmt "~"
  | Op_and -> fprintf fmt "and"
  | Op_or -> fprintf fmt "or"
  | Op_impl -> fprintf fmt "impl"
  | Op_if -> fprintf fmt "ite"

(* Function to print a base type *)
let print_base_type fmt = function
  | BooleanType -> fprintf fmt "bool"
  | IntegerType -> fprintf fmt "int"
  | RealType -> fprintf fmt "real"
  | EnumeratedDataType s -> fprintf fmt "%s" s

(* This function prints an enumerated type. It takes a formatter and a list of enumerated types as arguments. *)
let print_enumeratedtype fmt =
  fprintf fmt "%a"
    (print_list_with_new_lines (fun fmt {name; constructors} ->
      fprintf fmt "type %s =@\n  @[%a@]"
        name
        (print_list_with_new_lines (fun fmt e -> fprintf fmt "| %s" e))
        constructors
    ))

(* Utility functions for printing new lines, skipping lines, and commas *)
let print_new_line fmt () = fprintf fmt ";@\n"
let print_comma fmt () = fprintf fmt ", "

