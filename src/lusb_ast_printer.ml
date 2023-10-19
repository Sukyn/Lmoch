open Format
open Asttypes
open Lusb_ast

let rec print_list f sep fmt l = match l with
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s@ %a" f h sep (print_list f sep) t

let rec print_list_eol f sep fmt l = match l with
  | [] -> ()
  | [x] -> fprintf fmt "%a%s" f x sep
  | h :: t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol f sep) t

let print_const fmt c = match c with
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Creal f -> fprintf fmt "%f" f

let print_op fmt op = match op with
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
  | Op_add_f -> fprintf fmt "add_f"
  | Op_sub_f -> fprintf fmt "sub_f"
  | Op_mul_f -> fprintf fmt "mul_f"
  | Op_div_f -> fprintf fmt "div_f"
  | Op_not -> fprintf fmt "~"
  | Op_and -> fprintf fmt "and"
  | Op_or -> fprintf fmt "or"
  | Op_impl -> fprintf fmt "impl"
  | Op_if -> fprintf fmt "ite"

let rec print_exp fmt e = match e with
  | Nil -> fprintf fmt "nil"
  | Const c -> print_const fmt c
  | Id x -> fprintf fmt "%a" Ident.print x
  | Ope (op, el) -> fprintf fmt "%a(%a)" print_op op print_arg_list el
  | App (name, e_list) | Prim (name, e_list) ->
      fprintf fmt "%a(@[%a@])" Ident.print name print_arg_list e_list
  | Fby (e,e') -> fprintf fmt "(%a) fby (%a)" print_exp e print_exp e' 
  | When (e,x) -> fprintf fmt "(%a) when (%a)" print_exp e Ident.print x
  | Whenot (e,x) -> fprintf fmt "(%a) when not (%a)" print_exp e Ident.print x
  | Merge (x, e1, e2) -> fprintf fmt "merge %a (%a) (%a)" Ident.print x print_exp e1 print_exp e2
  | Tuple e_list ->
      fprintf fmt "(@[%a@])" print_tuple_arg_list e_list

and print_arg_list fmt e_list = match e_list with
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_tuple_arg_list fmt e_list = match e_list with
  | [] -> assert false
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_const_exp fmt ce_list = match ce_list with
  | [] -> assert false
  | [c] -> fprintf fmt "%a" print_const c
  | h :: t -> fprintf fmt "%a,@ %a" print_const h print_const_exp t

let print_eq fmt eq =
  fprintf fmt "@[(%a) = @[%a@]@]"
    (print_list Ident.print ",") eq.eq_patt.patt_desc
    print_exp eq.eq_expr.ttexpr_desc

let print_base_type fmt bty = match bty with
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Treal -> fprintf fmt "real"

(* let print_type = print_list print_cbase_type "*" *)

let print_clockb fmt clock = match clock with 
    | Clocking.Cbase -> fprintf fmt "base"
    | Clocking.Ccon (True n) -> fprintf fmt "on %a" Ident.print n
    | Clocking.Ccon (False n) -> fprintf fmt "on not %a" Ident.print n

let rec print_clock fmt clock = match clock with
   | Clocking.Sing n -> print_clockb fmt n
   | Clocking.Mul l -> print_list print_clock "," fmt l

let print_var_dec fmt (name, ty, clock) =
  fprintf fmt "%a : %a [%a]" Ident.print name print_base_type ty print_clock clock

let rec print_var_dec_list = print_list print_var_dec ";" 


let print_node fmt nd =
  fprintf fmt
    "@[node %a(@[%a@]) returns (@[%a@])@\nvar @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    Ident.print nd.t_name
    print_var_dec_list nd.t_input
    print_var_dec_list nd.t_output
    print_var_dec_list nd.t_local
    (print_list_eol print_eq ";") nd.t_equs

let print_node_list_std ndl =
  List.iter (fun nd -> Format.printf "%a@\n@." print_node nd) ndl
