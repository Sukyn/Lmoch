open Format
open Clocked_ast_printer
open Imp_ast
open Print_utils

let rec pp_expr_list fmt = pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_iexp fmt

and pp_iexp fmt expr =
  let open IExpr in
  match expr.idesc with
  | IE_const c -> print_const fmt c
  | IE_mem ident | IE_ident ident -> Ident.print fmt ident
  | IE_op (operator, expr_list) -> fprintf fmt "@[%a(%a)@]" print_operator operator pp_expr_list expr_list
  | IE_tuple expr_list -> fprintf fmt "(@[%a@])" pp_expr_list expr_list
  | IE_app (ident1, ident2, expr_list) -> fprintf fmt "%a <- %a(@[%a@])" Ident.print ident1 Ident.print ident2 pp_expr_list expr_list
  | IE_prim (ident, expr_list) -> fprintf fmt "%a(@[%a@])" Ident.print ident pp_expr_list expr_list
  | IE_case (expr, li) -> fprintf fmt "case(%a)@[<hov 2>%a@]" pp_iexp expr (pp_print_list ~pp_sep:print_new_line (fun fmt (l, r) -> fprintf fmt "%a: %a"pp_iexp l pp_iexp r)) li
  | IE_reset (ident1, ident2, expr_list, expr) -> fprintf fmt "%a <- %a(@[@%a]) every %a" Ident.print ident1 Ident.print ident2 pp_expr_list expr_list pp_iexp expr

let pretty_print_variable fmt (var_name, type_list, c_expr) =
  fprintf fmt "@[<v>%a : %a :: %a@]" Ident.print var_name print_base_type type_list print_clock_expr c_expr

let pp_ieq fmt {IEquation.pattern; IEquation.expr} =
  fprintf fmt "@[(%a) = @[%a@]@]" (pp_print_list ~pp_sep:print_comma pretty_print_variable) pattern pp_iexp expr

let pp_node_mem_init fmt (identifier1, identifier2) =
  fprintf fmt "(%a,%a)" Ident.print identifier1 Ident.print identifier2

let pp_mem fmt {Mem.fby; node_mem} =
  fprintf fmt "node=[@[%a@]]@\nfby=[@[%a@]]" (pp_print_list ~pp_sep:print_comma pp_node_mem_init) node_mem (pp_print_list ~pp_sep:print_comma pretty_print_variable) fby

let pp_atom fmt = function
  | Atom.Const c -> print_const fmt c
  | Ident i -> Ident.print fmt i

let pp_ident_atom fmt (id, at) =  fprintf fmt "(%a,%a)" Ident.print id pp_atom at

let pp_init fmt {Init.fby; Init.node} =
  fprintf fmt "node=[@[%a@]]@\nfby=[@[%a@]]" (pp_print_list ~pp_sep:print_comma pp_node_mem_init) node (pp_print_list ~pp_sep:print_comma pp_ident_atom) fby

let pp_compute fmt = fprintf fmt "%a" (pp_print_list ~pp_sep:print_new_line pp_ieq)
let pp_update fmt = fprintf fmt "%a" (pp_print_list ~pp_sep:print_new_line pp_ident_atom)

let pretty_print_variable_with_init fmt ((var_name, type_list, c_expr), init_val_opt) =
  let init_val_str = Option.fold ~none:"" ~some:(asprintf " init %a" print_const) init_val_opt in
  fprintf fmt "@[%a : %a :: %a%s@]" Ident.print var_name print_base_type type_list print_clock_expr c_expr init_val_str

let pp_node fmt (nd: INode.t) =
  fprintf fmt "@[node %a(@[%a@])@\n  returns (@[%a@])@\nvar @[%a;@]@\n  @[<v>mem={@[%a@]}@\ninit={@[%a@]}@\ncompute={@[%a@]}@\nupdate={@[%a@]}@\n@]" Ident.print nd.INode.name (pp_print_list ~pp_sep:print_comma pretty_print_variable) nd.INode.input_step (pp_print_list ~pp_sep:print_comma pretty_print_variable) nd.INode.output_step (pp_print_list ~pp_sep:print_comma pretty_print_variable_with_init) nd.INode.local pp_mem nd.INode.mem pp_init nd.INode.init pp_compute nd.INode.compute pp_update nd.INode.update

let pp (f : IFile.t) =
  printf "%a@\n@\n%a@." print_enumeratedtype f.IFile.types (pp_print_list ~pp_sep:pp_print_newline pp_node) f.IFile.nodes
