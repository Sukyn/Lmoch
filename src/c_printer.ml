open GoblintCil
open Format
open Print_utils

let print_skip_line fmt () = fprintf fmt ";@;"

let rec pp_type fmt = function
  | TInt (IInt, _) -> fprintf fmt "int"
  | TInt (IChar, _) -> fprintf fmt "char"
  | TFloat (_, _) -> fprintf fmt "float"
  | TPtr (t, _) -> fprintf fmt "%a*" pp_type t
  | TVoid _ -> fprintf fmt "void"
  | TComp ({cname; _}, _) -> fprintf fmt "struct %s" cname
  | TEnum ({ename; _}, _) -> fprintf fmt "enum %s" ename
  | _ -> failwith "Unsupported type"

let pp_unop fmt = function
  | Neg -> fprintf fmt "-"
  | BNot -> fprintf fmt "~"
  | LNot -> fprintf fmt "!"

let pp_binop fmt = function
  | PlusA | PlusPI | IndexPI -> fprintf fmt "+"
  | MinusA | MinusPI | MinusPP -> fprintf fmt "-"
  | Mult -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Mod -> fprintf fmt "%%"
  | Shiftlt -> fprintf fmt "<<"
  | Shiftrt -> fprintf fmt ">>"
  | Lt -> fprintf fmt "<"
  | Gt -> fprintf fmt ">"
  | Le -> fprintf fmt "<="
  | Ge -> fprintf fmt ">="
  | Eq -> fprintf fmt "=="
  | Ne -> fprintf fmt "!="
  | BAnd -> fprintf fmt "&"
  | BXor -> fprintf fmt "^"
  | BOr -> fprintf fmt "|"
  | LAnd -> fprintf fmt "&&"
  | LOr -> fprintf fmt "||"

let rec pp_offset is_mem fmt = function
  | NoOffset -> ()
  | Field (f, offset) -> fprintf fmt "%s%s%a" (if is_mem then "->" else ".") f.fname (pp_offset is_mem) offset
  | Index (idx, offset) -> fprintf fmt "[%a]%a" pp_exp idx (pp_offset is_mem) offset

and pp_exp fmt = function
  | Const (CInt (i, _, _)) -> fprintf fmt "%i" (cilint_to_int i)
  | Const (CReal (f, _, _)) -> fprintf fmt "%f" f
  | Const (CEnum (Const (CInt (i, _, _)), _, {eitems; _})) ->
    let e, _, _ = List.nth eitems (Z.to_int i) in fprintf fmt "%s" e
  | Const (CStr (s, _)) -> fprintf fmt "%s" s
  | Const _ -> failwith "Unsupported constant"
  | Lval lval -> pp_lval fmt lval
  | UnOp (op, e, _) -> fprintf fmt "%a(%a)" pp_unop op pp_exp e
  | BinOp (op, e1, e2, _) -> fprintf fmt "(%a %a %a)" pp_exp e1 pp_binop op pp_exp e2
  | Question (e1, e2, e3, _) -> fprintf fmt "%a ? %a : %a" pp_exp e1 pp_exp e2 pp_exp e3
  | CastE (t, e) -> fprintf fmt "(%a)%a" pp_type t pp_exp e
  | AddrOf lval -> fprintf fmt "&(%a)" pp_lval lval
  | _ -> failwith "Unsupported expression"
  
and pp_var fmt var = fprintf fmt "%s" var.vname

and pp_lval fmt = function
  | Var v, offset -> fprintf fmt "%a%a" pp_var v (pp_offset false) offset
  | Mem e, NoOffset -> fprintf fmt "*(mem)"
  | Mem e, offset -> fprintf fmt "mem%a" (pp_offset true) offset

let pp_call fmt ret call =
  let pp_maybe_lval fmt = function
    | Some lval -> fprintf fmt "%a = " pp_lval lval
    | None -> ()
  in
  fprintf fmt "%a%a(%a)" pp_maybe_lval ret pp_exp call (pp_print_list ~pp_sep:print_comma pp_exp)
  
let pp_arg fmt (n, typeList, _) = match typeList with
  | TArray (typeList, Some size, _) -> fprintf fmt "%a %s[%a]" pp_type typeList n pp_exp size
  | TArray (typeList, None, _) -> fprintf fmt "%a %s[]" pp_type typeList n
  | _ -> fprintf fmt "%a %s" pp_type typeList n

let pp_args fmt = function
  | Some args -> fprintf fmt "%a" (pp_print_list ~pp_sep:print_comma pp_arg) args
  | None -> fprintf fmt ""

let pp_local fmt local =
  let typeList, name, init = local.vtype, local.vname, local.vinit.init in
  let init = match init with
    | Some (SingleInit exp) -> asprintf " = %a" pp_exp exp
    | _ -> ""
  in
  fprintf fmt "%a %s%s;" pp_type typeList name init

let pp_locals fmt locals = fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_cut pp_local) locals

let pp_instr fmt = function
  | Set (lval, e, _, _) -> fprintf fmt "%a = %a" pp_lval lval pp_exp e
  | Call (ret, call, args, _, _) -> pp_call fmt ret call args
  | _ -> failwith "Unsupported instruction"
  
let rec pp_stmt_case fmt stmts =
  let cases = List.fold_left (fun l stmt -> match stmt.labels, l with
    | [Case _], _ -> [stmt] :: l
    | _, hd :: tl -> (stmt :: hd) :: tl
    | _ -> failwith "Unsupported statement case"
  ) [] stmts |> List.rev_map List.rev
  in
  fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_cut (fun fmt stmts ->
    match (List.hd stmts).labels with
    | [Case (e, _, _)] -> fprintf fmt "case %a: {@;<2 2>@[<v>%a;@]@;}" pp_exp e (pp_print_list ~pp_sep:print_skip_line pp_stmt) stmts
    | _ -> failwith "Unsupported case"
  )) cases

and print_block_if fmt cond b1 b2 = match b2 with
  | Some b2 -> fprintf fmt "if (%a) {@;<2 2>@[<v>%a@]@;} else {@;<2 2>@[<v>%a@]@;}" pp_exp cond (pp_block false) b1 (pp_block false) b2
  | None -> fprintf fmt "if (%a) {@;<2 2>@[<v>%a@]@;}" pp_exp cond (pp_block false) b1

and pp_stmt fmt stmt = match stmt.skind with
  | Instr il -> fprintf fmt "%a" (pp_print_list ~pp_sep:print_skip_line pp_instr) il
  | Return (e, _) -> (match e with Some e -> fprintf fmt "return %a" pp_exp e | None -> fprintf fmt "return")
  | Switch (e, _, stmts, _, _) -> fprintf fmt "switch (%a) {@;<2 2>@[<v>%a@]@;}" pp_exp e pp_stmt_case stmts
  | Break _ -> fprintf fmt "break"
  | Loop (b, _, _, _, _) -> fprintf fmt "while (1) {@;<2 2>@[<v>%a@]@;}" (pp_block false) b
  | If (cond, b1, b2, _, _) when b2.bstmts = [] -> print_block_if fmt cond b1 None
  | If (cond, b1, b2, _, _) -> print_block_if fmt cond b1 (Some b2)
  | _ -> failwith "Unsupported statement"

and pp_block brackets fmt block = if brackets then fprintf fmt "{@;<2 2>@[<v>%a;@]@;}@\n" (pp_print_list ~pp_sep:print_skip_line pp_stmt) block.bstmts
  else fprintf fmt "%a;" (pp_print_list ~pp_sep:print_skip_line pp_stmt) block.bstmts

let pp_fundec fmt fundec = let ret_ty, args = match fundec.svar.vtype with
    | TFun (typ, args, _, _) -> typ, args
    | _ -> failwith "Unsupported function declaration"
  in
  fprintf fmt "%a %s (%a) {@;<2 2>@[<v>%a%a%a@]@\n}@\n" pp_type ret_ty fundec.svar.vname pp_args args pp_locals fundec.slocals (fun fmt () -> if fundec.slocals <> [] then fprintf fmt "@;@;") () (pp_block false) fundec.sbody

let pp_comma_cut fmt () = fprintf fmt ",@;"

let pp_typeinfo fmt typeinfo = match typeinfo.ttype with
  | TEnum ({eitems; _}, _) -> fprintf fmt "enum %s {@;<2 2>@[<v>%a@]@\n};@\n" typeinfo.tname (pp_print_list ~pp_sep:pp_comma_cut (fun fmt (e, _, _) -> pp_print_string fmt e)) eitems
  | _ -> failwith "Unsupported type information"

let pp_global fmt = function
  | GCompTag ({cname; cfields}, _) -> fprintf fmt "struct %s {@\n@;<2 2>@[<v>%a@]@;};@\n" cname (pp_print_list ~pp_sep:pp_print_cut (fun fmt {ftype; fname} -> fprintf fmt "%a %s;" pp_type ftype fname)) cfields
  | GFun (fundec, _) -> pp_fundec fmt fundec
  | GType (typeinfo, _) -> pp_typeinfo fmt typeinfo
  | GText s -> fprintf fmt "%s\n" s
  | _ -> failwith "Unsupported global expression"

let pp fmt file = fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_newline pp_global) file.globals
