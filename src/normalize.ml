open Typed_ast

(* A function to generate unique identifiers for auxiliary variables *)
let generate_unique_identifier =
  let counter = ref 0 in
  fun () ->
    incr counter;
    Ident.make (Printf.sprintf "aux'%d" !counter) Ident.Stream
    
(* Create a new pattern for an expression, which is used in normalization *)
let new_pattern ({Expr.ttype = expression_types; Expr.tloc = location; _} as expr) =
  (* Generate a list of identifiers and corresponding types *)
  let declaration = List.map (fun type_element -> (generate_unique_identifier (), type_element)) expression_types in
  let identifiers, types = List.split declaration in
  (* Create an expression list with these identifiers and types *)
  let expr_list = List.map2 (fun id type_element ->
      { Expr.tdesc = TE_ident id; Expr.ttype = [type_element]; Expr.tloc = location; }) identifiers types in
  declaration,
  { Patt.tdesc = identifiers; Patt.ttype = types; Patt.tloc = location; },
  { expr with Expr.tdesc = if List.length types = 1 then TE_ident (List.hd identifiers) else TE_tuple expr_list; }

(* Implement is_constant_zero and is_constant_one based on how you represent constants in your AST. *)
let is_constant_zero expr =
  match expr.Expr.tdesc with
  | TE_const (IntegerConstant 0) -> true
  | TE_const (RealConstant 0.) -> true
  | _ -> false

let is_constant_one expr =
  match expr.Expr.tdesc with
  | TE_const (IntegerConstant 1) -> true
  | TE_const (RealConstant 1.) -> true
  | _ -> false

(* Normalize the operands in an expression *)
let rec normalize_operands context = function
  | [] -> context, []
  | hd :: tl ->
    let new_context, norm_hd = normalize context hd in
    let final_context, norm_tl = normalize_operands new_context tl in
    final_context, norm_hd :: norm_tl

and apply_operator current_context operator operand_list e =
  let current_context, pre_normalized_operands = normalize_operands current_context operand_list in
  match operator with
  | Asttypes.Op_not -> 
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (BooleanConstant b); _}] ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant (not b))}
      | [{Expr.tdesc = TE_op (Op_not, [operand]); _}] -> 
        current_context, operand
      | [{Expr.tdesc = TE_op (Op_and, [op1; op2]); _}] ->
         current_context, {e with Expr.tdesc = TE_op (Op_or, [op1; op2])}
      | [{Expr.tdesc = TE_op (Op_or, [op1; op2]); _}] ->
        current_context, {e with Expr.tdesc = TE_op (Op_and, [op1; op2])}
      | [{Expr.tdesc = TE_fby (op1, op2); _}] ->
        current_context, {e with Expr.tdesc = TE_fby({op1 with Expr.tdesc = TE_op(Op_not, [op1])}, {op2 with Expr.tdesc = TE_op(Op_not, [op2])})}
      | [operand] -> current_context, {e with Expr.tdesc = TE_op (Op_not, [operand])}
      | _ -> failwith "Invalid number of operands for operator not"
      end
  | Op_eq ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = operand1; _}; {Expr.tdesc = operand2; _}] when operand1 = operand2 ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
      | [{Expr.tdesc = TE_const (a); _}; {Expr.tdesc = TE_const (b); _}]->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a == b))}
      | [{Expr.tdesc = TE_const (BooleanConstant true); _}; op2]->
        current_context, op2
      | [op1; {Expr.tdesc = TE_const (BooleanConstant true); _}]->
        current_context, op1
      | [{Expr.tdesc = TE_const (BooleanConstant false); _}; op2]->
        current_context, {e with Expr.tdesc = TE_op (Op_not, [op2])}
      | [op1; {Expr.tdesc = TE_const (BooleanConstant false); _}]->
        current_context, {e with Expr.tdesc = TE_op (Op_not, [op1])}
      | [op1; {Expr.tdesc = TE_op (Op_not, [op2]); _}] when op1.tdesc = op2.tdesc ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
      | [{Expr.tdesc = TE_op (Op_not, [op1]); _}; op2] when op1.tdesc = op2.tdesc ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
      | [op1; op2] ->
          current_context, {e with Expr.tdesc = TE_op (Op_eq, [op1; op2])}
      | _ -> failwith "Invalid number of operands for operator =="
      end
  | Op_neq ->
    begin match pre_normalized_operands with
    | [{Expr.tdesc = operand1; _}; {Expr.tdesc = operand2; _}] when operand1 = operand2 ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
    | [{Expr.tdesc = TE_const (a); _}; {Expr.tdesc = TE_const (b); _}]->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a <> b))}
    | [{Expr.tdesc = TE_const (BooleanConstant false); _}; op2]->
      current_context, op2
    | [op1; {Expr.tdesc = TE_const (BooleanConstant false); _}]->
      current_context, op1
    | [{Expr.tdesc = TE_const (BooleanConstant true); _}; op2]->
      current_context, {e with Expr.tdesc = TE_op (Op_not, [op2])}
    | [op1; {Expr.tdesc = TE_const (BooleanConstant true); _}]->
      current_context, {e with Expr.tdesc = TE_op (Op_not, [op1])}
    | [op1; {Expr.tdesc = TE_op (Op_not, [op2]); _}] when op1.tdesc = op2.tdesc ->
      current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
    | [{Expr.tdesc = TE_op (Op_not, [op1]); _}; op2] when op1.tdesc = op2.tdesc ->
      current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
    | [op1; op2] ->
        current_context, {e with Expr.tdesc = TE_op (Op_neq, [op1; op2])}
    | _ -> failwith "Invalid number of operands for operator <>"
    end
  | Op_lt ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (IntegerConstant a); _}; {Expr.tdesc = TE_const (IntegerConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a < b))}
      | [{Expr.tdesc = TE_const (RealConstant a); _}; {Expr.tdesc = TE_const (RealConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a < b))}
          | [operand1; operand2] when operand1.tdesc = operand2.tdesc ->
            current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
          | [op1; op2] ->
          current_context, {e with Expr.tdesc = TE_op (Op_lt, [op1; op2])}
      | _ -> failwith "Invalid number of operands for operator <"
      end
  | Op_le ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (IntegerConstant a); _}; {Expr.tdesc = TE_const (IntegerConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a <= b))}
      | [{Expr.tdesc = TE_const (RealConstant a); _}; {Expr.tdesc = TE_const (RealConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a <= b))}
      | [operand1; operand2] when operand1.tdesc = operand2.tdesc ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
      | [op1; op2] ->
          current_context, {e with Expr.tdesc = TE_op (Op_le, [op1; op2])}
      | _ -> failwith "Invalid number of operands for operator <="
      end
  | Op_gt ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (IntegerConstant a); _}; {Expr.tdesc = TE_const (IntegerConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a > b))}
      | [{Expr.tdesc = TE_const (RealConstant a); _}; {Expr.tdesc = TE_const (RealConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a > b))}
          | [operand1; operand2] when operand1.tdesc = operand2.tdesc ->
            current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
          | [op1; op2] ->
          current_context, {e with Expr.tdesc = TE_op (Op_gt, [op1; op2])}
      | _ -> failwith "Invalid number of operands for operator >"
      end
  | Op_ge ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (IntegerConstant a); _}; {Expr.tdesc = TE_const (IntegerConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a >= b))}
      | [{Expr.tdesc = TE_const (RealConstant a); _}; {Expr.tdesc = TE_const (RealConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a >= b))}
          | [operand1; operand2] when operand1.tdesc = operand2.tdesc ->
            current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
          | [op1; op2] ->
          current_context, {e with Expr.tdesc = TE_op (Op_ge, [op1; op2])}
      | _ -> failwith "Invalid number of operands for operator >="
      end
  | Op_add -> 
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (IntegerConstant a); _}; {Expr.tdesc = TE_const (IntegerConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (IntegerConstant (a + b))}
      | [op1; op2] when is_constant_zero op1 -> current_context, op2
      | [op1; op2] when is_constant_zero op2 -> current_context, op1
      | [op1; {Expr.tdesc = TE_op (Op_sub, [op2]); _}]  ->
        normalize current_context {e with Expr.tdesc = TE_op (Op_sub, [op1; op2])}
      | [op1; op2] -> current_context, {e with Expr.tdesc = TE_op (Op_add, [op1; op2])}
      | _ -> failwith "Invalid number of operands for operator +"
      end
  | Op_sub ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (IntegerConstant a); _}; {Expr.tdesc = TE_const (IntegerConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (IntegerConstant (a - b))}
      | [op1; op2] when is_constant_zero op2 -> current_context, op1
      | [op1; op2] when is_constant_zero op1 -> current_context, {e with Expr.tdesc = TE_op (Op_sub, [op2])}
      | [op1; op2] when op1.tdesc = op2.tdesc -> current_context, {e with Expr.tdesc = TE_const (IntegerConstant 0)}
      | [op1; op2] -> current_context, {e with Expr.tdesc = TE_op (Op_sub, [op1; op2])}
      | [op1] when is_constant_zero op1 -> current_context, {e with Expr.tdesc = TE_const (IntegerConstant 0)}
      | [op1] -> current_context, {e with Expr.tdesc = TE_op (Op_sub, [op1])}
      | _ -> failwith "Invalid number of operands for operator -"
      end
  | Op_mul ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (IntegerConstant a); _}; {Expr.tdesc = TE_const (IntegerConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (IntegerConstant (a * b))}
      | [op1; op2] when is_constant_one op1 -> current_context, op2
      | [op1; op2] when is_constant_one op2 -> current_context, op1
      | [op1; op2] when is_constant_zero op1 || is_constant_zero op2 -> current_context, {e with Expr.tdesc = TE_const (IntegerConstant 0)}
      | [op1; op2] -> current_context, {e with Expr.tdesc = TE_op (Op_mul, [op1; op2])}
      | _ -> failwith "Invalid number of operands for operator *"
      end
  | Op_div ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (IntegerConstant a); _}; {Expr.tdesc = TE_const (IntegerConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (IntegerConstant (a / b))}
      | [op1; op2] when is_constant_one op2 -> current_context, op1
      | [op1; op2] when op1.tdesc = op2.tdesc -> current_context, {e with Expr.tdesc = TE_const (IntegerConstant 1)}
      | [op1; op2] -> current_context, {e with Expr.tdesc = TE_op (Op_div, [op1; op2])}
      | _ -> failwith "Invalid number of operands for operator /"
      end
  | Op_mod ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (IntegerConstant a); _}; {Expr.tdesc = TE_const (IntegerConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (IntegerConstant (a mod b))}
      | [op1; op2] -> current_context, {e with Expr.tdesc = TE_op (Op_mod, [op1; op2])}
      | _ -> failwith "Invalid number of operands for operator %"
      end
  | Op_and -> 
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (BooleanConstant a); _}; {Expr.tdesc = TE_const (BooleanConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a && b))}
      | [op1; {Expr.tdesc = TE_const (BooleanConstant false); _}] -> 
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
      | [{Expr.tdesc = TE_const (BooleanConstant false); _}; op2] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
      | [op1; {Expr.tdesc = TE_const (BooleanConstant true); _}] ->
          current_context, op1
      | [{Expr.tdesc = TE_const (BooleanConstant true); _}; op2] ->
          current_context, op2
      | [{Expr.tdesc = operand1; _}; {Expr.tdesc = operand2; _}] when operand1 = operand2 ->
          current_context, {e with Expr.tdesc = operand1}
      | [op1; {Expr.tdesc = TE_op (Op_not, [op2]); _}] when op1.tdesc = op2.tdesc ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
      | [{Expr.tdesc = TE_op (Op_not, [op1]); _}; op2] when op1.tdesc = op2.tdesc ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
      | [op1; op2] -> current_context, {e with Expr.tdesc = TE_op (Op_and, [op1; op2])}
      | _ -> failwith "Invalid number of operands for operator &&"
      end
  | Op_or ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (BooleanConstant a); _}; {Expr.tdesc = TE_const (BooleanConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant (a || b))}
      | [op1; {Expr.tdesc = TE_const (BooleanConstant true); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
      | [{Expr.tdesc = TE_const (BooleanConstant true); _}; op2] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
      | [op1; {Expr.tdesc = TE_const (BooleanConstant false); _}] ->
          current_context, op1
      | [{Expr.tdesc = TE_const (BooleanConstant false); _}; op2] ->
          current_context, op2
      | [operand1; operand2] when operand1.tdesc = operand2.tdesc ->
          current_context, operand1
      | [op1; {Expr.tdesc = TE_op (Op_not, [op2]); _}] when op1.tdesc = op2.tdesc ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
      | [{Expr.tdesc = TE_op (Op_not, [op1]); _}; op2] when op1.tdesc = op2.tdesc ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
      | [op1; op2] -> current_context, {e with Expr.tdesc = TE_op (Op_or, [op1; op2])}
      | _ -> failwith "Invalid number of operands for operator ||"
      end
  | Op_impl ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (BooleanConstant a); _}; {Expr.tdesc = TE_const (BooleanConstant b); _}] ->
          current_context, {e with Expr.tdesc = TE_const (BooleanConstant ((not a) || b))}
      | [op1; {Expr.tdesc = TE_const (BooleanConstant true); _}] ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
      | [{Expr.tdesc = TE_const (BooleanConstant true); _}; op2] ->
        current_context, op2
      | [op1; {Expr.tdesc = TE_const (BooleanConstant false); _}] ->
        current_context, {e with Expr.tdesc = TE_op (Op_not, [op1])}
      | [{Expr.tdesc = TE_const (BooleanConstant false); _}; op2] ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
      | [operand1; operand2] when operand1.tdesc = operand2.tdesc ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
      | [op1; {Expr.tdesc = TE_op (Op_not, [op2]); _}] when op1.tdesc = op2.tdesc ->
        current_context, {e with Expr.tdesc = TE_op (Op_not, [op1])}
      | [{Expr.tdesc = TE_op (Op_not, [op1]); _}; op2] when op1.tdesc = op2.tdesc ->
        current_context, op2
      | [op1; op2] -> current_context, {e with Expr.tdesc = TE_op (Op_or, [{e with Expr.tdesc = TE_op (Op_not, [op1])}; op2])}
      | _ -> failwith "Invalid number of operands for operator ->"
      end
  | Op_if ->
      begin match pre_normalized_operands with
      | [{Expr.tdesc = TE_const (BooleanConstant true); _}; op2; _] ->
          current_context, op2
      | [{Expr.tdesc = TE_const (BooleanConstant false); _}; _; op3] ->
          current_context, op3
      | [_; op2; op3] when op3.tdesc = op2.tdesc ->
          current_context, op2
      | [{Expr.tdesc = TE_op (Op_not, [b]); _}; op1; op2] ->
          normalize current_context {e with Expr.tdesc = TE_op (Op_if, [b; op2; op1])}
      | [b; {Expr.tdesc = TE_const (BooleanConstant true); _}; {Expr.tdesc = TE_const (BooleanConstant false); _}] ->
        current_context, b
      | [b; {Expr.tdesc = TE_const (BooleanConstant false); _}; {Expr.tdesc = TE_const (BooleanConstant true); _}] ->
        current_context, {e with Expr.tdesc = TE_op (Op_not, [b])}
      | [b; {Expr.tdesc = TE_const (BooleanConstant true); _}; {Expr.tdesc = TE_op (Op_not, [b']); _}] when b.tdesc = b'.tdesc ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant true)}
      | [b; {Expr.tdesc = TE_const (BooleanConstant false); _}; b'] when b.tdesc = b'.tdesc ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
      | [b; {Expr.tdesc = TE_op (Op_not, [b']); _}; {Expr.tdesc = TE_const (BooleanConstant false); _}] when b.tdesc = b'.tdesc ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
      | [b; {Expr.tdesc = TE_op (Op_not, [b']); _}; {Expr.tdesc = TE_const (BooleanConstant true); _}] when b.tdesc = b'.tdesc ->
        current_context, {e with Expr.tdesc = TE_op (Op_not, [b])}
      | [b; {Expr.tdesc = TE_op (Op_not, [b']); _}; b''] when b.tdesc = b'.tdesc && b.tdesc = b''.tdesc ->
        current_context, {e with Expr.tdesc = TE_const (BooleanConstant false)}
      | [b; {Expr.tdesc = TE_op (Op_not, [b']); _}; b''] when b.tdesc = b'.tdesc ->
        current_context, {e with Expr.tdesc = TE_op (Op_and, [{e with Expr.tdesc = TE_op (Op_not, [b])}; b''])}
      | [b; {Expr.tdesc = TE_const (BooleanConstant true); _}; b'] -> 
        current_context, {e with Expr.tdesc = TE_op (Op_or, [b; b'])}
      | [b; {Expr.tdesc = TE_const (BooleanConstant false); _}; b'] ->
        current_context, {e with Expr.tdesc = TE_op (Op_and, [{e with Expr.tdesc = TE_op (Op_not, [b])}; b'])}
      | [b; b'; {Expr.tdesc = TE_const (BooleanConstant true); _}] ->
        current_context, {e with Expr.tdesc = TE_op (Op_or, [{e with Expr.tdesc = TE_op (Op_not, [b])}; b'])}
      | [b; b'; {Expr.tdesc = TE_const (BooleanConstant false); _}] ->
        current_context, {e with Expr.tdesc = TE_op (Op_and, [b; b'])}
      | [op1; op2; op3] ->
          begin 
            match op1.Expr.tdesc with
            | (TE_ident _) | TE_op ((Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge | Op_not | Op_and | Op_or), _) ->
              let optrue = ({op2 with tdesc = TE_const (BooleanConstant true)}, {op2 with tdesc = TE_when (op2, "True", op1)} ) in 
              let opfalse = ({op3 with tdesc = TE_const (BooleanConstant false)}, {op3 with tdesc = TE_when (op3, "False", op1)} ) in
              normalize current_context ({e with Expr.tdesc = TE_merge (op1, [optrue; opfalse])})
           | _ -> failwith "Unreachable"
          end
      | _ -> failwith "Invalid number of operands for operator if"
      end

and handle_expression construct_expr expression_list e context =
  let (new_equations, new_variables), normalized_expression_list = normalize_list context expression_list in
  let pattern_declaration, pattern, expression = new_pattern e in
  let new_equation = {
    Equation.tpatt = pattern;
    Equation.texpr = {e with Expr.tdesc = construct_expr normalized_expression_list}
  } in
  (new_equation :: new_equations, pattern_declaration @ new_variables), expression

(* Main normalization function that recursively handles different expression types *)
and normalize context e =
  match e.Expr.tdesc with
  | TE_const _ | TE_ident _ -> context, e
  | TE_op (operator, operand_list) -> apply_operator context operator operand_list e
  | TE_app (function_name, expression_list) ->
    handle_expression (fun el -> TE_app (function_name, el)) expression_list e context
  | TE_prim (function_name, expression_list) ->
    handle_expression (fun el -> TE_prim (function_name, el)) expression_list e context
  | TE_tuple expression_list ->
    let context, normalized_expression_list = normalize_list context expression_list in
    context, {e with Expr.tdesc = TE_tuple normalized_expression_list}
  | TE_fby (expression1, expression2) ->
      let context, normalized_expression1 = normalize context expression1 in
      let (new_equations, new_variables), normalized_expression2 = normalize context expression2 in
      let pattern_declaration2, pattern2, expression2 = new_pattern normalized_expression2 in
      let equation2 = {Equation.tpatt = pattern2; Equation.texpr = normalized_expression2} in
      let pattern_declaration1, pattern1, expression1 = new_pattern e in
      let equation1 = {
        Equation.tpatt = pattern1;
        Equation.texpr = {e with Expr.tdesc = TE_fby (normalized_expression1, expression2)}
      } in
      (equation1::equation2::new_equations, pattern_declaration1@pattern_declaration2@new_variables), expression1
  | TE_merge (id, exp_list) -> 
    let context, norm_list = List.fold_left_map (fun ctx (constr, exp) ->
      let ctx, norm_exp = normalize ctx exp in
      ctx, (constr, norm_exp)) context exp_list in
    context, {e with Expr.tdesc = TE_merge (id, norm_list)}
  | TE_when (expression1, constructor, expression2) ->
      let context, normalized_expression1 = normalize context expression1 in
      let context, normalized_expression2 = normalize context expression2 in
      begin
        match normalized_expression2.Expr.tdesc with
        | TE_ident _ ->
          context, {e with Expr.tdesc = TE_when (normalized_expression1, constructor, normalized_expression2)}
        | _ ->
          let (new_equations, new_variables) = context in
          let pattern_declaration, pattern, new_expression = new_pattern normalized_expression2 in
          let new_equation = {Equation.tpatt = pattern; Equation.texpr = normalized_expression2} in
          (new_equation::new_equations, pattern_declaration@new_variables), {e with Expr.tdesc = TE_when (normalized_expression1, constructor, new_expression)}
      end
  | TE_reset (id, el, ex) ->
    let context, el' = normalize_list context el in
    let (new_eqs, new_vars), e' = normalize context ex in
    let x_decl, x_patt, x_expr = new_pattern e in
    let x_eq = {Equation.tpatt = x_patt;
                Equation.texpr = {e with Expr.tdesc = TE_reset (id, el', e')}} in
    (x_eq::new_eqs, x_decl@new_vars), x_expr
  | TE_pre _
  | TE_arrow _ -> failwith "Unreachable"

(* Normalize a list of expressions *)
and normalize_list current_context expression_list =
  List.fold_right (fun expression (current_context, normalized_list) ->
      let current_context, normalized_expression = normalize current_context expression in
      current_context, normalized_expression::normalized_list)
    expression_list
    (current_context, [])
    
(* Normalize the equations within a node *)
let normalize_equation_within_node node equation =
  let (new_equations, local_variables), normalized_expression = normalize ([], []) equation.Equation.texpr in
  let local_variables = List.map (fun variable -> variable, None) local_variables in
  {
    node with
    Node.tlocal = local_variables @ node.Node.tlocal;
    Node.teqs = {equation with Equation.texpr = normalized_expression}::(List.rev new_equations) @ node.Node.teqs;
  }

(* Normalize all nodes within a file *)
let file file =
  {file with File.tnodes = List.map (fun node ->
    List.fold_left normalize_equation_within_node {node with Node.teqs = []} node.Node.teqs |> fun normalized_node ->
    {normalized_node with Node.teqs = List.rev normalized_node.Node.teqs})
    file.File.tnodes}
