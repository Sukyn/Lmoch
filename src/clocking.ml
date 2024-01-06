open Asttypes
open Typed_ast
open Clocked_ast
open Ident
open Format

(* Exception for handling unification failures *)
exception Unify

(* Index generation for unique identification *)
let index = ref 0
let gen_index () = incr index; !index

(* Function to generate a fresh clock kind *)
let fresh_ck () = Cvar (ref (Cindex (gen_index ())))

module S = Set.Make(Ident)
module M = Map.Make(Ident)

type raiseError =
  | Clash of Ident.t
  | ExpectedBaseClock of ct
  | ExpectedClock of ct * ct
  | Other of string
  | UnboundVar of Ident.t
  | Unreachable

exception Error of positionRange * raiseError

let raiseError loc e = raise (Error (loc, e))
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

type varType = InputVar | LocalVar | OutputVar

module Delta = struct
  let nodes = Hashtbl.create 97
  let find node = Hashtbl.find nodes node.name
  let add node' node_type = Hashtbl.replace nodes node'.name (node', node_type); node'
end

module Gamma = struct
  type t = ck M.t
  let empty = M.empty
  let add loc env id ck = if M.mem id env then raiseError loc (Clash id); M.add id ck env
  let adds loc var_type = List.fold_left (fun env (id,_) -> add loc env id (match var_type with
    | LocalVar | InputVar -> Cbase
    | OutputVar -> fresh_ck ()))
  let find loc env id = try M.find id env with Not_found -> raiseError loc (UnboundVar id)
end

let rec occur_check index = function
  | Cbase -> ()
  | Cvar { contents = Cindex n } when index <> n -> ()
  | Con (ck, _, _) -> occur_check index ck
  | _ -> raise Unify

let rec ck_repr = function
  | Cbase | Con _ | Cvar { contents = Cindex _ } as ck -> ck
  | Cvar ({ contents = Clink ck } as link) ->
      let ck' = ck_repr ck in
      link.contents <- Clink ck';
      ck'

let rec unify_ck ck1 ck2 =
  let ck1' = ck_repr ck1 in
  let ck2' = ck_repr ck2 in
  if ck1' == ck2' then ()
  else match (ck1', ck2') with
    | Cbase, Cbase -> ()
    | Cvar { contents = Cindex n1 }, Cvar { contents = Cindex n2 } when n1 = n2 -> ()
    | Con (ck1, c1, n1), Con (ck2, c2, n2) when c1 = c2 && n1 = n2 -> unify_ck ck1 ck2
    | Cvar ({ contents = Cindex n } as v), ck | ck, Cvar ({ contents = Cindex n } as v) ->
        occur_check n ck;
        v.contents <- Clink ck
    | _ -> raise Unify

(* Function to unify two clock types *)
let rec unify t1 t2 =
  if t1 == t2 then ()
  else match (t1, t2) with
    | Ck (Cbase | Cvar { contents = Cindex _ }), Cprod []
    | Cprod [], Ck (Cbase | Cvar { contents = Cindex _ }) -> ()
    | Ck ck1, Ck ck2 -> unify_ck ck1 ck2
    | Cprod t1_list, Cprod t2_list -> unify_list t1_list t2_list
    | _ -> raise Unify

(* Helper function to unify lists of clock types *)
and unify_list t1_list =
  try List.iter2 unify t1_list
  with Invalid_argument _ -> raise Unify


let rec pretty_print_clock fmt = function
  | Cbase -> fprintf fmt "Base"
  | Con (c, s, id) -> fprintf fmt "@[%a on %s(%a)@]" pretty_print_clock c s Ident.print id
  | Cvar {contents = Clink _} -> fprintf fmt "Linked clock"
  | Cvar {contents = Cindex i} -> fprintf fmt "@[ck%i@]" i

let rec pretty_print_clock_type fmt = function
  | Ck ck -> fprintf fmt "%a" pretty_print_clock ck
  | Cprod clock_list -> fprintf fmt "(%a)" (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt " * ") pretty_print_clock_type) clock_list

let reportErrorFormat fmt = function
  | Clash id -> fprintf fmt "The variable %a is defined multiple times in the same scope" Ident.print id
  | ExpectedBaseClock cl -> fprintf fmt "A base clock was expected, but the actual clock is %a" pretty_print_clock_type cl
  | ExpectedClock (c1, c2) -> fprintf fmt "The expected clock is %a, but the actual clock is %a" pretty_print_clock_type c1 pretty_print_clock_type c2
  | Other s -> fprintf fmt "An error occurred: %s" s
  | UnboundVar id -> fprintf fmt "The variable %a is not defined in the current scope" Ident.print id
  | Unreachable -> fprintf fmt "An unreachable code block was found"

let unify_clock_list types clockList =
  let rec aux baseClock constructors = function
    | Ck (Con (clock, branch, _)) :: remainingClocks ->
      unify_ck clock baseClock;
      if List.mem branch constructors then aux baseClock (List.filter ((<>) branch) constructors) remainingClocks else raiseError dummy_loc (Other "Cannot merge clocks")
    | [] -> ()
    | _ -> assert false
  in
  match clockList with
  | Ck Cbase :: _ -> Cbase
  | Ck (Con _) :: _ as clocks ->
    let (baseClock, constructor) = match List.hd clocks with Ck (Con (clock, constructor, _)) -> clock, constructor | _ -> assert false in
    let clockType = List.find (fun {constructors; _} -> List.mem constructor constructors) types in
    aux baseClock clockType.constructors clocks;
    baseClock
  | _ -> raiseError dummy_loc (Other "Cannot merge clocks")

let rec find_base_ck = function
  | Ck ck -> ck
  | Cprod [] -> assert false
  | Cprod (ct::_) -> find_base_ck ct

let rec clock_expression env types expression = 
  let loc = expression.Expr.tloc in 
  
  let description, clock = 
    match expression.Expr.tdesc with 
    | TE_const constant -> CExpr.CE_const constant, Ck (fresh_ck ())
    | TE_ident identifier -> CE_ident identifier, Ck (Gamma.find loc env identifier)
    | TE_op (operator, exprList) -> let exprs = List.map (clock_expression env types) exprList in
                                    let clocks = List.map (fun ce -> ce.CExpr.cclock) exprs in
                                    (match clocks with
                                    | c1::cs -> 
                                        List.iter (fun c2 -> 
                                          try unify c1 c2 with Unify ->
                                            raiseError loc (ExpectedClock (c1, c2))) cs
                                    | _ -> ());
                                    CE_op (operator, exprs), Ck (find_base_ck (List.hd clocks)) 
    | TE_prim (primitiveName, expressionList) -> 
      let clockedExpressionList = List.map (clock_expression env types) expressionList in
      CE_prim (primitiveName, clockedExpressionList), Cprod (List.map (fun clockedExpression -> clockedExpression.CExpr.cclock) clockedExpressionList)
    | TE_app (functionName, expressionList) ->
      let (functionDetails, (inputClock, outputClock)) = Delta.find functionName in
      let clockedExpressionList = clock_args env types loc inputClock expressionList in
      CE_app (functionDetails, clockedExpressionList), outputClock
    | TE_tuple expressionList ->
      let clockedExpressionList = List.map (clock_expression env types) expressionList in
      CE_tuple clockedExpressionList, Cprod (List.map (fun clockedExpression -> clockedExpression.CExpr.cclock) clockedExpressionList)
    | TE_merge (id, tes) ->
      let clockedId = clock_expression env types id in
      let clockedExprList = List.map (fun (left, right) -> (clock_expression env types left, clock_expression env types right)) tes in
      let clocks = List.map (fun (_, clockedExpr) -> clockedExpr.CExpr.cclock) clockedExprList in
      CE_merge (clockedId, clockedExprList), Ck (unify_clock_list types clocks)
    | TE_fby (expression1, expression2) ->
      let clockedExpression1 = clock_expression env types expression1 in
      let clockedExpression2 = clock_expression env types expression2 in
      CE_fby (clockedExpression1, clockedExpression2), clockedExpression1.CExpr.cclock
    | TE_when (expression, branch, ({Expr.tdesc = TE_ident identifier; _} as identifierExpression)) ->
      let clock = Gamma.find loc env identifier in
      let clockedExpression = clock_expression env types expression in
      let clockedIdentifierExpression = clock_expression env types identifierExpression in
      (match clockedExpression.CExpr.cclock with
      | Ck clockedExpressionClock -> (try unify_ck clock clockedExpressionClock with Unify ->
          raiseError loc (ExpectedClock (Ck clock, clockedExpression.CExpr.cclock)));
        CE_when (clockedExpression, branch, clockedIdentifierExpression), Ck (Con (clock, branch, identifier))
      | _ as clockType -> raiseError loc (ExpectedBaseClock clockType))
    | TE_when _ -> failwith "Expression is not normalized"
    | TE_reset (functionName, expressionList, expression) ->
      let (functionDetails, (inputClock, outputClock)) = Delta.find functionName in
      let clockedExpressionList = clock_args env types loc inputClock expressionList in
      let clockedExpression = clock_expression env types expression in
      CE_reset (functionDetails, clockedExpressionList, clockedExpression),
      (match clockedExpressionList, clockedExpression with
      | [], _ -> outputClock
      | {CExpr.cclock = Ck clock1; _}::_, {CExpr.cclock = Ck clock2; _} -> (try unify_ck clock1 clock2 with Unify ->
          raiseError loc (ExpectedClock (Ck clock1, Ck clock2)));
        outputClock
      | _ -> assert false)
    | TE_pre _  | TE_arrow _ -> raiseError loc Unreachable
  in
  { CExpr.cdesc = description; ctype = expression.ttype; cclock = clock; cloc = loc; }
  
and clock_args env types loc paramsClock expressionList =
  let clockedExpressionList = List.map (clock_expression env types) expressionList in
  let actualClocks = match clockedExpressionList with
    | [] -> Cprod []
    | [{CExpr.cclock = singleClock; _}] -> singleClock
    | _ -> Cprod (List.map (fun {CExpr.cclock = clock; _} -> clock) clockedExpressionList)
  in
  (try unify paramsClock actualClocks with Unify ->
    raiseError loc (ExpectedClock (paramsClock, actualClocks)));
  clockedExpressionList
  
let unify_expression_clock baseClock env patternExpression pattern =
  let expressionClock = Gamma.find dummy_loc env patternExpression in
  try unify_ck baseClock expressionClock
  with Unify ->
    raiseError pattern.Patt.tloc (ExpectedClock (Ck expressionClock, Ck baseClock))

let rec clock_patt env pattern clock =
  let () = match clock with
  | Ck baseClock ->
      begin match pattern.Patt.tdesc with
      | [expression] -> unify_expression_clock baseClock env expression pattern
      | _ -> failwith "Expected a single expression in pattern"
      end
  | Cprod clockList ->
      List.iter2 (fun patternExpression -> function
        | Ck baseClock -> unify_expression_clock baseClock env patternExpression pattern
        | _ -> failwith "Nested tuples are not supported currently") pattern.Patt.tdesc clockList
  in
  {CPatt.cdesc = pattern.Patt.tdesc; CPatt.ctype = pattern.Patt.ttype; CPatt.cclock = clock; CPatt.cloc = pattern.Patt.tloc; },
  env
  

let clock_equation env types equation =
  let clockedExpression = clock_expression env types equation.Equation.texpr in
  let clockedPattern, updatedEnv = clock_patt env equation.tpatt clockedExpression.cclock in
  updatedEnv, {CEquation.cpattern = clockedPattern; cexpr = clockedExpression }

let rec find_clock_kind ck = match ck_repr ck with
  | Cbase
  | Cvar { contents = Cindex _ } -> ck
  | Con(ck,_,_) -> find_clock_kind ck
  | Cvar { contents = Clink _ } -> failwith "Unexpected clock representation: Clink found in find_clock_kind function"

let clock_node types node =
  let complete_env = List.fold_left (fun env (key, kind) -> Gamma.adds node.Node.tloc kind env key) Gamma.empty
                  [((List.map fst node.tlocal), LocalVar); (node.tinput, InputVar); (node.toutput, OutputVar)] in
  
  let updated_env, clocked_equations =
    List.fold_left_map (fun env -> clock_equation env types) complete_env node.teqs
  in
  
  M.iter (fun _ clock -> unify_ck Cbase (find_clock_kind clock)) updated_env;

  let get_clocks sets =
    match (List.map (fun (id, _) -> Gamma.find node.tloc updated_env id) sets) with 
    | [clock] -> Ck clock
    | clock_list -> Cprod (List.rev_map (fun clock -> Ck clock) clock_list)
  in

  let convert_sets sets =
    List.map (fun (id, typeList) -> (id, typeList, Gamma.find node.tloc updated_env id)) sets
  in

  {
    CNode.name = Delta.add node.tname (get_clocks node.tinput, get_clocks node.toutput);
    CNode.input = convert_sets node.tinput;
    CNode.output = convert_sets node.toutput;
    CNode.local = List.map (fun ((id, typeList), v) -> ((id, typeList, Gamma.find node.tloc updated_env id), v)) node.tlocal;
    CNode.equs = clocked_equations;
    CNode.loc = node.tloc;
  }

let clock_file file main =
  let clockedNodes = List.map (clock_node file.File.ttypes) file.tnodes in
  { CFile.nodes = clockedNodes; types = file.ttypes }
