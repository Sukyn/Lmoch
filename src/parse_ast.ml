(* Abstract Syntax Trees *)

open Asttypes

(* Type for identifiers *)
type ident = string

(* Module for parsed expressions *)
module PExpr = struct
  (* Parsed expression description and location *)
  type t = {
    desc: desc;
    loc: positionRange;
  }

  (* Forms of expression descriptions *)
  and desc =
    | Const of const
    | Ident of ident
    | Op of operator * t list
    | App of ident * t list
    | Arrow of t * t
    | Pre of t
    | Tuple of t list
    | Merge of t * (t * t) list
    | When of t * string * t
    | Reset of ident * t list * t
end

(* Module for parsed patterns *)
module PPattern = struct
  (* Parsed pattern description and location *)
  type t = {
    desc: desc;
    loc: positionRange;
  }

  (* Forms of pattern descriptions *)
  and desc =
    | Ident of ident
    | Tuple of ident list
end

(* Module for parsed equations *)
module PEq = struct
  (* Parsed equation: pattern and expression *)
  type t = {
    pattern: PPattern.t;
    expr: PExpr.t;
  }
end

(* Forms of parsed equations *)
type parsed_eq_type =
  | Eq of PEq.t
  | Automaton of parsed_autom_type
  | Match of PExpr.t * parsed_case_autom_type list


(* Parsed automaton: list of cores and location *)
and parsed_autom_type = {
  core_automaton: parsed_autom_core_type list;
  loc_automaton: positionRange;
}

(* Parsed automaton core: case, conditions, outputs, and weak indicator *)
and parsed_autom_core_type = {
  case_automaton: parsed_case_autom_type;
  cond_automaton: PExpr.t list;
  out_automaton: string list;
}

(* Parsed case: constructor, equation, and location *)
and parsed_case_autom_type = {
  case_type: string;
  case_equation: parsed_eq_type;
  case_location: positionRange;
}

(* Parsed node: name, inputs, outputs, local variables, equations, and location *)
type parsed_node_type = {
  node_name: ident;
  node_inputs: (ident * basicType) list;
  node_outputs: (ident * basicType) list;
  node_localvars: (ident * basicType) list;
  node_equs: parsed_eq_type list;
  node_location: positionRange;
}

(* Parsed file: types, nodes *)
type parsed_file_type = {
  additional_types: enumeratedDataType list;
  nodes: parsed_node_type list;
}
