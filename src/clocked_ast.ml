open Asttypes

(* Type definitions to represent clock types and clock kinds *)
type ct =
  | Ck of ck
  | Cprod of ct list

and ck =
  | Cbase
  | Cvar of link ref
  | Con of ck * string * Ident.t

and link =
  | Cindex of int
  | Clink of ck

(* Module for clocked variables *)
module CVar = struct
  type t = Ident.t * basicType * ck  (* variable name, type, and clock *)
end

(* Module for clocked expressions *)
module CExpr = struct
  type t = {
    cdesc: desc;  (* description of the expression *)
    ctype: typeList;  (* type of the expression *)
    cclock: ct;  (* clock of the expression *)
    cloc: positionRange;  (* location of the expression *)
  }

  and desc =
    | CE_const of const
    | CE_ident of Ident.t
    | CE_op of operator * t list
    | CE_app of Ident.t * t list
    | CE_prim of Ident.t * t list
    | CE_pre of t
    | CE_tuple of t list
    | CE_merge of t * (t * t) list
    | CE_fby of t * t
    | CE_when of t * string * t
    | CE_reset of Ident.t * t list * t
end

(* Module for clocked patterns *)
module CPatt = struct
  type t = {
    cdesc: Ident.t list;  (* description of the pattern *)
    ctype: typeList;  (* type of the pattern *)
    cclock: ct;  (* clock of the pattern *)
    cloc: positionRange;  (* location of the pattern *)
  }
end

(* Module for clocked equations *)
module CEquation = struct
  type t = {
    cpattern: CPatt.t;  (* pattern of the equation *)
    cexpr: CExpr.t;  (* expression of the equation *)
  }
end

(* Module for clocked nodes *)
module CNode = struct
  type t = {
    name: Ident.t;  (* name of the node *)
    input: CVar.t list;  (* input variables of the node *)
    output: CVar.t list;  (* output variables of the node *)
    local: (CVar.t * const option) list;  (* local variables of the node *)
    equs: CEquation.t list;  (* equations of the node *)
    loc: positionRange;  (* location of the node *)
  }
end

(* Module for clocked files *)
module CFile = struct
  type t = {
    nodes : CNode.t list;  (* list of nodes in the file *)
    types : enumeratedDataType list  (* list of types in the file *)
  }
end