(* Abstract syntax trees with types *)

open Asttypes

(* A module for typed variables *)
module TypedVar = struct
  (* A typed variable consists of an identifier and a basic type *)
  type t = Ident.t * basicType
end

(* A module for expressions *)
module Expr = struct
  (* An expression consists of a description, a type list, and a position range *)
  type t =
    { tdesc: desc;
      ttype:  typeList;
      tloc: positionRange; }

  (* Description of an expression *)
  and desc =
    | TE_const of const
    | TE_ident of Ident.t
    | TE_op of operator * t list
    | TE_app of Ident.t * t list
    | TE_prim of Ident.t * t list
    | TE_arrow of t * t
    | TE_pre of t
    | TE_tuple of t list
    | TE_merge of t * (t * t) list
    | TE_fby of t * t
    | TE_when of t * string * t
    | TE_reset of Ident.t * t list * t
end

(* A module for patterns *)
module Patt = struct
  (* A pattern consists of a description, a type list, and a position range *)
  type t =
    { tdesc: Ident.t list;
      ttype: typeList;
      tloc: positionRange; }
end

(* A module for equations *)
module Equation = struct
  (* An equation consists of a pattern and an expression *)
  type t = { tpatt: Patt.t; texpr: Expr.t; }
end

(* A module for nodes *)
module Node = struct
  (* A node consists of a name, input variables, output variables, local variables, equations, and a position range *)
  type t =
    { tname: Ident.t;
      tinput: TypedVar.t list;
      toutput: TypedVar.t list;
      tlocal: (TypedVar.t * const option) list;
      teqs: Equation.t list;
      tloc: positionRange; }
end

(* A module for files *)
module File = struct
  (* A file consists of a list of nodes and a list of enumerated data types *)
  type t =
  { tnodes: Node.t list;
    ttypes: enumeratedDataType list; }
end