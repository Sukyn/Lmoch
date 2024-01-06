open Asttypes
open Clocked_ast

(* A module for atoms *)
module Atom = struct
  (* An atom can be a constant or an identifier *)
  type t =
    | Const of const  (* Represents a constant value *)
    | Ident of Ident.t  (* Represents an identifier *)
end

(* A module for cvars *)
module CVar = struct
  (* A cvar consists of an identifier, a basic type, and a clock *)
  type t = Ident.t * basicType * ck
end

(* A module for memory *)
module Mem = struct
  (* A memory consists of a list of cvars for 'fby' and a list of identifier pairs for 'node_mem' *)
  type t = {
    fby: CVar.t list;  (* Represents a list of cvars for 'fby' *)
    node_mem: (Ident.t * Ident.t) list  (* Represents a list of identifier pairs for 'node_mem' *)
  }
end

(* A module for initialization *)
module Init = struct
  (* An initialization consists of a list of identifier-atom pairs for 'fby' and a list of identifier pairs for 'node' *)
  type t = {
    fby: (Ident.t * Atom.t) list;  (* Represents a list of identifier-atom pairs for 'fby' *)
    node: (Ident.t * Ident.t) list;  (* Represents a list of identifier pairs for 'node' *)
  }
end

(* A module for expressions *)
module IExpr = struct
  (* An expression consists of a description and a list of basic types *)
  type t = {
    idesc: desc;  (* Represents the description of the expression *)
    itype: basicType list;  (* Represents the list of basic types of the expression *)
  }

  (* Description of an expression can be one of the following *)
  and desc =
    | IE_const of const  (* Represents a constant expression *)
    | IE_ident of Ident.t  (* Represents an identifier expression *)
    | IE_mem of Ident.t  (* Represents a memory expression *)
    | IE_op of operator * t list  (* Represents an operator expression *)
    | IE_app of Ident.t * Ident.t * t list  (* Represents an application expression *)
    | IE_prim of Ident.t * t list  (* Represents a primitive expression *)
    | IE_tuple of t list  (* Represents a tuple expression *)
    | IE_case of t * (t * t) list  (* Represents a case expression *)
    | IE_reset of Ident.t * Ident.t * t list * t  (* Represents a reset expression *)
end

(* A module for equations *)
module IEquation = struct
  (* An equation consists of a pattern and an expression *)
  type t = {
    pattern: CVar.t list;  (* Represents the pattern of the equation *)
    expr: IExpr.t;  (* Represents the expression of the equation *)
  }
end

(* A module for nodes *)
module INode = struct
  (* A node consists of a name, input step, output step, local variables, memory, initialization, computation, update, and a boolean indicating if memory is needed *)
  type t = {
    name: Ident.t;  (* Represents the name of the node *)
    input_step: CVar.t list;  (* Represents the input step of the node *)
    output_step: CVar.t list;  (* Represents the output step of the node *)
    local: (CVar.t * const option) list;  (* Represents the local variables of the node *)
    mem: Mem.t;  (* Represents the memory of the node *)
    init: Init.t;  (* Represents the initialization of the node *)
    compute: IEquation.t list;  (* Represents the computation of the node *)
    update: (Ident.t * Atom.t) list;  (* Represents the update of the node *)
    need_mem: bool;  (* Represents if the node needs memory *)
  }
end

(* A module for files *)
module IFile = struct
  (* A file consists of a list of nodes and a list of enumerated data types *)
  type t = {
    nodes: INode.t list;  (* Represents the list of nodes in the file *)
    types: enumeratedDataType list  (* Represents the list of enumerated data types in the file *)
  }
end