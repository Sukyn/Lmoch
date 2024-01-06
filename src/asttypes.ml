(* AST Types for the language *)

(* Position range in the source code represented by two Lexing positions *)
type positionRange = Lexing.position * Lexing.position

(* Basic types in the language *)
type basicType =
  | BooleanType                (* Represents a boolean type *)
  | IntegerType                (* Represents an integer type *)
  | RealType                   (* Represents a real number type *)
  | EnumeratedDataType of string  (* Represents an enumerated data type *)

(* List of basic types *)
type typeList = basicType list

(* Abstract data type with a name and constructors for enumerated and record types *)
type enumeratedDataType = {
  name: string;
  constructors: string list;
}

(* Constants in the language *)
type const =
  | BooleanConstant of bool    (* Boolean constant *)
  | IntegerConstant of int     (* Integer constant *)
  | RealConstant of float      (* Real number constant *)
  | EnumeratedDataConstant of string * string option  (* Enumerated data constant *)

(* Operators in the language *)
type operator =
  | Op_eq | Op_neq                     (* Equality and inequality *)
  | Op_lt | Op_le | Op_gt | Op_ge      (* Comparison operators: <, <=, >, >= *)
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod  (* Arithmetic operators: +, -, *, /, mod *)
  | Op_not                              (* Logical NOT *)
  | Op_and | Op_or | Op_impl            (* Logical AND, OR, IMPLIES *)
  | Op_if                               (* Conditional operator *)

(* End of AST Type Definitions *)
