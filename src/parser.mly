%{
open Asttypes
open Parse_ast

exception RedefinitionError of positionRange * string

(* Helper functions to create expressions and patterns *)
let make_expr descriptor location = { PExpr.desc = descriptor; PExpr.loc = location; }
let make_pattern descriptor location = { PPattern.desc = descriptor; PPattern.loc = location }
let make_expr_with_operator operator expressions location = make_expr (Op (operator, expressions)) location

(* Hashtable to track type and constructor mappings *)
(* A small prime number is chosen for the hashtable size, as we don't expect large numbers of types or constructors *)
let constructorToTypeMap = Hashtbl.create 11
let typeToConstructorMap = Hashtbl.create 11
let nodeMap = Hashtbl.create 11

(* Function to check for existing keys in a hashtable and add new key-value pairs *)
let generate_error_message key = Printf.sprintf "Redefinition error: %s is already defined." key

let check_and_add_to_map hashtable key value location =
    if Hashtbl.mem hashtable key then
        raise (RedefinitionError (location, generate_error_message key))
    else
        Hashtbl.add hashtable key value

%}
(* Token declarations *)
%token <bool> CONST_BOOL
%token <int> CONST_INT
%token <float> CONST_REAL
%token <string> IDENT
%token BOOL, REAL, INT
%token AND, OR, NOT, IMPL
%token EQUAL, NEQ
%token MINUS, PLUS, DIV, MOD, STAR
%token ARROW
%token BAR
%token COLON, SEMICOL, COMMA
%token <Asttypes.operator> COMP
%token IF, THEN, ELSE
%token END
%token EOF
%token <string * string> CONST_ENUMERATED
%token NODE
%token LET, TEL
%token LPAREN, RPAREN
%token PRE, FBY
%token RETURNS
%token SLASH
%token VAR
%token MERGE, WHEN, WHENOT
%token EVERY, RESET
%token AUTOMATON
%token UNTIL, CONTINUE
%token TYPE
%token <string> CONSTR

(* Precedence and associativity declarations *)
%nonassoc ELSE
%right ARROW
%left IMPL
%left OR
%left AND
%right WHEN WHENOT
%left COMP EQUAL NEQ
%left PLUS MINUS
%left STAR SLASH DIV MOD
%nonassoc NOT PRE EVERY
%right FBY

(* Start symbol and production rules *)
%start file
%type <Parse_ast.parsed_file_type> file

%%

(* Production rules for file, type declarations, and node declarations *)
(* Detailed comments are provided above each rule for clarity *)

(* File production rule *)
file: type_decs node_decs EOF
  { { additional_types = $1 ; nodes = $2} }
;

(* Type declarations *)
type_decs:
| /* empty */       { [] }
| new_type type_decs    { $1 :: $2 }
;


(* Type declaration *)
new_type:
| TYPE IDENT EQUAL BAR? separated_list(BAR, CONSTR)
  { check_and_add_to_map typeToConstructorMap $2 $5 $sloc;
    List.iter (fun e ->
       check_and_add_to_map constructorToTypeMap  e $2 $sloc) $5;
    {name = $2; constructors = $5} }
;

(* Node declarations *)
node_decs:
| /* empty */       { [] }
| node node_decs    { $1 :: $2 }
;


(* Node declaration rule. This rule parses a node declaration, which consists of 
   the keyword NODE, an identifier, a list of input parameters, the keyword RETURNS, 
   a list of output parameters, a list of local parameters, and a list of equations. *)
node:
| NODE IDENT LPAREN in_params RPAREN
  RETURNS LPAREN out_params RPAREN SEMICOL
  local_params
  LET eq_list TEL semi_opt
    {  if Hashtbl.mem nodeMap $2 then raise (RedefinitionError ($sloc, $2));
      Hashtbl.add nodeMap $2 ();
    { 
    node_name = $2;
    node_inputs = $4;
    node_outputs = $8;
    node_localvars = $11;
    node_equs = $13;
    node_location = $sloc; } }
;


(* Parameter lists *)
in_params: 
| /* empty */ { [] }
| param_list { $1 }
;

out_params:
| /* empty */ { [] }
| param_list { $1 }
;

local_params:
| /* empty */ { [] }
| VAR param_semicol_list { $2 }
;

param_with_semicol:
| ident_comma_list COLON typ SEMICOL
    { let typ = $3 in
      List.map (fun id -> (id, typ)) $1 }
;

param_list:
| param_with_semicol param_list
    { $1 @ $2 }
| ident_comma_list COLON typ
    { let typ = $3 in
      List.map (fun id -> (id, typ)) $1 }
;

param_semicol_list:
| param_with_semicol param_semicol_list
    { $1 @ $2 }
| param_with_semicol
    { $1 }
;

(* Equation lists *)
eq_list:
| eq eq_list { $1 :: $2 }
| eq { [$1] }
;

eq:
| pattern EQUAL expr SEMICOL
    { Eq { pattern = $1; expr = $3; } }
| AUTOMATON list(case_autom) END semi_opt
    { Automaton ({core_automaton = $2; loc_automaton = $sloc}) }
;

case_autom:
| BAR CONSTR ARROW eq outlist
  { let cond, out = $5 in
    { case_automaton = {case_type = $2; case_equation = $4; case_location = $sloc}; 
      cond_automaton=cond; 
      out_automaton=out}  }
;

outlist:
| { [], [] }
| UNTIL expr CONTINUE CONSTR outlist
  { let cond, out = $5 in
    $2 :: cond, $4 :: out}
;


(* Patterns *)
pattern:
| IDENT
    { make_pattern (PPattern.Ident $1) $sloc}
| LPAREN IDENT RPAREN
    { make_pattern (PPattern.Ident $2) $sloc}
| LPAREN IDENT COMMA ident_comma_list RPAREN
    { make_pattern (PPattern.Tuple($2::$4)) $sloc}
;



expr:
| LPAREN expr RPAREN
    { $2 }
| const
    { make_expr (Const $1) $sloc }
| IDENT
    { make_expr (Ident $1) $sloc }
| IDENT LPAREN expr_comma_list RPAREN
    { make_expr (App ($1, $3)) $sloc}
| IF expr THEN expr ELSE expr
    { make_expr_with_operator Op_if [$2; $4; $6] $sloc}
| expr PLUS expr
    { make_expr_with_operator Op_add [$1; $3] $sloc}
| expr MINUS expr
    { make_expr_with_operator Op_sub [$1; $3] $sloc}
| expr STAR expr
    { make_expr_with_operator Op_mul [$1; $3] $sloc}
| expr SLASH expr
    { make_expr_with_operator Op_div [$1; $3] $sloc}
| expr DIV expr
    { make_expr_with_operator Op_div [$1; $3] $sloc}
| expr MOD expr
    { make_expr_with_operator Op_mod [$1; $3] $sloc}
| expr COMP expr
    { make_expr_with_operator $2 [$1; $3] $sloc}
| expr EQUAL expr
    { make_expr_with_operator Op_eq [$1; $3] $sloc}
| expr NEQ expr
    { make_expr_with_operator Op_neq [$1; $3] $sloc}
| expr AND expr
    { make_expr_with_operator Op_and [$1; $3] $sloc}
| expr OR expr
    { make_expr_with_operator Op_or [$1; $3] $sloc}
| expr IMPL expr
    { make_expr_with_operator Op_impl [$1; $3] $sloc}
| expr ARROW expr
    { make_expr (Arrow ($1, $3))  $sloc}
| expr FBY expr
    { make_expr (Arrow ($1, make_expr (Pre ($3)) $sloc))  $sloc}
| MINUS expr 
    { make_expr_with_operator Op_sub [$2] $sloc}
| NOT expr
    { make_expr_with_operator Op_not [$2] $sloc}
| PRE expr
    { make_expr (Pre ($2))  $sloc}
| LPAREN expr COMMA expr_comma_tail RPAREN
    { make_expr (Tuple ($2::$4))  $sloc}
| MERGE IDENT list(merge_branche)
    { let ident = make_expr (Ident $2) $loc($2) in
      make_expr (Merge (ident, $3)) $sloc }
(* When with Enumerated types *)
| expr WHEN CONSTR LPAREN expr RPAREN
    { make_expr (When ($1, $3, $5)) $sloc }
(* Basic When & Whenot : Boolean types *)
| expr WHEN expr
    { make_expr (When ($1, "True", $3)) $sloc }
| expr WHENOT expr
    { make_expr (When ($1, "False", $3)) $sloc }
| RESET IDENT LPAREN expr_comma_list RPAREN EVERY expr
    { make_expr (Reset ($2, $4, $7)) $sloc }
;

merge_branche:
 LPAREN const ARROW expr RPAREN { (make_expr (Const $2) $loc($2), $4) }
;

(* Constants *)
const:
| CONST_BOOL
    { BooleanConstant $1 }
| CONST_INT
    { IntegerConstant $1 }
| CONST_REAL
    { RealConstant $1 }
| CONST_ENUMERATED
    { let condition, typ = $1 in
      EnumeratedDataConstant (typ, Some condition) }
| CONSTR
    { let typ = Hashtbl.find constructorToTypeMap  $1 in
      EnumeratedDataConstant (typ, Some $1) }
;

ident_comma_list:
| IDENT COMMA ident_comma_list
    { $1 :: $3 }
| IDENT { [$1] }
;

expr_comma_list:
|    { [] }
| expr expr_comma_tail { $1 :: $2 }
;

expr_comma_tail:
| COMMA expr expr_comma_tail
    { $2 :: $3 }
| { [] }
;

typ:
| BOOL   { BooleanType }
| INT    { IntegerType }
| REAL   { RealType }
| IDENT  { EnumeratedDataType $1 }
;

semi_opt:
    { () }
| SEMICOL
    { () }
;
