{ (* Dependencies and module openings *)
open Lexing
open Parser
open Asttypes
open Parse_ast

(* Exception for lexical errors *)
exception LexicalError of string

(* Constants *)
let keywordHashTableSize = 30  (* Size of the keyword hash table *)

(* Initialize the keyword hash table with language keywords *)
let initializeKeywordHashTable () =
  let table = Hashtbl.create keywordHashTableSize in
  let add_kw (kw, tkn) = Hashtbl.add table kw tkn in
  let keywords = [
        "and", AND; "automaton", AUTOMATON; "bool", BOOL; "continue", CONTINUE;
        "div", DIV; "else", ELSE; "end", END; "every", EVERY; "false", CONST_BOOL(false);
        "fby", FBY; "if", IF; "int", INT; "let", LET; "merge", MERGE; "mod", MOD;
        "node", NODE; "not", NOT; "or", OR; "pre", PRE; "real", REAL; "reset", RESET;
        "returns", RETURNS; "tel", TEL; "then", THEN; "true", CONST_BOOL(true);
        "type", TYPE; "until", UNTIL; "var", VAR; "when", WHEN; "whenot", WHENOT;
    ] in
  List.iter add_kw keywords;
  table

let keywordTable = initializeKeywordHashTable ()

(* Helper Functions *)
let identifyKeywordOrReturnIdentifier keyword =
  match Hashtbl.find_opt keywordTable keyword with
  | Some token -> token
  | None -> IDENT keyword

let updatePositionNewline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}
(* Character class definitions *)
let lowercaseAlpha = ['a'-'z']
let uppercaseAlpha = ['A'-'Z']
let alpha = lowercaseAlpha | uppercaseAlpha
let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let float = digit+ '.' digit* exponent? | digit* '.' digit+ exponent? | digit+ exponent

let identifier = lowercaseAlpha (alpha | '_' | digit)*
let constructors = uppercaseAlpha (alpha | '_' | digit)*
let typeConstructor = uppercaseAlpha (alpha | '_' | digit)* "'" identifier

(* Lexer rules *)
rule token = parse
    | '\n'  { updatePositionNewline lexbuf; token lexbuf }
    | [' ' '\t' '\r']+  { token lexbuf }
    | "--" [^ '\n']* '\n'  { updatePositionNewline lexbuf; token lexbuf }
    | "/*"  { comment lexbuf; token lexbuf }
    | identifier  { identifyKeywordOrReturnIdentifier (lexeme lexbuf) }
    | constructors  { CONSTR (lexeme lexbuf) }
    | typeConstructor  { 
          match String.split_on_char '\'' (lexeme lexbuf) with
          | [cons; typeList] -> CONST_ENUMERATED (cons, typeList)
          | _ -> assert false }
    | digit+  { CONST_INT (int_of_string (lexeme lexbuf)) }
    | float  { CONST_REAL (float_of_string (lexeme lexbuf)) }
    | "-"  { MINUS } 
    | "+"  { PLUS }
    | "*"  { STAR }
    | "/"  { SLASH }
    | ">"  { COMP Op_gt }
    | ">="  { COMP Op_ge }
    | "<"  { COMP Op_lt }
    | "<="  { COMP Op_le }
    | "<>"  { NEQ }
    | "=>"  { IMPL }
    | "->"  { ARROW }
    | "("  { LPAREN }
    | ")"  { RPAREN }
    | ":"  { COLON }
    | ";"  { SEMICOL }
    | "="  { EQUAL }
    | "|"  { BAR }
    | ","  { COMMA }
    | _    { raise (LexicalError (Printf.sprintf "Unexpected character '%s' at line %d, position %d. Expected a valid token." (lexeme lexbuf) lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))) }
    | eof  { EOF }
    
and comment = parse
  | "*/"  { () }
  | '\n'  { updatePositionNewline lexbuf; comment lexbuf }
  | _     { comment lexbuf }
  | eof   { raise (LexicalError (Printf.sprintf "Unterminated comment starting at line %d." lexbuf.lex_start_p.pos_lnum)) }
