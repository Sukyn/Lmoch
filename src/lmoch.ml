(* Main program *)

open Format
open Lexing
open Lexer
open Parser
open Parse_ast

let usage = Printf.sprintf "Usage: %s [options] file.lus [main]" Sys.argv.(0)

let parse_only = ref false
let automaton_only = ref false
let type_only = ref false
let clock_only = ref false
let normalize_only = ref false
let schedule_only = ref false
let imp_only = ref false
let c_only = ref false

let verbose = ref false
let no_sleep = ref false
let no_nl = ref false

let options = [
  ("-parse-only", Arg.Set parse_only, "Stops the process immediately after parsing the source code, providing a syntax tree.");
  ("-automaton-only", Arg.Set automaton_only, "Halts the process post compiling the automaton, suitable for analysis of state transitions.");
  ("-type-only", Arg.Set type_only, "Terminates after type checking, confirming data types without further compilation steps.");
  ("-clock-only", Arg.Set clock_only, "Ceases operation after clocking, which helps in analyzing the timing and sequencing of operations.");
  ("-norm-only", Arg.Set normalize_only, "Stops after normalization to simplify and standardize the code, making it easier to understand and optimize.");
  ("-sched-only", Arg.Set schedule_only, "Ends the process post scheduling, which is useful for understanding execution order and resource allocation.");
  ("-imp-only", Arg.Set imp_only, "Halts after the imperative translation, providing a view of the code in a more traditional imperative form.");
  ("-c-only", Arg.Set c_only, "Stops after converting to intermediate C representation, useful for checking how high-level constructs are translated.");
  ("-no-sleep", Arg.Set no_sleep, "Disables the sleep call in the C main file, often used to increase responsiveness or for real-time requirements.");
  ("-verbose", Arg.Set verbose, "Enables printing of intermediate transformations, offering insight into each compilation stage.");
  ("-v", Arg.Set verbose, "Alias for '-verbose', prints intermediate transformations for detailed process insight.")
]


(* Define a type to hold the file and main node information *)
type file_main = {
  file: string option;
  main: string option;
}

(* Initialize the file and main node with None *)
let file_main = ref {file = None; main = None}

(* Function to update the file and main node based on the arguments passed *)
let set_file_main arg =
  match !file_main with
  | {file = None; _} -> file_main := {!file_main with file = Some arg}
  | {main = None; _} -> file_main := {!file_main with main = Some arg}
  | _ -> raise (Arg.Bad "Too many arguments")

let file, main_node = 
  Arg.parse options set_file_main usage;
  (* Match the parsed file and main node to ensure both are set *)
  match !file_main with
  | {file = Some f; main = Some m} -> (f, m)  (* Return the file and main node *)
  | _ -> 
      (* Display usage information and exit if not both arguments are provided *)
      Arg.usage options usage; 
      exit 1

(* Function to report the location of an issue using positionRange *)
let report_loc (file: string) ((b, e): Asttypes.positionRange) =
  (* Function to calculate the character position in a line *)
  let char_position pos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in

  if b.Lexing.pos_lnum > 0 && e.Lexing.pos_lnum > 0 then
    (* Print the formatted message with calculated character positions *)
    eprintf "File \"%s\", line %d, characters %d-%d:\n"
      file b.Lexing.pos_lnum (char_position b) (char_position e)
  else
    (* Handle invalid position data *)
    eprintf "File \"%s\": Invalid location data provided.\n" file


(* Function to parse a file and update the AST *)
let parse_file channel lexer_buffer =
  (* Parse the file using the provided lexer and parser *)
  let parsed_file = Parser.file Lexer.token lexer_buffer in
  
  (* Define the inductive bool type *)
  let inductive_bool = Asttypes.{
    name = "inductive_bool";
    constructors = ["False"; "True"]
  } in

  (* Update the parsed file with the additional inductive_bool type *)
  let updated_file = Parse_ast.{
    parsed_file with
    additional_types = inductive_bool :: parsed_file.additional_types
  } in

  (* Close the input channel *)
  close_in_noerr channel;

  (* Exit if only parsing is required *)
  if !parse_only then exit 0;

  (* Return the updated file *)
  updated_file

(* Function to print verbose messages *)
let print_verbose_message verbose_msg =
  let border = "/**************************************/" in
  let formatted_msg = Printf.sprintf "/* %s */" verbose_msg in
  [border; formatted_msg; border]
  |> List.map (Format.sprintf "%s@.")

let handle_exception lb = function
  | LexicalError s -> report_loc file (lexeme_start_p lb, lexeme_end_p lb); printf "%s%s[Error 1001] Lexical error: %s%s\n@." "\027[31m" "\027[1m" "\027[0m" s; exit 0
  | Parser.Error -> report_loc file (lexeme_start_p lb, lexeme_end_p lb); printf "%s%s[Error 1002] Syntax error%s\n@." "\027[31m" "\027[1m" "\027[0m"; exit 0
  | Clean_automaton.Error (l, e) -> printf "%s%s[Error 1003] Sugar syntax error: %s%a\n@." "\027[31m" "\027[1m" "\027[0m" Clean_automaton.reportErrorFormat e; exit 0
  | Typing.Error(l,e) -> report_loc file l; printf "%s%s[Error 1004] Typing error: %s%a\n@." "\027[31m" "\027[1m" "\027[0m" Typing.reportErrorFormat e; exit 0
  | Clocking.Error (l, e) -> report_loc file l; printf "%s%s[Error 1005] Clocking error: %s@?@[%a@]\n@." "\027[31m" "\027[1m" "\027[0m" Clocking.reportErrorFormat e; exit 0
  | Scheduling.Error (e) -> printf "%s%s[Error 1006] Scheduling error: %s@?@[%a@]\n@." "\027[31m" "\027[1m" "\027[0m" Scheduling.reportErrorFormat e; exit 0
  | e -> printf "%s%s[Error 1000] Anomaly:%s %s\n@." "\027[31m" "\027[1m" "\027[0m" (Printexc.to_string e); exit 0

(* Compil_step function: Executes a step in the compilation process *)
let compile_step process_step flag verbose_msg printer =
  (* Execute the provided function and store the result *)
  let result = process_step () in

  (* Print verbose messages if the verbose flag is set *)
  if !verbose then begin
    List.iter (printf "%s") (print_verbose_message verbose_msg);
    printer result
  end;

  (* Exit if the flag is set *)
  if !flag then exit 0;

  (* Return the result *)
  result

(* Define a function to handle the compilation process *)
let compile_process file =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    (* Parse and process the file *)
    let f = parse_file c lb in

    (* Perform compilation steps *)
    let fa = compile_step (fun () -> Clean_automaton.clean f) automaton_only "Unsugar automatons a bit" Parsed_ast_printer.print_file_standard in
    let ft = compile_step (fun () -> Typing.type_file fa main_node) type_only "Typed ast" Typed_ast_printer.print_file_std in
    let fn = compile_step (fun () -> Normalize.file ft) normalize_only "Normalized ast" Typed_ast_printer.print_file_std in
    let fc = compile_step (fun () -> Clocking.clock_file fn main_node)  clock_only"Clocked ast" Clocked_ast_printer.print_file_standard in
    let fs = compile_step (fun () -> Scheduling.schedule fc) schedule_only "Scheduled ast" Clocked_ast_printer.print_file_standard in
    let fi = compile_step (fun () -> Imp.compile fs) imp_only "Imp ast" Imp_ast_printer.pp in
    let fc = compile_step (fun () -> Compile.compile fi main_node file !no_sleep) c_only "C file" (C_printer.pp std_formatter) in

    (* Save the compiled file *)
    let file_name = Printf.sprintf "%s.c" (Filename.remove_extension file) in
    let file_c = open_out file_name in
    let out = Format.formatter_of_out_channel file_c in
    C_printer.pp out fc;
    close_out file_c;
  with e ->
    (* Handle exceptions and close resources *)
    close_in c;  (* Ensure the file is closed in case of an error *)
    handle_exception lb e

(* Entry point of the program *)
let () =
  try
    compile_process file
  with
  | Sys_error msg -> Printf.eprintf "System error: %s\n" msg
  | _ -> Printf.eprintf "An unexpected error occurred\n"

