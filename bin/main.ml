open Base
open Stdio

(* Define a function that will be called with the file argument *)
let read_file file_name = In_channel.with_file file_name ~f:In_channel.input_all

let parse_module file_content =
  let open PyreAst.Parser in
  with_context (fun context ->
    match Concrete.parse_module ~context file_content with
    | Result.Error { Error.message; line; column; _ } ->
      let message =
        Printf.sprintf "Parsing error at line %d, column %d: %s" line column message
      in
      failwith message
    | Result.Ok ast -> ast)
;;

(* Process a single Python file and add its imports to the import map *)
let process_file verbose all_imports file_name =
  let open Circular_imports.Logger in
  log verbose (Printf.sprintf "Processing file: %s" file_name);
  try
    log verbose "Reading file content...";
    let file_content = read_file file_name in
    log verbose "Parsing module...";
    let module_ast = parse_module file_content in
    log verbose "Extracting imports...";
    let raw_imports = Circular_imports.Analyzer.extract_imports ~verbose module_ast in
    log verbose "Normalizing imports...";
    let imports =
      Circular_imports.Analyzer.normalize_imports ~verbose file_name raw_imports
    in
    all_imports @ imports
    (* List.fold imports ~init:file_map ~f:(fun acc import -> *)
    (*   Map.add_multi acc ~key:import.source ~data:import.target) *)
  with
  | exn ->
    Stdlib.Printf.printf "Error processing %s: %s\n" file_name (Exn.to_string exn);
    failwith "Something went wrong"
;;

(* Recursively find all Python files in a directory *)
let rec find_python_files verbose dir =
  let open Circular_imports.Logger in
  log verbose (Printf.sprintf "Searching for Python files in directory: %s" dir);
  let contents = Stdlib.Sys.readdir dir |> Array.to_list in
  List.fold contents ~init:[] ~f:(fun acc name ->
    let path = Stdlib.Filename.concat dir name in
    if Stdlib.Sys.is_directory path && not (String.is_prefix name ~prefix:".")
    then acc @ find_python_files verbose path
    else if String.is_suffix path ~suffix:".py"
    then path :: acc
    else acc)
;;

(* Find cycles in a file*)
let find_cycles verbose entry_file =
  let open Circular_imports.Logger in
  log verbose (Printf.sprintf "Finding cycles starting from entry file: %s" entry_file);
  let dir = Stdlib.Filename.dirname entry_file in
  let python_files = find_python_files verbose dir in
  log verbose (Printf.sprintf "Found %d Python files:" (List.length python_files));
  List.iter python_files ~f:(fun name -> log verbose (Printf.sprintf "%s, " name));
  log verbose "\n";
  let imports = List.fold python_files ~init:[] ~f:(process_file verbose) in
  log verbose "Building import graph...";
  let graph = Circular_imports.Analyzer.build_graph imports in
  (* Find cycles in the import graph *)
  let cycles = Circular_imports.Analyzer.find_cycles ~verbose graph in
  log verbose (Printf.sprintf "Found %d cycles" (List.length cycles));
  cycles
;;

(* Find circular imports in a project *)
let find_and_show_cicular_imports entry_file verbose =
  let cycles = find_cycles verbose entry_file in
  (* Display cycles *)
  let open Stdlib in
  if List.length cycles > 0
  then (
    Printf.printf "Found %d circular import chain(s):\n\n" (List.length cycles);
    let formatted_cycles = Circular_imports.Analyzer.format_cycles cycles in
    Printf.printf "%s\n" formatted_cycles;
    exit 1)
  else (
    Printf.printf "No circular imports found.\n";
    exit 0)
;;

(* Build the command and pass it to Term.eval *)
let () = Circular_imports.Argparser.run ~callback:find_and_show_cicular_imports
