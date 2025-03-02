open Base

(* Define a function that will be called with the file argument *)
let read_file file_name = In_channel.with_file file_name ~f:In_channel.input_all

let parse_module file_content =
  let open PyreAst.Parser in
  with_context (fun context ->
    match Concrete.parse_module ~context file_content with
    | Result.Error { Error.message; line; column; _ } ->
      let message =
        Format.sprintf "Parsing error at line %d, column %d: %s" line column message
      in
      failwith message
    | Result.Ok ast -> ast)
;;

(* Process a single Python file and add its imports to the import map *)
let process_file file_map file_name =
  try
    let file_content = read_file file_name in
    let module_ast = parse_module file_content in
    let raw_imports = Circular_imports.Analyzer.extract_imports module_ast in
    let imports = Circular_imports.Analyzer.normalize_imports file_name raw_imports in
    List.fold imports ~init:file_map ~f:(fun acc import ->
      Map.add_multi acc ~key:import.source ~data:import.target)
  with
  | exn ->
    Printf.printf "Error processing %s: %s\n" file_name (Exn.to_string exn);
    file_map
;;

(* Recursively find all Python files in a directory *)
let rec find_python_files dir =
  let contents = Sys_unix.readdir dir |> Array.to_list in
  List.fold contents ~init:[] ~f:(fun acc name ->
    let path = Filename.concat dir name in
    if Sys_unix.is_directory path = `Yes && not (String.is_prefix name ~prefix:".")
    then acc @ find_python_files path
    else if String.is_suffix path ~suffix:".py"
    then path :: acc
    else acc)
;;

(* Find circular imports in a project *)
let find_cicular_imports entry_file =
  let dir = Filename.dirname entry_file in
  let python_files = find_python_files dir in
  (* Build a complete import map from all files *)
  let import_map =
    List.fold python_files ~init:(Map.empty (module String)) ~f:process_file
  in
  (* Convert the map to an import graph structure *)
  let import_graph =
    Map.to_alist import_map
    |> List.map ~f:(fun (source, targets) ->
      Circular_imports.Analyzer.{ source; target = targets })
    |> Circular_imports.Analyzer.build_graph
  in
  (* Find cycles in the import graph *)
  let cycles = Circular_imports.Analyzer.find_cycles import_graph in
  (* Display cycles *)
  if List.length cycles > 0
  then (
    Printf.printf "Found %d circular import chain(s):\n\n" (List.length cycles);
    let formatted_cycles = Circular_imports.Analyzer.format_cycles cycles in
    Printf.printf "%s\n" formatted_cycles)
  else Printf.printf "No circular imports found.\n"
;;

(* Build the command and pass it to Term.eval *)
let () = Circular_imports.Argparser.run ~callback:find_cicular_imports
