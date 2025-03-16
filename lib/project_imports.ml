open Base
open Logger

type t = Module_imports.t

(* Recursively find all Python files in a directory *)
let rec find_python_files ~verbose dir =
  log verbose (Printf.sprintf "Searching for Python files in directory: %s" dir);
  let contents = Stdlib.Sys.readdir dir |> Array.to_list in
  List.fold contents ~init:[] ~f:(fun acc name ->
    let path = Stdlib.Filename.concat dir name in
    if Stdlib.Sys.is_directory path && not (String.is_prefix name ~prefix:".")
    then acc @ find_python_files ~verbose path
    else if String.is_suffix path ~suffix:".py"
    then path :: acc
    else acc)
;;

(* Process a single Python file and add its imports to the import map *)
let find_imports_in_file ~verbose ~project_root all_imports file_name =
  log verbose (Printf.sprintf "Processing file: %s" file_name);
  try
    log verbose "Extracting imports...";
    let imports = Module_imports.from_file ~verbose ~project_root file_name in
    List.fold ~init:all_imports imports ~f:(fun acc import -> import :: acc)
  with
  | exn ->
    Stdlib.Printf.printf "Error processing %s: %s\n" file_name (Exn.to_string exn);
    failwith "Something went wrong"
;;

let find ~verbose file_name =
  (* Find all python files in this project*)
  log verbose (Printf.sprintf "Finding cycles starting from entry file: %s" file_name);
  let dir = Stdlib.Filename.dirname file_name in
  let python_files = find_python_files ~verbose dir in
  log verbose (Printf.sprintf "Found %d Python files:" (List.length python_files));
  (* Find all imports in this project *)
  List.fold python_files ~init:[] ~f:(find_imports_in_file ~verbose ~project_root:dir)
;;
