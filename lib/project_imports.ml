open Core
open Logger

type t = Module_imports.t

let read_excluded_dirs () =
  In_channel.read_all "config/excluded_dirs.sexp"
  |> Sexp.of_string
  |> List.t_of_sexp String.t_of_sexp
;;

(* Recursively find all Python files in a directory *)
let rec find_python_files ~verbose ~excluded_dirs dir acc =
  log verbose (Printf.sprintf "Searching for Python files in directory: %s" dir);
  match Sys_unix.is_directory dir with
  | `No | `Unknown -> acc
  | `Yes ->
    let dir_contents =
      Sys_unix.readdir dir
      |> Array.to_list
      |> List.filter ~f:(fun dir -> not (List.mem excluded_dirs dir ~equal:String.equal))
      (* Iterate over the contents of the directory and filter out hidden files *)
    in
    List.fold dir_contents ~init:acc ~f:(fun acc name ->
      let path = Fpath.append (Fpath.v dir) (Fpath.v name) |> Fpath.to_string in
      match Sys_unix.is_directory path with
      | `Yes ->
        find_python_files ~verbose ~excluded_dirs path acc
        (* Tail-recursive call *)
        (* Append each file to the accumulator *)
      | `No | `Unknown -> if String.is_suffix path ~suffix:".py" then path :: acc else acc)
;;

(* List.fold dir_contents ~init:[] ~f:(fun acc name -> *)
(*   let path = Stdlib.Filename.concat dir name in *)
(*   if Sys.is_directory path && not (String.is_prefix name ~prefix:".") *)
(*   then acc @ find_python_files ~verbose path *)
(*   else if String.is_suffix path ~suffix:".py" *)
(*   then path :: acc *)
(*   else acc) *)

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
  let dir = Filename.dirname file_name in
  let excluded_dirs = read_excluded_dirs () in
  let python_files = find_python_files ~verbose ~excluded_dirs dir [] in
  log verbose (Printf.sprintf "Found %d Python files:" (List.length python_files));
  (* Find all imports in this project *)
  List.fold python_files ~init:[] ~f:(find_imports_in_file ~verbose ~project_root:dir)
;;
