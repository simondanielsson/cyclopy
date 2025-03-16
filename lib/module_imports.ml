open Base
open Stdio

(* Represent all import statements of a module *)
type t = Import.t list

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

let extract_imports_from_module
      ~verbose
      ~module_name
      (module_ast : PyreAst.Concrete.Module.t)
  =
  let open PyreAst.Concrete in
  let visit_stmt imports = function
    | Statement.Import { names; _ } ->
      List.fold names ~init:imports ~f:(fun acc import_alias ->
        let import_name = Identifier.to_string import_alias.name in
        Logger.log verbose ("Found import statement: " ^ import_name);
        let import = Import.make ~source:module_name ~target:import_name in
        import :: acc)
    | Statement.ImportFrom { module_ = Some from_module_name; _ } ->
      let import_name = Identifier.to_string from_module_name in
      Logger.log verbose ("Found from-import statement: " ^ import_name);
      let import = Import.make ~source:module_name ~target:import_name in
      import :: imports
    | _ -> imports
  in
  List.fold module_ast.body ~init:[] ~f:(fun acc stmt -> visit_stmt acc stmt)
;;

(* Compute the full module name based on the file path and project root *)
let module_name_of_path ~project_root file_name =
  (* If file_path begins with project_root, remove it *)
  let relative_path =
    if String.is_prefix ~prefix:project_root file_name
    then String.drop_prefix file_name (String.length project_root + 1)
    else file_name
  in
  (* Remove the file extension *)
  let file_without_extension = Stdlib.Filename.chop_extension relative_path in
  (* Split the path using the system's directory separator and join with dots *)
  let dir_separator = Char.of_string Stdlib.Filename.dir_sep in
  let parts = String.split ~on:dir_separator file_without_extension in
  String.concat ~sep:"." parts
;;

let from_file_content ~verbose ~module_name file_content =
  parse_module file_content |> extract_imports_from_module ~verbose ~module_name
;;

let from_file ~verbose ~project_root ?reader file_name =
  let file_content =
    match reader with
    | None -> In_channel.with_file file_name ~f:In_channel.input_all
    | Some read -> read file_name
  in
  let module_name = module_name_of_path ~project_root file_name in
  from_file_content ~verbose ~module_name file_content
;;
