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
  let module_imports =
    List.fold module_ast.body ~init:[] ~f:(fun acc stmt -> visit_stmt acc stmt)
  in
  let get_package_name import =
    let target_module_components = String.split import ~on:'.' in
    let target_package_components =
      List.take target_module_components (List.length target_module_components - 1)
    in
    String.concat ~sep:"." target_package_components
  in
  let is_in_same_package import =
    String.equal
      (get_package_name (Import.target_of_t import))
      (get_package_name module_name)
  in
  let add_init_import import =
    let target_module_components = String.split (Import.target_of_t import) ~on:'.' in
    let target_package_components_rev =
      List.take target_module_components (List.length target_module_components - 1)
      |> List.rev
    in
    let target_init_module_components =
      "__init__" :: target_package_components_rev |> List.rev
    in
    let init_import =
      Import.make
        ~source:module_name
        ~target:(String.concat target_init_module_components ~sep:".")
    in
    init_import
  in
  let imports =
    List.fold module_imports ~init:module_imports ~f:(fun acc import ->
      if is_in_same_package import then acc else add_init_import import :: acc)
  in
  let imports_without_duplicates =
    List.remove_consecutive_duplicates imports ~equal:Import.equal
  in
  List.iter imports_without_duplicates ~f:(fun import ->
    Logger.log
      verbose
      (Printf.sprintf
         "Adding import: %s -> %s"
         (Import.source_of_t import)
         (Import.target_of_t import)));
  imports_without_duplicates
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
