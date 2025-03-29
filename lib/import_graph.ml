open Core
open Stdio

type t =
  { graph : (string, string list) Hashtbl.t
  ; transpose_graph : (string, string list) Hashtbl.t
  ; packages : String.Hash_set.t
  ; root : string
  }
[@@deriving sexp_of]

(** Prints an import graph [t] to stdout. *)
let show ~sexp_of t = sexp_of t |> Sexp.to_string_hum |> print_endline

(* ------------------- CONSTANTS  --------------------*)

let excluded_dirs = [ "__pycache__"; ".DS_Store" ]
let init_module_name = "__init__"

(* Sometimes needed as string or char*)
let module_separator = '.'
let module_separator_str = "."

(* ------------------- UTILS --------------------*)

let relativize_path_exn ~root file_name =
  match Fpath.relativize ~root file_name with
  | Some path -> path
  | None ->
    failwith
      (Printf.sprintf
         "Could not relativize file name %s with root %s."
         (Fpath.to_string file_name)
         (Fpath.to_string root))
;;

(* Get the directory that [path] is in. *)
let parents path = fst (Fpath.split_base path)

(* Compute the full module name based on the file path and project root *)
let module_name_of_path ~root file_name =
  (* Remove project root if the main module does not sit in the working directory *)
  let relative_path = relativize_path_exn ~root file_name in
  let file_without_extension = Fpath.rem_ext relative_path in
  String.substr_replace_all
    (Fpath.to_string file_without_extension)
    ~pattern:Fpath.dir_sep
    ~with_:module_separator_str
;;

let%expect_test "test_module_name_of_path" =
  let modname =
    module_name_of_path ~root:(Fpath.v "root_dir") (Fpath.v "root_dir/src/main.py")
  in
  print_string modname;
  [%expect {|src.main|}];
  let modname = module_name_of_path ~root:(Fpath.v ".") (Fpath.v "src/main.py") in
  print_string modname;
  [%expect {|src.main|}]
;;

(** Parse the AST from a file [file_name] *)
let parse_ast file_name =
  let content =
    In_channel.with_file ~f:In_channel.input_all (Fpath.to_string file_name)
  in
  let open PyreAst.Parser in
  with_context (fun context ->
    match Concrete.parse_module ~context content with
    | Result.Error { Error.message; line; column; _ } ->
      let message =
        Printf.sprintf "Parsing error at line %d, column %d: %s" line column message
      in
      failwith message
    | Result.Ok ast -> ast)
;;

(* Extract all prefixes from a module name.

   Example: s='a.b.c'; returns ["a", "a.b", "a.b.c"]*)
let extract_prefixes s =
  let parts = String.split s ~on:module_separator in
  List.foldi parts ~init:[] ~f:(fun i acc _ ->
    let subparts = List.take parts (i + 1) |> String.concat ~sep:module_separator_str in
    subparts :: acc)
  |> List.rev
;;

let%expect_test "test_extract_prefixes" =
  print_s [%sexp (extract_prefixes "a.b.c" : string list)];
  [%expect {|(a a.b a.b.c)|}];
  print_s [%sexp (extract_prefixes "module" : string list)];
  [%expect {|(module)|}];
  print_s [%sexp (extract_prefixes "" : string list)];
  [%expect {|("")|}]
;;

(** Get all python imports from a file [file_name] *)
let get_imports ~packages file_name =
  (* Cases: *)
  (* 1. import package.nested_package                                   -> package.__init__ + package.nested_package.__init__ *)
  (* 2. import package.nested_package.module                            -> package.__init__ + package.nested_package.__init__ +  + package.nested_package.module  *)
  (* 3. from package.nested_package import package1, ..., module, ...   -> package.__init__ + package.nested_package.__init__ + package.nested_package.package1.__init__ + package.nested_package.module *)
  (* 4. from package.nested_package.module import object1, object2      -> package.__init__ + package.nested_package.__init__ + package.nested_package.module *)
  let ast = parse_ast file_name in
  let open PyreAst.Concrete in
  let find_imports existing_imports = function
    (* Import an entire package or a module *)
    (* TODO: Add __init__ if module name is a package name *)
    | Statement.Import { names; _ } ->
      List.fold names ~init:existing_imports ~f:(fun acc import ->
        let import_name = Identifier.to_string import.name in
        if Hash_set.mem packages import_name
        then (
          (* Case 1. *)
          let subpackage_inits =
            extract_prefixes import_name
            |> List.map ~f:(fun package ->
              String.concat ~sep:module_separator_str [ package; init_module_name ])
          in
          subpackage_inits @ acc)
        else (
          (* Case 2. All expect last import part is a package. *)
          let parts = extract_prefixes import_name in
          let subpackage_inits =
            List.take parts (List.length parts - 1)
            |> List.map ~f:(fun package ->
              String.concat ~sep:module_separator_str [ package; init_module_name ])
          in
          let imported_module =
            List.hd (List.rev parts) |> Option.value ~default:"UNREACHABLE"
          in
          (imported_module :: subpackage_inits) @ acc))
    | Statement.ImportFrom { module_ = Some from_module_name; names; _ } ->
      let from_name = Identifier.to_string from_module_name in
      let import_names =
        List.map names ~f:(fun { ImportAlias.name; _ } -> Identifier.to_string name)
      in
      let subpackage_inits =
        extract_prefixes from_name
        |> List.map ~f:(fun package ->
          String.concat
            ~sep:(String.of_char module_separator)
            [ package; init_module_name ])
      in
      (* Check if [from_name] is a valid package. If so, we are on Case 3.
         Otherwise it's last component is a module (case 4.)*)
      if Hash_set.mem packages from_name
      then (
        (* Case 3. *)
        let top_level_imported_modules =
          List.fold import_names ~init:[] ~f:(fun acc name ->
            (* If the import name is a package, add the __init__ of that package. Otherwise, we just add the module name. *)
            let maybe_package =
              String.concat ~sep:module_separator_str [ from_name; name ]
            in
            if Hash_set.mem packages maybe_package
            then (
              let package_init =
                String.concat
                  ~sep:module_separator_str
                  [ maybe_package; init_module_name ]
              in
              package_init :: acc)
            else maybe_package :: acc)
        in
        top_level_imported_modules @ subpackage_inits @ existing_imports)
      else (
        (* Case 4. The first chunks of the "from" part is the package (for which we need to extract
           the __init__'s), and the last chunk is the module name. *)
        let module_import = from_name in
        let remaining_subpackage_inits =
          List.take subpackage_inits (List.length subpackage_inits - 1)
        in
        (module_import :: remaining_subpackage_inits) @ existing_imports)
    | _ -> existing_imports
  in
  List.fold ast.body ~init:[] ~f:find_imports |> List.rev
;;

(* ---------------------------------------*)

let add_imports_to_graph ~root ~packages python_files graph =
  print_endline "Python files to check:";
  print_s [%sexp (List.map ~f:Fpath.to_string python_files : string list)];
  let add_imports_from_file file =
    printf "Adding imports from file %s\n" (Fpath.to_string file);
    let imports = get_imports ~packages file in
    if not (List.is_empty imports)
    then (
      let module_name = module_name_of_path ~root file in
      Hashtbl.add_exn graph ~key:module_name ~data:imports)
  in
  List.iter python_files ~f:add_imports_from_file;
  graph
;;

(** Create an import graph. *)
let create_graph ~packages root =
  let rec find_python_files acc dir =
    (* Skip if the dir is not valid *)
    if List.mem excluded_dirs Fpath.(base dir |> to_string) ~equal:String.equal
    then acc
    else (
      let dir_str = Fpath.to_string dir in
      Sys_unix.fold_dir dir_str ~init:acc ~f:(fun _acc file_or_dir ->
        let file_or_dir_fpath = Fpath.v file_or_dir in
        let full_path = Fpath.append dir file_or_dir_fpath in
        if
          Sys_unix.is_file_exn (Fpath.to_string full_path)
          && String.equal (Fpath.get_ext file_or_dir_fpath) ".py"
        then full_path :: _acc
        else if Sys_unix.is_directory_exn (Fpath.to_string full_path)
        then find_python_files _acc full_path
        else _acc))
  in
  let python_files = find_python_files [] root in
  let graph = Hashtbl.create (module String) in
  let graph = add_imports_to_graph ~root ~packages python_files graph in
  graph
;;

(** Create a transpose graph from a graph [graph]. *)
let transpose graph =
  let transpose_graph = Hashtbl.create (module String) in
  Hashtbl.iteri graph ~f:(fun ~key ~data ->
    List.iter data ~f:(fun value ->
      let current = Hashtbl.find_or_add transpose_graph value ~default:(fun () -> []) in
      Hashtbl.set transpose_graph ~key:value ~data:(key :: current)));
  transpose_graph
;;

(** Find all packages in a python project, starting from [root]. *)
let find_packages root =
  let rec find_packages_in_dir acc dir =
    let packages =
      Sys_unix.fold_dir dir ~init:acc ~f:(fun _acc subdir ->
        let subdir_full_path =
          Fpath.append (Fpath.v dir) (Fpath.v subdir) |> Fpath.to_string
        in
        if
          Sys_unix.is_directory_exn subdir_full_path
          && not (List.mem ~equal:String.equal excluded_dirs subdir)
        then (
          let relative_subdir_path =
            Fpath.(relativize ~root:(v root) (v subdir_full_path)) |> Option.value_exn
          in
          let subdir_package_name =
            String.substr_replace_all
              (Fpath.to_string relative_subdir_path)
              ~pattern:Fpath.dir_sep
              ~with_:(String.of_char module_separator)
          in
          find_packages_in_dir (subdir_package_name :: _acc) subdir_full_path)
        else _acc)
    in
    packages
  in
  let all_packages = find_packages_in_dir [] root in
  String.Hash_set.of_list all_packages
;;

(** Create an import graph from a root file [file_name] *)
let create file_name : t =
  (* Parent of the base file is considered the root. *)
  (* TODO: fix the case where CWD=. file_name=api/__main__.py *)
  let root = parents file_name in
  printf "file_name %s\n" (Fpath.to_string file_name);
  printf "root %s\n" (Fpath.to_string root);
  printf "Finding all packages\n";
  let packages = find_packages (Fpath.to_string root) in
  printf "Creating graph\n";
  let graph = create_graph ~packages root in
  printf "Creating graph transpose\n";
  let transpose_graph = transpose graph in
  let t =
    { graph; transpose_graph; packages; root = module_name_of_path ~root file_name }
  in
  show ~sexp_of:sexp_of_t t;
  t
;;

let%expect_test "test_get_imports" =
  let file_name =
    Fpath.v
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_from_import_package/__main__.py"
  in
  let packages =
    find_packages
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_from_import_package/"
  in
  print_s [%sexp (get_imports ~packages file_name : string list)];
  (* from lib import nested *)
  [%expect {|(lib.__init__ lib.nested.__init__)|}];
  let file_name =
    Fpath.v
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_from_import_module/__main__.py"
  in
  let packages =
    find_packages
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_from_import_module/"
  in
  print_s [%sexp (get_imports ~packages file_name : string list)];
  (* from lib import mod *)
  [%expect {|(lib.__init__ lib.mod)|}];
  let file_name =
    Fpath.v
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_from_import_object/__main__.py"
  in
  let packages =
    find_packages
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_from_import_object/"
  in
  print_s [%sexp (get_imports ~packages file_name : string list)];
  (* from lib.mod import a *)
  [%expect {|(lib.__init__ lib.mod)|}];
  let file_name =
    Fpath.v
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_from_import_module_and_package/__main__.py"
  in
  let packages =
    find_packages
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_from_import_module_and_package/"
  in
  print_s [%sexp (get_imports ~packages file_name : string list)];
  (* from lib import nested, mod *)
  [%expect {|(lib.__init__ lib.nested.__init__ lib.mod)|}];
  let file_name =
    Fpath.v
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_from_import_packages/__main__.py"
  in
  let packages =
    find_packages
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_from_import_packages/"
  in
  print_s [%sexp (get_imports ~packages file_name : string list)];
  (* from lib import nested, another_nested *)
  [%expect {|(lib.__init__ lib.nested.__init__ lib.another_nested.__init__)|}];
  let file_name =
    Fpath.v
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_import_module/__main__.py"
  in
  let packages =
    find_packages
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_import_module/"
  in
  print_s [%sexp (get_imports ~packages file_name : string list)];
  (* import lib.mod *)
  [%expect {|(lib.__init__ lib.mod)|}];
  let file_name =
    Fpath.v
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_import_package/__main__.py"
  in
  let packages =
    find_packages
      "/Users/danielssonsimon/projects/circular_imports/data/fixtures/test_import_package/"
  in
  print_s [%sexp (get_imports ~packages file_name : string list)];
  (* import lib *)
  [%expect {|(lib.__init__)|}]
;;
