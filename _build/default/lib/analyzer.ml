open Base

(* Represent an import statement *)
type import =
  { source : string
  ; target : string
  }

(* Graph representation for detecting circular dependencies *)
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type import_graph = StringSet.t StringMap.t

(* Extract imports from a Python module *)
let extract_imports module_ast =
  let open PyreAst.Concrete in
  let rec visit_stmt imports = function
    | Import { Import.imports; _ } ->
      List.fold imports ~init:imports ~f:(fun acc { Import.name; _ } ->
        { source = "current_module"; target = name } :: acc)
    | ImportFrom { ImportFrom.module_name; _ } ->
      { source = "current_module"; target = module_name } :: imports
    | Block { Block.statements; _ } ->
      List.fold statements ~init:imports ~f:(fun acc stmt -> visit_stmt acc stmt)
    | _ -> imports
  in
  match module_ast with
  | Module { Module.statements; _ } ->
    List.fold statements ~init:[] ~f:(fun acc stmt -> visit_stmt acc stmt)
  | _ -> []
;;

(* Build a graph from import statements *)
let build_graph ~source_file imports =
  let base_module = Filename.basename source_file |> Filename.chop_extension in
  (* Initialize with empty dependencies for all modules *)
  let init_graph =
    List.fold imports ~init:StringMap.empty ~f:(fun acc { source; target } ->
      let real_source =
        if String.equal source "current_module" then base_module else source
      in
      let graph =
        if not (StringMap.mem acc real_source)
        then StringMap.set acc ~key:real_source ~data:StringSet.empty
        else acc
      in
      if not (StringMap.mem graph target)
      then StringMap.set graph ~key:target ~data:StringSet.empty
      else graph)
  in
  (* Add all dependencies *)
  List.fold imports ~init:init_graph ~f:(fun acc { source; target } ->
    let real_source =
      if String.equal source "current_module" then base_module else source
    in
    let current_deps =
      match StringMap.find acc real_source with
      | Some deps -> deps
      | None -> StringSet.empty
    in
    StringMap.set acc ~key:real_source ~data:(StringSet.add current_deps target))
;;

(* DFS to find cycles *)
let find_cycles graph =
  let cycles = ref [] in
  let rec dfs path visited node =
    if StringSet.mem visited node
    then (
      (* Check if we've found a cycle *)
      match List.findi path ~f:(fun _ n -> String.equal n node) with
      | Some (idx, _) ->
        let cycle = List.sub path ~pos:idx ~len:(List.length path - idx) in
        cycles := (cycle @ [ node ]) :: !cycles
      | None -> ())
    else (
      let visited = StringSet.add visited node in
      let neighbors =
        match StringMap.find graph node with
        | Some deps -> StringSet.to_list deps
        | None -> []
      in
      List.iter neighbors ~f:(fun neighbor -> dfs (path @ [ node ]) visited neighbor))
  in
  StringMap.iter_keys graph ~f:(fun node -> dfs [] StringSet.empty node);
  !cycles
;;

(* Format cycles for display *)
let format_cycles cycles =
  if List.is_empty cycles
  then "No circular imports found."
  else (
    let cycle_strings =
      List.mapi cycles ~f:(fun i cycle ->
        let cycle_str = String.concat ~sep:" -> " cycle in
        Printf.sprintf "Cycle %d: %s" (i + 1) cycle_str)
    in
    String.concat ~sep:"\n" cycle_strings)
;;

open Base

(* Represent a module import relationship *)
type import =
  { source : string
  ; target : string
  }

(* Extract import statements from a Python AST *)
let extract_imports module_ast =
  let open PyreAst.Ast.Statement in
  let rec extract_from_statements statements imports =
    List.fold statements ~init:imports ~f:(fun acc statement ->
      match statement with
      | Import { imports; _ } ->
        List.fold imports ~init:acc ~f:(fun acc_inner import ->
          let target = import.PyreAst.Ast.Import.name.PyreAst.Ast.Identifier.value in
          { source = "current_module"; target } :: acc_inner)
      | ImportFrom { from = Some from; imports; _ } ->
        let from_module = from.PyreAst.Ast.ImportFrom.name.PyreAst.Ast.Identifier.value in
        List.fold imports ~init:acc ~f:(fun acc_inner import ->
          let target = from_module in
          { source = "current_module"; target } :: acc_inner)
      | _ -> acc)
  in
  match module_ast with
  | PyreAst.Ast.Module.Module { body; _ } -> extract_from_statements body []
  | _ -> []
;;

(* Convert source module name once we know the actual file being processed *)
let normalize_imports file_path imports =
  let module_name = Filename.basename file_path |> Filename.chop_extension in
  List.map imports ~f:(fun import ->
    if String.equal import.source "current_module"
    then { import with source = module_name }
    else import)
;;

(* Build a graph representing import relationships *)
let build_graph imports =
  let graph = Hashtbl.create (module String) in
  List.iter imports ~f:(fun { source; target } ->
    match Hashtbl.find graph source with
    | Some deps -> Hashtbl.set graph ~key:source ~data:(target :: deps)
    | None -> Hashtbl.set graph ~key:source ~data:[ target ]);
  graph
;;

(* Find cycles in the import graph using DFS *)
let find_cycles graph =
  let visited = Hashtbl.create (module String) in
  let rec_stack = Hashtbl.create (module String) in
  let cycles = ref [] in
  let rec dfs node path =
    Hashtbl.set visited ~key:node ~data:true;
    Hashtbl.set rec_stack ~key:node ~data:true;
    match Hashtbl.find graph node with
    | Some neighbors ->
      List.iter neighbors ~f:(fun neighbor ->
        let cycle_found = ref false in
        if not (Hashtbl.mem visited neighbor)
        then dfs neighbor (neighbor :: path)
        else if Hashtbl.find_exn rec_stack neighbor
        then (
          (* Found a cycle *)
          let cycle_path = neighbor :: path in
          let cycle_start_idx =
            List.findi cycle_path ~f:(fun _ n -> String.equal n neighbor)
            |> Option.value_exn
            |> fst
          in
          let cycle = List.take cycle_path (cycle_start_idx + 1) |> List.rev in
          cycles := cycle :: !cycles;
          cycle_found := true))
    | None ->
      ();
      Hashtbl.set rec_stack ~key:node ~data:false
  in
  Hashtbl.iter_keys graph ~f:(fun node ->
    if not (Hashtbl.mem visited node) then dfs node [ node ]);
  !cycles
;;

(* Format cycles for display *)
let format_cycles cycles =
  List.map cycles ~f:(fun cycle ->
    String.concat ~sep:" -> " cycle ^ " -> " ^ List.hd_exn cycle)
  |> String.concat ~sep:"\n"
;;
