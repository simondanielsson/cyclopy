open Base

(* Represent an import statement *)
type import =
  { source : string
  ; target : string
  }

let extract_imports ~verbose (module_ast : PyreAst.Concrete.Module.t) =
  let open PyreAst.Concrete in
  let visit_stmt imports = function
    | Statement.Import { names; _ } ->
      List.fold names ~init:imports ~f:(fun acc import_alias ->
        let import_name = Identifier.to_string import_alias.name in
        Logger.log verbose ("Found import statement: " ^ import_name);
        { source = "current_module"; target = import_name } :: acc)
    | Statement.ImportFrom { module_ = Some module_name; _ } ->
      let import_name = Identifier.to_string module_name in
      Logger.log verbose ("Found from-import statement: " ^ import_name);
      { source = "current_module"; target = import_name } :: imports
    | _ -> imports
  in
  List.fold module_ast.body ~init:[] ~f:(fun acc stmt -> visit_stmt acc stmt)
;;

(* Convert source module name once we know the actual file being processed *)
let normalize_imports ~verbose file_path imports =
  let module_name = Stdlib.Filename.(basename file_path |> chop_extension) in
  Logger.log
    verbose
    (Printf.sprintf "Normalizing import using module name %s if applicable" module_name);
  List.map imports ~f:(fun import ->
    Logger.log verbose ("Source and target:" ^ import.source ^ " " ^ import.target);
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
let find_cycles ~verbose graph =
  let visited = Hashtbl.create (module String) in
  let rec_stack = Hashtbl.create (module String) in
  let cycles = ref [] in
  let rec dfs node path =
    Hashtbl.set visited ~key:node ~data:true;
    Hashtbl.set rec_stack ~key:node ~data:true;
    match Hashtbl.find graph node with
    | Some neighbors ->
      Logger.log
        verbose
        (Printf.sprintf "Found %d neighbors to node: %s" (List.length neighbors) node);
      List.iter neighbors ~f:(fun neighbor ->
        if not (Hashtbl.mem visited neighbor)
        then dfs neighbor (neighbor :: path)
        else if Hashtbl.find_exn rec_stack neighbor
        then (
          (* Found a cycle *)
          Logger.log verbose ("Found cycle to neightbor " ^ neighbor);
          let cycle_path = neighbor :: path in
          Logger.log verbose ("Cycle path " ^ String.concat ~sep:"-" (List.rev cycle_path));
          let cycle_start_idx =
            List.findi cycle_path ~f:(fun _ n -> String.equal n neighbor)
            |> Option.value_exn
            |> fst
          in
          Logger.log verbose ("Cycle start index " ^ Int.to_string cycle_start_idx);
          let cycle = List.drop cycle_path cycle_start_idx |> List.rev in
          (* Only complete the cycle if it isn’t already complete *)
          let cycle_complete =
            match List.last cycle with
            | Some last when String.equal last (List.hd_exn cycle) -> cycle
            | _ -> cycle @ [ List.hd_exn cycle ]
          in
          Logger.log
            verbose
            ("Len of cycle before adding to all cycles list: "
             ^ (List.length cycle_complete |> Int.to_string));
          cycles := cycle_complete :: !cycles))
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
  List.map cycles ~f:(fun cycle -> String.concat ~sep:" -> " cycle)
  |> String.concat ~sep:"\n"
;;
