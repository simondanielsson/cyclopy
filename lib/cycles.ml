open Base

type t = string list list

(** Build a graph representing import relationships.

    The graph is represented as a mapping from the module name to the modules it imports
    *)
let build_graph imports =
  let graph = Hashtbl.create (module String) in
  List.iter imports ~f:(fun import ->
    let source = Import.source_of_t import in
    let target = Import.target_of_t import in
    match Hashtbl.find graph source with
    | Some deps -> Hashtbl.set graph ~key:source ~data:(target :: deps)
    | None -> Hashtbl.set graph ~key:source ~data:[ target ]);
  graph
;;

(** Find cycles in the import graph using DFS *)
let find ~verbose (imports : Project_imports.t) =
  let graph = build_graph imports in
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
          Logger.log verbose ("Found cycle to neighbor " ^ neighbor);
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
let format_cycles t =
  List.map t ~f:(fun cycle -> String.concat ~sep:" -> " cycle) |> String.concat ~sep:"\n"
;;

let show ~verbose t =
  Logger.log verbose (Printf.sprintf "Found %d cycles" (List.length t));
  (* Display cycles *)
  let open Stdlib in
  if List.length t > 0
  then (
    Printf.printf "Found %d circular import chain(s):\n\n" (List.length t);
    let formatted_cycles = format_cycles t in
    Printf.printf "%s\n" formatted_cycles;
    exit 1)
  else (
    Printf.printf "No circular imports found.\n";
    exit 0)
;;
