open Core

type t = string list list list

let create_indicator_graph ~default graph =
  let visited =
    Hashtbl.create
      (module String)
      ~size:(List.length (Hashtbl.keys graph))
      ~growth_allowed:false
  in
  Hashtbl.iter_keys graph ~f:(fun key ->
    match Hashtbl.add visited ~key ~data:default with
    | `Duplicate ->
      failwith ("Tried to add visited=false for module more than once. module=" ^ key)
    | `Ok -> ());
  visited
;;

(** Create a stack of the [graph] in post order, starting from [root].

    Post order ensures that for any two elements u, v in the returned stack
    where u -> v, u is always to the left of v. If u and v belong to the same SCC,
    then their relative order is arbitrary so their order in the stack will also be.

    The stack contains the name of the module of each import vertex. *)
let traverse_graph_post_order ~root (graph : (string, string list) Hashtbl.t)
  : string list
  =
  let visited = Hashtbl.create (module String) in
  let rec visit_vertices_post_order ~(stack : string list) (vertex : string) : string list
    =
    if not (Hashtbl.mem visited vertex)
    then (
      Hashtbl.add_exn visited ~key:vertex ~data:true;
      let neighbors = Hashtbl.find graph vertex |> Option.value ~default:[] in
      (* Visit all neighbors, prepending to the stack *)
      let new_acc =
        List.fold neighbors ~init:stack ~f:(fun module_acc neighbor ->
          visit_vertices_post_order ~stack:module_acc neighbor)
      in
      (* Lastly, add current vertex to the stack *)
      vertex :: new_acc)
    else stack
  in
  visit_vertices_post_order ~stack:[] root
;;

let%expect_test "test_traverse_graph_post_order" =
  let graph = Hashtbl.create (module String) in
  Hashtbl.add_exn graph ~key:"main" ~data:[ "a"; "utils.b" ];
  Hashtbl.add_exn graph ~key:"a" ~data:[ "main" ];
  Hashtbl.add_exn graph ~key:"utils.b" ~data:[ "utils.c" ];
  Hashtbl.add_exn graph ~key:"utils.c" ~data:[ "main" ];
  let root = "main" in
  print_s [%sexp (traverse_graph_post_order ~root graph : string list)];
  [%expect {|(main utils.b utils.c a)|}]
;;

(** Collect strongly connected components (SCC's).

    The algorithm will return "sets" of SCC's. You need to transverse the graph
    once more to recover the original order. *)
let find_strongly_connected_components
      ~(stack : string list)
      (transpose_graph : (string, string list) Hashtbl.t)
  : string list list
  =
  let visited = Hashtbl.create (module String) in
  (* Graph to indicate which SCC root each vertex belongs to *)
  let rec add_vertices_to_sccs (scc : string list) (vertex : string) : string list =
    if not (Hashtbl.mem visited vertex)
    then (
      Hashtbl.add_exn visited ~key:vertex ~data:true;
      let neighbors = Hashtbl.find transpose_graph vertex |> Option.value ~default:[] in
      let scc = vertex :: scc in
      printf "adding vertex to scc: %s\n" vertex;
      print_s [%sexp (scc : string list)];
      List.fold neighbors ~init:scc ~f:add_vertices_to_sccs)
    else scc
  in
  let sccs =
    List.fold stack ~init:[] ~f:(fun acc vertex ->
      let scc = add_vertices_to_sccs [] vertex |> List.rev in
      print_endline "SCC done";
      scc :: acc)
    |> List.remove_consecutive_duplicates ~equal:(List.equal String.equal)
  in
  (* Remove empties *)
  List.fold sccs ~init:[] ~f:(fun acc scc ->
    if not (List.is_empty scc) then scc :: acc else acc)
;;

let%expect_test "test_traverse_graph_post_order" =
  let graph = Hashtbl.create (module String) in
  Hashtbl.add_exn graph ~key:"main" ~data:[ "a"; "utils.b" ];
  Hashtbl.add_exn graph ~key:"a" ~data:[ "main" ];
  Hashtbl.add_exn graph ~key:"utils.b" ~data:[ "utils.c" ];
  Hashtbl.add_exn graph ~key:"utils.c" ~data:[ "main" ];
  let root = "main" in
  let stack = traverse_graph_post_order ~root graph in
  let transpose_graph = Import_graph.transpose graph in
  print_s
    [%sexp (find_strongly_connected_components ~stack transpose_graph : string list list)];
  [%expect {|((main utils.c utils.b a))|}]
;;

let normalize_cycle (cycle : string list) : string list =
  match cycle with
  | [] -> []
  | _ ->
    let len = List.length cycle in
    let rotated_cycles =
      List.init len ~f:(fun i ->
        let left, right = List.split_n cycle i in
        right @ left)
    in
    List.min_elt rotated_cycles ~compare:(List.compare String.compare) |> Option.value_exn
;;

let find_cycles_in_scc scc graph : string list list =
  print_endline "----\nRecovering order";
  let ssc_set = String.Hash_set.of_list scc in
  let cycles = ref [] in
  let rec find_cycle ~start_vertex ~current_vertex ~path ~visited =
    if not (Hash_set.mem visited current_vertex)
    then (
      let path = current_vertex :: path in
      let neighbors = Hashtbl.find graph current_vertex |> Option.value ~default:[] in
      List.iter neighbors ~f:(fun neighbor ->
        if String.equal start_vertex neighbor
        then (
          print_endline "Completed cycle";
          print_s [%sexp (path : string list)];
          (* let completed_cycle = List.rev (start_vertex :: path) in *)
          cycles := List.rev path :: !cycles)
        else if
          Hash_set.mem ssc_set neighbor
          && not (List.mem path neighbor ~equal:String.equal)
        then (
          print_endline "Continuing";
          find_cycle ~start_vertex ~current_vertex:neighbor ~path ~visited)))
  in
  (* Find all cycles from each node in the SSC. (Might have duplicates) *)
  List.iter scc ~f:(fun start_vertex ->
    let visited = Hash_set.create (module String) in
    find_cycle ~start_vertex ~current_vertex:start_vertex ~path:[] ~visited);
  print_endline "Cycles:";
  print_s [%sexp (!cycles : string list list)];
  (* Remove duplicate cycles (rotational) *)
  let cycle_equal c c' =
    String.Hash_set.equal (String.Hash_set.of_list c) (String.Hash_set.of_list c')
  in
  let normalized_cycles =
    List.map !cycles ~f:normalize_cycle
    |> List.sort ~compare:(List.compare String.compare)
  in
  print_endline "Normalized cycles:";
  print_s [%sexp (normalized_cycles : string list list)];
  let unique_cycles =
    List.remove_consecutive_duplicates
      normalized_cycles
      ~which_to_keep:`First
      ~equal:cycle_equal
  in
  (* Add back the root to the end of the cycle *)
  List.map unique_cycles ~f:(fun cycle ->
    match cycle with
    | [] -> []
    | hd :: _ as c -> c @ [ hd ])
;;

(* Run kosaraju's algorithm to find the strongly connected components (SCCs) of a [graph] (using its [transpose_graph])*)
let kosaraju (import_graph : Import_graph.t) : t =
  (* 1. Run DFS to populate a stack with the vertices in correct order *)
  let stack = traverse_graph_post_order ~root:import_graph.root import_graph.graph in
  (* 2. Run DFS on transpose graph in the stack order to find the SCCs *)
  let sccs = find_strongly_connected_components ~stack import_graph.transpose_graph in
  (* A SCC might contain multiple cycles. *)
  List.fold sccs ~init:[] ~f:(fun acc scc ->
    let cycles = find_cycles_in_scc scc import_graph.graph in
    if not (List.is_empty cycles) then cycles :: acc else acc)
;;

(** Find all cycles in a python project represented by a [import_graph] *)
let find (import_graph : Import_graph.t) : t =
  printf "Finding cycles...\n";
  let sccs = kosaraju import_graph in
  sccs
;;

let show t =
  printf "Found %d SCCs\n" (List.length t);
  List.iteri t ~f:(fun i scc ->
    printf "SCC %d (%d cycles)\n" (i + 1) (List.length scc);
    List.iteri scc ~f:(fun j cycle ->
      printf "\tCycle %d: " (j + 1);
      print_endline (String.concat ~sep:" -> " cycle)))
;;

let%expect_test "test_traverse_graph_post_order" =
  let graph = Hashtbl.create (module String) in
  Hashtbl.add_exn graph ~key:"main" ~data:[ "a"; "utils.b" ];
  Hashtbl.add_exn graph ~key:"a" ~data:[ "main" ];
  Hashtbl.add_exn graph ~key:"utils.b" ~data:[ "utils.c" ];
  Hashtbl.add_exn graph ~key:"utils.c" ~data:[ "main" ];
  let root = "main" in
  let stack = traverse_graph_post_order ~root graph in
  print_s [%sexp (stack : string list)];
  [%expect {|(main utils.b utils.c a)|}];
  let transpose_graph = Import_graph.transpose graph in
  let sccs = find_strongly_connected_components ~stack transpose_graph in
  match sccs with
  | [] -> ()
  | scc :: _ ->
    print_endline "SSC:";
    let t = find_cycles_in_scc scc graph in
    print_s [%sexp (t : string list list)];
    [%expect {|((a main a) (main utils.b utils.c main))|}];
    show [ t ];
    [%expect
      {|
Found 1 SCCs
SCC 1 (2 cycles)
       Cycle 1: a -> main -> a
       Cycle 2: main -> utils.b -> utils.c -> main
    |}]
;;
