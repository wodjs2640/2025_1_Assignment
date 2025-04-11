type require = id * cond list

and cond =
  | Items of gift list
  | Same of id
  | Common of cond * cond
  | Except of cond * cond

and gift = int
and id = A | B | C | D | E

let rec collect_gifts c =
  match c with
  | Items gifts -> gifts
  | Same _ -> []
  | Common (c1, c2) | Except (c1, c2) -> collect_gifts c1 @ collect_gifts c2

let collect_all_items reqs =
  reqs
  |> List.map (fun (_id, conds) -> List.map collect_gifts conds |> List.flatten)
  |> List.flatten |> List.sort_uniq compare

let create_gift_map gifts =
  let map = Hashtbl.create (List.length gifts) in
  List.iteri (fun idx gift -> Hashtbl.add map gift idx) gifts;
  map

let rec mark_gifts gifts arr gift_map =
  List.iter (fun g -> arr.(Hashtbl.find gift_map g) <- true) gifts

let rec process_cond id_map id conds arr gift_map =
  List.iter
    (fun cond ->
      match cond with
      | Items gifts -> mark_gifts gifts arr gift_map
      | Same other_id ->
          if Hashtbl.mem id_map other_id then
            let other_arr = Hashtbl.find id_map other_id in
            for i = 0 to Array.length arr - 1 do
              if other_arr.(i) then arr.(i) <- true
            done
      | Common (c1, c2) ->
          let arr1 = Array.copy arr in
          let arr2 = Array.copy arr in
          process_cond id_map id [ c1 ] arr1 gift_map;
          process_cond id_map id [ c2 ] arr2 gift_map;
          for i = 0 to Array.length arr - 1 do
            arr.(i) <- arr1.(i) && arr2.(i)
          done
      | Except (c1, c2) ->
          let arr1 = Array.copy arr in
          let arr2 = Array.copy arr in
          process_cond id_map id [ c1 ] arr1 gift_map;
          process_cond id_map id [ c2 ] arr2 gift_map;
          for i = 0 to Array.length arr - 1 do
            arr.(i) <- arr1.(i) && not arr2.(i)
          done)
    conds

let get_marked_gifts arr gift_map =
  let reverse_map = Hashtbl.create (Array.length arr) in
  Hashtbl.iter (fun gift idx -> Hashtbl.add reverse_map idx gift) gift_map;
  arr |> Array.to_list
  |> List.mapi (fun idx r ->
         if r then Some (Hashtbl.find reverse_map idx) else None)
  |> List.filter_map (fun x -> x)

exception No_minimum

let create_dependency_graph reqs =
  let graph = Hashtbl.create (List.length reqs) in
  let add_edge from_id to_id =
    if not (Hashtbl.mem graph from_id) then Hashtbl.add graph from_id [];
    let edges = Hashtbl.find graph from_id in
    if not (List.mem to_id edges) then
      Hashtbl.replace graph from_id (to_id :: edges)
  in
  let rec process_cond id cond =
    match cond with
    | Items _ -> ()
    | Same other_id -> add_edge id other_id
    | Common (c1, c2) | Except (c1, c2) ->
        process_cond id c1;
        process_cond id c2
  in
  List.iter
    (fun (id, conds) ->
      Hashtbl.add graph id [];
      List.iter (process_cond id) conds)
    reqs;
  graph

let has_cycle graph =
  let visited = Hashtbl.create (Hashtbl.length graph) in
  let rec_detect = Hashtbl.create (Hashtbl.length graph) in
  let has_cycle = ref false in
  let rec dfs id =
    if Hashtbl.mem rec_detect id then has_cycle := true
    else if not (Hashtbl.mem visited id) then (
      Hashtbl.add visited id ();
      Hashtbl.add rec_detect id ();
      List.iter dfs (Hashtbl.find graph id);
      Hashtbl.remove rec_detect id)
  in
  List.iter dfs (Hashtbl.fold (fun k _ acc -> k :: acc) graph []);
  !has_cycle

let can_satisfy_with_empty reqs =
  let rec check_cond cond =
    match cond with
    | Items gifts -> gifts = []
    | Same _ -> true
    | Common (c1, c2) -> check_cond c1 && check_cond c2
    | Except (c1, c2) -> true
  in
  List.for_all (fun (_, conds) -> List.for_all check_cond conds) reqs

let topological_sort graph reqs =
  let visited = Hashtbl.create (Hashtbl.length graph) in
  let temp = Hashtbl.create (Hashtbl.length graph) in
  let order = ref [] in
  let has_cycle = ref false in
  let rec visit id =
    if Hashtbl.mem temp id then has_cycle := true
    else if not (Hashtbl.mem visited id) then (
      Hashtbl.add temp id ();
      List.iter visit (Hashtbl.find graph id);
      Hashtbl.remove temp id;
      Hashtbl.add visited id ();
      order := id :: !order)
  in
  List.iter visit (Hashtbl.fold (fun k _ acc -> k :: acc) graph []);
  if !has_cycle then [] else !order

let rec evaluate_cond id_map cond =
  match cond with
  | Items gifts -> gifts
  | Same other_id -> (
      match Hashtbl.find_opt id_map other_id with
      | Some gifts -> gifts
      | None -> [])
  | Common (c1, c2) ->
      let gifts1 = evaluate_cond id_map c1 in
      let gifts2 = evaluate_cond id_map c2 in
      List.filter (fun g -> List.mem g gifts2) gifts1
  | Except (c1, c2) ->
      let gifts1 = evaluate_cond id_map c1 in
      let gifts2 = evaluate_cond id_map c2 in
      List.filter (fun g -> not (List.mem g gifts2)) gifts1

let shoppingList reqs =
  let dependency_graph = create_dependency_graph reqs in
  let sorted_ids =
    let ids = topological_sort dependency_graph reqs in
    if ids = [] then Hashtbl.fold (fun k _ acc -> k :: acc) dependency_graph []
    else ids
  in
  let id_map = Hashtbl.create (List.length reqs) in

  List.iter (fun id -> Hashtbl.add id_map id []) sorted_ids;

  let process_id id =
    let conds =
      match List.find_opt (fun (req_id, _) -> req_id = id) reqs with
      | Some (_, conds) -> conds
      | None -> []
    in
    let gifts =
      List.fold_left
        (fun acc cond ->
          let new_gifts = evaluate_cond id_map cond in
          List.sort_uniq compare (acc @ new_gifts))
        [] conds
    in
    (id, gifts)
  in

  let rec process_until_stable iterations =
    if iterations > 100 then raise No_minimum;
    let changed = ref false in
    let new_results = List.map process_id sorted_ids in
    List.iter2
      (fun id new_gifts ->
        let old_gifts = Hashtbl.find id_map id in
        if
          List.length old_gifts <> List.length new_gifts
          || not
               (List.for_all2 ( = )
                  (List.sort compare old_gifts)
                  (List.sort compare new_gifts))
        then (
          changed := true;
          Hashtbl.replace id_map id new_gifts))
      sorted_ids (List.map snd new_results);
    if !changed then process_until_stable (iterations + 1)
  in

  try
    process_until_stable 0;

    List.sort
      (fun (id1, _) (id2, _) ->
        match (id1, id2) with
        | A, A -> 0
        | A, _ -> -1
        | _, A -> 1
        | B, B -> 0
        | B, _ -> -1
        | _, B -> 1
        | C, C -> 0
        | C, _ -> -1
        | _, C -> 1
        | D, D -> 0
        | D, _ -> -1
        | _, D -> 1
        | E, E -> 0)
      (List.map (fun id -> (id, Hashtbl.find id_map id)) sorted_ids)
  with _ -> raise No_minimum
