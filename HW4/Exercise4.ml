type require = id * cond list

and cond =
  | Items of gift list (* gifts *)
  | Same of id (* same gifts as for id *)
  | Common of cond * cond (* common gifts *)
  | Except of cond * cond (* exclude gifts *)

and gift = int (* gift id *)
and id = A | B | C | D | E (* pig names *)

let rec collect_gifts (c : cond) : gift list =
  match c with
  | Items gifts -> gifts
  | Same _ -> []
  | Common (c1, c2) | Except (c1, c2) -> collect_gifts c1 @ collect_gifts c2

let collect_all_items (reqs : require list) : gift list =
  List.flatten
    (List.map
       (fun (_id, conds) -> List.flatten (List.map collect_gifts conds))
       reqs)

let rec mark_gifts (gifts : gift list) (arr : bool ref array) =
  List.iter (fun g -> arr.(g) := true) gifts

let rec process_cond (conds : cond list) (arr : bool ref array) =
  List.iter
    (fun cond ->
      match cond with
      | Items gifts -> mark_gifts gifts arr
      | Same _ -> ()
      | Common (c1, c2) | Except (c1, c2) -> process_cond [ c1; c2 ] arr)
    conds

let get_marked_gifts arr =
  arr |> Array.to_list
  |> List.mapi (fun idx r -> if !r then Some idx else None)
  |> List.filter_map (fun x -> x)

let shoppingList (reqs : require list) : (id * gift list) list =
  let all_ids = [ A; B; C; D; E ] in
  let all_gifts = collect_all_items reqs in
  let gift_count = List.length all_gifts in
  let process_id id =
    let conds =
      match List.find_opt (fun (req_id, _) -> req_id = id) reqs with
      | Some (_, conds) -> conds
      | None -> failwith "Invalid Input"
    in
    let arr = Array.make gift_count (ref false) in
    process_cond conds arr;
    (id, get_marked_gifts arr)
  in
  List.map process_id all_ids

let () =
  let rl =
    [
      (A, []);
      (B, [ Items [ 1; 2 ] ]);
      (C, [ Items [ 3 ] ]);
      (D, [ Items [ 2; 3 ] ]);
      (E, []);
    ]
  in
  let ans = shoppingList rl in
  let print_gift_list id gifts =
    Printf.printf "%s, [" (match id with A -> "A" | B -> "B" | C -> "C" | D -> "D" | E -> "E");
    List.iter (fun g -> Printf.printf "%d " g) gifts;
    print_endline "]"
  in
  List.iter (fun (id, gifts) -> print_gift_list id gifts) ans
