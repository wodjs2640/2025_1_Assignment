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
  match gifts with
  | g :: rest ->
      arr.(g) := true;
      mark_gifts rest arr
  | [] -> ()

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
  let a_conds, b_conds, c_conds, d_conds, e_conds =
    match reqs with
    | [ (_, a); (_, b); (_, c); (_, d); (_, e) ] -> (a, b, c, d, e)
    | _ -> failwith "Invalid Input"
  in
  let all_gifts = collect_all_items reqs in
  let gift_count = List.length all_gifts in
  let a_arr = Array.make gift_count (ref false) in
  let b_arr = Array.make gift_count (ref false) in
  let c_arr = Array.make gift_count (ref false) in
  let d_arr = Array.make gift_count (ref false) in
  let e_arr = Array.make gift_count (ref false) in

  process_cond a_conds a_arr;
  process_cond b_conds b_arr;
  process_cond c_conds c_arr;
  process_cond d_conds d_arr;
  process_cond e_conds e_arr;

  let a_gifts = get_marked_gifts a_arr in
  let b_gifts = get_marked_gifts b_arr in
  let c_gifts = get_marked_gifts c_arr in
  let d_gifts = get_marked_gifts d_arr in
  let e_gifts = get_marked_gifts e_arr in

  [ (A, a_gifts); (B, b_gifts); (C, c_gifts); (D, d_gifts); (E, e_gifts) ]
