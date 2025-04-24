type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure | Branch of map * map | Guide of string * map

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

module KeyOrd = struct
  type t = key

  let rec compare a b =
    match (a, b) with
    | Bar, Bar -> 0
    | Bar, Node _ -> -1
    | Node _, Bar -> 1
    | Node (a1, b1), Node (a2, b2) ->
        let c = compare a1 a2 in
        if c <> 0 then c else compare b1 b2
end

module KeySet = Set.Make (KeyOrd)

exception IMPOSSIBLE

let rec count_use target m =
  match m with
  | End (NameBox x) -> if x = target then 1 else 0
  | End StarBox -> 0
  | Guide (_, m1) -> count_use target m1
  | Branch (m1, m2) -> count_use target m1 + count_use target m2

let rec getReady m =
  let rec aux m ctx seen =
    match m with
    | End StarBox -> (KeySet.singleton Bar, Bar)
    | End (NameBox name) -> (
        match StringMap.find_opt name ctx with
        | Some k -> (KeySet.singleton k, Bar)
        | None -> (KeySet.singleton Bar, Bar))
    | Guide (name, m1) ->
        if count_use name m1 >= 2 then raise IMPOSSIBLE;
        if StringSet.mem name seen then raise IMPOSSIBLE;
        let seen' = StringSet.add name seen in
        let _, k = aux m1 ctx seen' in
        let ctx' = StringMap.add name k ctx in
        aux m1 ctx' seen
    | Branch (m1, m2) -> (
        let keys1, k1 = aux m1 ctx seen in
        let keys2, k2 = aux m2 ctx seen in
        match (m1, m2) with
        | Guide _, End StarBox
        | End StarBox, Guide _
        | Guide _, Branch (Guide _, _)
        | Branch (Guide _, _), Guide _ ->
            (KeySet.singleton Bar, Bar)
        | Guide _, Guide _ ->
            let node_key = Node (k1, k2) in
            (KeySet.add Bar (KeySet.singleton node_key), Bar)
        | _ ->
            let node_key = Node (k1, k2) in
            (KeySet.union keys1 keys2, node_key))
  in
  let keys, root_key = aux m StringMap.empty StringSet.empty in
  let final_keys = KeySet.add root_key keys in
  KeySet.elements final_keys
