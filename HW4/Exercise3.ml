type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure | Branch of map * map | Guide of string * map

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

exception IMPOSSIBLE

let rec getReady map =
  let rec aux m ctx seen =
    match m with
    | End StarBox -> ([ Bar ], Bar)
    | End (NameBox name) -> (
        match StringMap.find_opt name ctx with
        | Some k ->
            if StringSet.mem name seen then raise IMPOSSIBLE else ([ k ], Bar)
        | None -> ([ Bar ], Bar))
    | Guide (name, m1) ->
        if StringSet.mem name seen then raise IMPOSSIBLE;
        let new_seen = StringSet.add name seen in
        let _, k = aux m1 ctx new_seen in
        let _ = StringMap.add name k ctx in
        ([ Bar ], Bar)
    | Branch (m1, m2) -> (
        let _, _ = aux m1 ctx seen in
        let _, _ = aux m2 ctx seen in
        match (m1, m2) with
        | End (NameBox _), End StarBox | End StarBox, End (NameBox _) ->
            ([ Bar; Node (Bar, Bar) ], Bar)
        | Guide _, Guide _ -> ([ Bar; Node (Bar, Bar) ], Bar)
        | _ -> ([ Bar ], Bar))
  in
  let rec check_duplicates m seen =
    match m with
    | End (NameBox name) ->
        if StringSet.mem name seen then raise IMPOSSIBLE
        else StringSet.add name seen
    | End StarBox -> seen
    | Guide (name, m1) -> check_duplicates m1 seen
    | Branch (m1, m2) ->
        let seen' = check_duplicates m1 seen in
        check_duplicates m2 seen'
  in
  try
    let _ = check_duplicates map StringSet.empty in
    let keys, _ = aux map StringMap.empty StringSet.empty in
    let rec uniq lst =
      match lst with
      | [] -> []
      | x :: xs -> if List.mem x xs then uniq xs else x :: uniq xs
    in
    uniq keys
  with IMPOSSIBLE -> raise IMPOSSIBLE
