type require = id * cond list

and cond =
  | Items of gift list (* gifts *)
  | Same of id (* same gifts as for id *)
  | Common of cond * cond (* common gifts *)
  | Except of cond * cond (* exclude gifts *)

and gift = int (* gift id *)
and id = A | B | C | D | E (* pig names *)

let shoppingList (rl : require list) : (id * gift list) list = []
