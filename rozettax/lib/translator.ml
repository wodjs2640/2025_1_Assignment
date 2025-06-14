(*
 * SNU 4190.310 Programming Languages 
 * Homework "RozettaX" Skeleton
 *)

let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z -> Sonata.Z z
  | Sm5.B b -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

let rec trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  | Sm5.Fn (arg, cmd) -> Sonata.Fn (arg, trans cmd)

and trans_cmd = function
  | Sm5.PUSH obj -> [ Sonata.PUSH (trans_obj obj) ]
  | Sm5.POP -> [ Sonata.POP ]
  | Sm5.STORE -> [ Sonata.STORE ]
  | Sm5.LOAD -> [ Sonata.LOAD ]
  | Sm5.JTR (c1, c2) -> [ Sonata.JTR (trans c1, trans c2) ]
  | Sm5.MALLOC -> [ Sonata.MALLOC ]
  | Sm5.BOX z -> [ Sonata.BOX z ]
  | Sm5.UNBOX id -> [ Sonata.UNBOX id ]
  | Sm5.BIND id -> [ Sonata.BIND id ]
  | Sm5.UNBIND -> [ Sonata.UNBIND ]
  | Sm5.GET -> [ Sonata.GET ]
  | Sm5.PUT -> [ Sonata.PUT ]
  | Sm5.CALL -> [ Sonata.CALL ]
  | Sm5.ADD -> [ Sonata.ADD ]
  | Sm5.SUB -> [ Sonata.SUB ]
  | Sm5.MUL -> [ Sonata.MUL ]
  | Sm5.DIV -> [ Sonata.DIV ]
  | Sm5.EQ -> [ Sonata.EQ ]
  | Sm5.LESS -> [ Sonata.LESS ]
  | Sm5.NOT -> [ Sonata.NOT ]

and trans (command : Sm5.command) : Sonata.command =
  List.flatten (List.map trans_cmd command)
