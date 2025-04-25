(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * Homework "SM5"
 *)

(* TODO : complete this function *)
let rec trans : K.program -> Machine.command = function
  | K.NUM i -> [ Machine.PUSH (Machine.Val (Machine.Z i)) ]
  | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [ Machine.ADD ]
  | K.LETV (x, e1, e2) ->
      trans e1
      @ [
          Machine.MALLOC;
          Machine.BIND x;
          Machine.PUSH (Machine.Id x);
          Machine.STORE;
        ]
      @ trans e2
      @ [ Machine.UNBIND; Machine.POP ]
  | K.READ x ->
      [
        Machine.GET;
        Machine.PUSH (Machine.Id x);
        Machine.STORE;
        Machine.PUSH (Machine.Id x);
        Machine.LOAD;
      ]
  | _ -> failwith "Unimplemented"
