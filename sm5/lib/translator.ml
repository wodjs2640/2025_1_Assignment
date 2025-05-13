(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * Homework "SM5"
 *)

open K
open Machine

(* TODO : complete this function *)
let rec trans : K.program -> Machine.command = function
  | K.NUM i -> [ Machine.PUSH (Machine.Val (Machine.Z i)) ]
  | K.TRUE -> [ Machine.PUSH (Machine.Val (Machine.B true)) ]
  | K.FALSE -> [ Machine.PUSH (Machine.Val (Machine.B false)) ]
  | K.UNIT -> [ Machine.PUSH (Machine.Val Machine.Unit) ]
  | K.VAR x -> [ Machine.PUSH (Machine.Id x); Machine.LOAD ]
  | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [ Machine.ADD ]
  | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [ Machine.SUB ]
  | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [ Machine.MUL ]
  | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [ Machine.DIV ]
  | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [ Machine.EQ ]
  | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [ Machine.LESS ]
  | K.NOT e -> trans e @ [ Machine.NOT ]
  | K.ASSIGN (x, e) ->
      trans e
      @ [ Machine.PUSH (Machine.Id x); Machine.STORE ]
      @ [ Machine.PUSH (Machine.Id x); Machine.LOAD ]
  | K.SEQ (e1, e2) -> (
      match e1 with
      | K.ASSIGN _ | K.WRITE _ | K.UNIT -> trans e1 @ [ Machine.POP ] @ trans e2
      | _ -> trans e1 @ trans e2)
  | K.IF (e1, e2, e3) -> trans e1 @ [ Machine.JTR (trans e2, trans e3) ]
  | K.WHILE (e1, e2) ->
      trans e1
      @ [
          Machine.JTR
            ( trans e2 @ [ Machine.POP ] @ trans (K.WHILE (e1, e2)),
              [ Machine.PUSH (Machine.Val Machine.Unit) ] );
        ]
  | K.FOR (x, e1, e2, e3) ->
      trans e1
      @ [
          Machine.MALLOC;
          Machine.BIND x;
          Machine.PUSH (Machine.Id x);
          Machine.STORE;
        ]
      @ trans e2
      @ [
          Machine.MALLOC;
          Machine.BIND ("#for_end_" ^ x);
          Machine.PUSH (Machine.Id ("#for_end_" ^ x));
          Machine.STORE;
        ]
      @ trans
          (K.WHILE
             ( K.LESS (K.VAR x, K.ADD (K.VAR ("#for_end_" ^ x), K.NUM 1)),
               K.SEQ (e3, K.ASSIGN (x, K.ADD (K.VAR x, K.NUM 1))) ))
      @ [ Machine.UNBIND; Machine.POP; Machine.UNBIND; Machine.POP ]
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
  | K.LETF (f, x, e1, e2) ->
      [ Machine.PUSH (Machine.Fn (x, trans e1)) ]
      @ [ Machine.BIND f ] @ trans e2
      @ [ Machine.UNBIND; Machine.POP ]
  | K.CALLV (f, e) ->
      trans e (* [value] *)
      @ [ Machine.MALLOC ] (* [value; loc] *)
      @ [ Machine.BIND "#tmp" ] (* [value] *)
      @ [ Machine.PUSH (Machine.Id "#tmp") ]
      @ [ Machine.STORE ] (* [] *)
      @ [ Machine.PUSH (Machine.Id f) ]
      @ [ Machine.LOAD ] (* [proc] *)
      @ [ Machine.PUSH (Machine.Id "#tmp") ]
      @ [ Machine.LOAD ] (* [proc; value] *)
      @ [ Machine.PUSH (Machine.Id "#tmp") ]
      @ [ Machine.LOAD ] (* [proc; value; loc] *)
      @ [ Machine.UNBIND ] @ [ Machine.POP ] @ [ Machine.CALL ]
  | K.CALLR (f, x) ->
      [
        Machine.PUSH (Machine.Id x);
        Machine.PUSH (Machine.Id f);
        Machine.LOAD;
        Machine.CALL;
      ]
  | K.READ x ->
      [
        Machine.GET;
        Machine.PUSH (Machine.Id x);
        Machine.STORE;
        Machine.PUSH (Machine.Id x);
        Machine.LOAD;
      ]
  | K.WRITE e -> trans e @ [ Machine.PUT ]
