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
      let for_condition = ref [] in
      let for_body =
        [
          Machine.PUSH (Machine.Id ("#for_" ^ x));
          Machine.LOAD;
          Machine.PUSH (Machine.Id x);
          Machine.STORE;
          Machine.PUSH (Machine.Id "s");
          Machine.LOAD;
          Machine.PUSH (Machine.Id x);
          Machine.LOAD;
          Machine.ADD;
          Machine.PUSH (Machine.Id "s");
          Machine.STORE;
          Machine.PUSH (Machine.Id "s");
          Machine.LOAD;
          Machine.PUT;
          Machine.PUSH (Machine.Id x);
          Machine.LOAD;
          Machine.PUT;
          Machine.PUSH (Machine.Id ("#for_" ^ x));
          Machine.LOAD;
          Machine.PUSH (Machine.Val (Machine.Z 1));
          Machine.ADD;
          Machine.PUSH (Machine.Id ("#for_" ^ x));
          Machine.STORE;
        ]
      in
      for_condition :=
        [
          Machine.PUSH (Machine.Id ("#for_" ^ x));
          Machine.LOAD;
          Machine.PUSH (Machine.Id ("#for_end_" ^ x));
          Machine.LOAD;
          Machine.EQ;
          Machine.JTR
            ( for_body @ [ Machine.PUSH (Machine.Val Machine.Unit) ],
              [
                Machine.PUSH (Machine.Id ("#for_" ^ x));
                Machine.LOAD;
                Machine.PUSH (Machine.Id ("#for_end_" ^ x));
                Machine.LOAD;
                Machine.LESS;
                Machine.JTR
                  ( for_body @ !for_condition,
                    [ Machine.PUSH (Machine.Val Machine.Unit) ] );
              ] );
        ];
      trans e1
      @ [
          Machine.MALLOC;
          Machine.BIND ("#for_" ^ x);
          Machine.PUSH (Machine.Id ("#for_" ^ x));
          Machine.STORE;
        ]
      @ trans e2
      @ [
          Machine.MALLOC;
          Machine.BIND ("#for_end_" ^ x);
          Machine.PUSH (Machine.Id ("#for_end_" ^ x));
          Machine.STORE;
        ]
      @ !for_condition
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
      [
        Machine.PUSH
          (Machine.Fn (x, trans e1 @ [ Machine.UNBIND; Machine.POP ]));
        Machine.BIND f;
        Machine.PUSH (Machine.Id f);
        Machine.BIND f;
      ]
      @ trans e2
      @ [ Machine.UNBIND; Machine.POP; Machine.UNBIND; Machine.POP ]
  | K.CALLV (f, e) ->
      let result =
        trans e @ [ Machine.MALLOC ] @ [ Machine.BIND "#tmp" ]
        @ [ Machine.PUSH (Machine.Id "#tmp") ]
        @ [ Machine.STORE ]
        @ [ Machine.PUSH (Machine.Id f) ]
        @ [ Machine.PUSH (Machine.Id "#tmp") ]
        @ [ Machine.LOAD ]
        @ [ Machine.PUSH (Machine.Id "#tmp") ]
        @ [ Machine.CALL ]
        @ [ Machine.UNBIND; Machine.POP ]
      in
      result
  | K.CALLR (f, x) ->
      [
        Machine.PUSH (Machine.Id x);
        Machine.MALLOC;
        Machine.BIND "#tmp";
        Machine.PUSH (Machine.Id "#tmp");
        Machine.STORE;
        Machine.PUSH (Machine.Id f);
        Machine.PUSH (Machine.Id "#tmp");
        Machine.LOAD;
        Machine.PUSH (Machine.Id "#tmp");
        Machine.CALL;
        Machine.PUSH (Machine.Id x);
        Machine.STORE;
        Machine.PUSH (Machine.Id x);
        Machine.LOAD;
        Machine.UNBIND;
        Machine.POP;
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
