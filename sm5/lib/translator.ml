(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * Homework "SM5"
 *)

let rec trans : K.program -> Machine.command = function
  | K.NUM i ->
      Printf.printf "Pushing NUM: %d\n" i;
      flush stdout;
      [ Machine.PUSH (Machine.Val (Machine.Z i)) ]
  | K.TRUE ->
      Printf.printf "Pushing TRUE\n";
      flush stdout;
      [ Machine.PUSH (Machine.Val (Machine.B true)) ]
  | K.FALSE ->
      Printf.printf "Pushing FALSE\n";
      flush stdout;
      [ Machine.PUSH (Machine.Val (Machine.B false)) ]
  | K.UNIT ->
      Printf.printf "Pushing UNIT\n";
      flush stdout;
      [ Machine.PUSH (Machine.Val Machine.Unit) ]
  | K.VAR x ->
      Printf.printf "Pushing VAR: %s\n" x;
      flush stdout;
      [ Machine.PUSH (Machine.Id x); Machine.LOAD ]
  | K.ADD (e1, e2) ->
      Printf.printf "Translating ADD\n";
      let t1 = trans e1 in
      let t2 = trans e2 in
      Printf.printf "Before ADD: \n";
      t1 @ t2 @ [ Machine.ADD ]
  | K.SUB (e1, e2) ->
      Printf.printf "Translating SUB\n";
      trans e1 @ trans e2 @ [ Machine.SUB ]
  | K.MUL (e1, e2) ->
      Printf.printf "Translating MUL\n";
      trans e1 @ trans e2 @ [ Machine.MUL ]
  | K.DIV (e1, e2) ->
      Printf.printf "Translating DIV\n";
      trans e1 @ trans e2 @ [ Machine.DIV ]
  | K.EQUAL (e1, e2) ->
      Printf.printf "Translating EQUAL\n";
      trans e1 @ trans e2 @ [ Machine.EQ ]
  | K.LESS (e1, e2) ->
      Printf.printf "Translating LESS\n";
      trans e1 @ trans e2 @ [ Machine.LESS ]
  | K.NOT e ->
      Printf.printf "Translating NOT\n";
      trans e @ [ Machine.NOT ]
  | K.ASSIGN (x, e) ->
      Printf.printf "Translating ASSIGN for: %s\n" x;
      trans e
      @ [ Machine.PUSH (Machine.Id x); Machine.STORE ]
      @ [ Machine.PUSH (Machine.Val Machine.Unit) ]
  | K.SEQ (e1, e2) ->
      Printf.printf "Translating SEQ\n";
      trans e1 @ [ Machine.POP ] @ trans e2
  | K.IF (e1, e2, e3) ->
      Printf.printf "Translating IF\n";
      trans e1 @ [ Machine.JTR (trans e2, trans e3) ]
  | K.WHILE (e1, e2) ->
      Printf.printf "Translating WHILE\n";
      let c1 = trans e1 in
      let c2 = trans e2 in
      let loop = [ Machine.JTR (c2 @ [ Machine.POP ] @ c1, []) ] in
      c1 @ loop
  | K.FOR (x, e1, e2, e3) ->
      Printf.printf "Translating FOR\n";
      let c1 = trans e1 in
      let c2 = trans e2 in
      let c3 = trans e3 in
      [ Machine.MALLOC ] @ c1
      @ [ Machine.BIND x; Machine.PUSH (Machine.Id x); Machine.STORE ]
      @ c2
      @ [ Machine.PUSH (Machine.Id x); Machine.LOAD; Machine.LESS ]
      @ [
          Machine.JTR
            ( c3
              @ [
                  Machine.PUSH (Machine.Id x);
                  Machine.LOAD;
                  Machine.PUSH (Machine.Val (Machine.Z 1));
                  Machine.ADD;
                  Machine.PUSH (Machine.Id x);
                  Machine.STORE;
                ]
              @ c2
              @ [ Machine.PUSH (Machine.Id x); Machine.LOAD; Machine.LESS ]
              @ [ Machine.JTR (c3, []) ],
              [] );
        ]
      @ [ Machine.UNBIND ]
      @ [ Machine.PUSH (Machine.Val Machine.Unit) ]
  | K.LETV (x, e1, e2) ->
      Printf.printf "Translating LETV for: %s\n" x;
      let t1 = trans e1 in
      Printf.printf "After translating e1\n";
      let t2 =
        [
          Machine.MALLOC;
          Machine.BIND x;
          Machine.PUSH (Machine.Id x);
          Machine.STORE;
        ]
      in
      Printf.printf "After allocating memory and binding variable\n";
      let t3 = trans e2 in
      Printf.printf "After translating e2\n";
      t1 @ t2 @ t3 @ [ Machine.UNBIND; Machine.POP ]
  | K.LETF (f, x, e1, e2) ->
      Printf.printf "Translating LETF for: %s\n" f;
      [ Machine.PUSH (Machine.Fn (x, trans e1)) ]
      @ [ Machine.BIND f ] @ trans e2 @ [ Machine.UNBIND ]
  | K.CALLV (f, e) ->
      Printf.printf "Translating CALLV for: %s\n" f;
      [ Machine.PUSH (Machine.Id f); Machine.LOAD ]
      @ [ Machine.MALLOC ] @ trans e @ [ Machine.CALL ]
      @ [ Machine.PUSH (Machine.Val Machine.Unit) ]
  | K.CALLR (f, x) ->
      Printf.printf "Translating CALLR for: %s\n" f;
      [ Machine.PUSH (Machine.Id x) ]
      @ [ Machine.PUSH (Machine.Id f); Machine.LOAD; Machine.CALL ]
  | K.READ x ->
      Printf.printf "Translating READ for: %s\n" x;
      [
        Machine.GET;
        Machine.PUSH (Machine.Id x);
        Machine.STORE;
        Machine.PUSH (Machine.Id x);
        Machine.LOAD;
      ]
  | K.WRITE e ->
      Printf.printf "Translating WRITE\n";
      trans e @ [ Machine.PUT ]
