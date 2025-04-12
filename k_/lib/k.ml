(*
 * SNU 4190.310 Programming Languages 2025 Spring
 *  K- Interpreter
 *)

(** Location Signature *)
module type LOC = sig
  type t

  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC = struct
  type t = Location of int

  let base = Location 0
  let equal (Location a) (Location b) = a = b
  let diff (Location a) (Location b) = a - b
  let increase (Location base) n = Location (base + n)
end

(** Memory Signature *)
module type MEM = sig
  type 'a t

  exception Not_allocated
  exception Not_initialized

  val empty : 'a t
  (** get empty memory *)

  val load : 'a t -> Loc.t -> 'a
  (** load value : Mem.load mem loc => value *)

  val store : 'a t -> Loc.t -> 'a -> 'a t
  (** save value : Mem.store mem loc value => mem' *)

  val alloc : 'a t -> Loc.t * 'a t
  (** get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(** Environment Signature *)
module type ENV = sig
  type ('a, 'b) t

  exception Not_bound

  val empty : ('a, 'b) t
  (** get empty environment *)

  val lookup : ('a, 'b) t -> 'a -> 'b
  (** lookup environment : Env.lookup env key => content *)

  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (** id binding : Env.bind env key content => env'*)
end

(** Memory Implementation *)
module Mem : MEM = struct
  exception Not_allocated
  exception Not_initialized

  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list

  let empty = M (Loc.base, [])

  let rec replace_nth l n c =
    match l with
    | h :: t -> if n = 1 then c :: t else h :: replace_nth t (n - 1) c
    | [] -> raise Not_allocated

  let load (M (boundary, storage)) loc =
    match List.nth storage (Loc.diff boundary loc - 1) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary, storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary, storage)) =
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(** Environment Implementation *)
module Env : ENV = struct
  exception Not_bound

  type ('a, 'b) t = E of ('a -> 'b)

  let empty = E (fun x -> raise Not_bound)
  let lookup (E env) id = env id
  let bind (E env) id loc = E (fun x -> if x = id then loc else env x)
end

(**  K- Interpreter *)
module type KMINUS = sig
  exception Error of string

  type id = string

  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp  (** sequence *)
    | IF of exp * exp * exp  (** if-then-else *)
    | WHILE of exp * exp  (** while loop *)
    | LETV of id * exp * exp  (** variable binding *)
    | LETF of id * id list * exp * exp  (** procedure binding *)
    | CALLV of id * exp list  (** call by value *)
    | CALLR of id * id list  (** call by referenece *)
    | RECORD of (id * exp) list  (** record construction *)
    | FIELD of exp * id  (** access record field *)
    | ASSIGN of id * exp  (** assgin to variable *)
    | ASSIGNF of exp * id * exp  (** assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type memory
  type env
  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)

  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS = struct
  exception Error of string

  type id = string

  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp  (** sequence *)
    | IF of exp * exp * exp  (** if-then-else *)
    | WHILE of exp * exp  (** while loop *)
    | LETV of id * exp * exp  (** variable binding *)
    | LETF of id * id list * exp * exp  (** procedure binding *)
    | CALLV of id * exp list  (** call by value *)
    | CALLR of id * id list  (** call by referenece *)
    | RECORD of (id * exp) list  (** record construction *)
    | FIELD of exp * id  (** access record field *)
    | ASSIGN of id * exp  (** assgin to variable *)
    | ASSIGNF of exp * id * exp  (** assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)
  type memory = value Mem.t

  type env = (id, env_entry) Env.t
  and env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with Num n -> n | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with Bool b -> b | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with Unit -> () | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with Record r -> r | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc")
      | Proc (id_list, exp, env) -> (id_list, exp, env)
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | READ x ->
        let v = Num (read_int ()) in
        let l = lookup_env_loc env x in
        (v, Mem.store mem l v)
    | WRITE e ->
        let v, mem' = eval mem env e in
        let n = value_int v in
        let _ = print_endline (string_of_int n) in
        (v, mem')
    | LETV (x, e1, e2) ->
        let v, mem' = eval mem env e1 in
        let l, mem'' = Mem.alloc mem' in
        eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
        let v, mem' = eval mem env e in
        let l = lookup_env_loc env x in
        (v, Mem.store mem' l v)
    | LETF (f, id_list, e1, e2) ->
        let rec_env = ref env in
        let proc = Proc (id_list, e1, !rec_env) in
        let env' = Env.bind env f proc in
        rec_env := env';
        eval mem env' e2
    | CALLV (f, e_list) ->
        let id_list, exp, env' =
          try
            match Env.lookup env f with
            | Addr _ -> raise (Error "TypeError : not proc")
            | Proc (id_list, exp, env) as p ->
                let env' = Env.bind env f p in
                (id_list, exp, env')
          with Env.Not_bound -> raise (Error "Unbound")
        in
        if List.length id_list <> List.length e_list then
          raise (Error "InvalidArg");
        let v_list, mem' =
          List.fold_left
            (fun (acc, m) e ->
              let v, m' = eval m env e in
              (v :: acc, m'))
            ([], mem) e_list
        in
        let env'', mem'' =
          List.fold_left2
            (fun (acc_env, acc_mem) id v ->
              let l, new_mem = Mem.alloc acc_mem in
              let final_mem = Mem.store new_mem l v in
              (Env.bind acc_env id (Addr l), final_mem))
            (env', mem') id_list (List.rev v_list)
        in
        eval mem'' env'' exp
    | CALLR (f, x_list) ->
        let id_list, exp, env' =
          match lookup_env_proc env f with
          | exception Not_found -> raise (Error "Unbound")
          | res -> res
        in
        if List.length id_list <> List.length x_list then
          raise (Error "InvalidArg");
        let l_list = List.map (lookup_env_loc env) x_list in
        let env'' =
          List.fold_left2
            (fun acc_env id l -> Env.bind acc_env id (Addr l))
            env' id_list l_list
        in
        eval mem env'' exp
    | VAR x -> (
        match Env.lookup env x with
        | Addr l -> (Mem.load mem l, mem)
        | _ -> raise (Error "TypeError: Addr expected"))
    | ADD (e1, e2) -> (
        let v1, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        match (v1, v2) with
        | Num n1, Num n2 -> (Num (n1 + n2), mem'')
        | _ -> raise (Error "TypeError: not int"))
    | SUB (e1, e2) -> (
        let v1, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        match (v1, v2) with
        | Num n1, Num n2 -> (Num (n1 - n2), mem'')
        | _ -> raise (Error "TypeError: not int"))
    | IF (e1, e2, e3) -> (
        let v, mem' = eval mem env e1 in
        match v with
        | Bool true -> eval mem' env e2
        | Bool false -> eval mem' env e3
        | _ -> raise (Error "TypeError: boolean expected"))
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | NUM n -> (Num n, mem)
    | MUL (e1, e2) ->
        let v1, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        (Num (value_int v1 * value_int v2), mem'')
    | DIV (e1, e2) ->
        let v1, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        if value_int v2 = 0 then raise (Error "Division by zero")
        else (Num (value_int v1 / value_int v2), mem'')
    | EQUAL (e1, e2) ->
        let v1, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        (Bool (v1 = v2), mem'')
    | LESS (e1, e2) ->
        let v1, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        (Bool (value_int v1 < value_int v2), mem'')
    | NOT e ->
        let v, mem' = eval mem env e in
        (Bool (not (value_bool v)), mem')
    | SEQ (e1, e2) ->
        let _, mem' = eval mem env e1 in
        eval mem' env e2
    | WHILE (e1, e2) ->
        let rec loop mem env =
          let v, mem' = eval mem env e1 in
          if value_bool v then
            let _, mem'' = eval mem' env e2 in
            loop mem'' env
          else (Unit, mem')
        in
        loop mem env
    | RECORD e_list ->
        let v_list, mem' =
          List.fold_left
            (fun (acc, m) (id, e) ->
              let v, m' = eval m env e in
              let l, m'' = Mem.alloc m' in
              let m''' = Mem.store m'' l v in
              ((id, l) :: acc, m'''))
            ([], mem) e_list
        in
        let record =
         fun id ->
          try List.assoc id (List.rev v_list)
          with Not_found -> raise (Error "Field not found")
        in
        (Record record, mem')
    | FIELD (e, id) -> (
        let v, mem' = eval mem env e in
        match v with
        | Record r -> (
            try (Mem.load mem' (r id), mem')
            with Mem.Not_initialized -> raise (Error "Field not initialized"))
        | _ -> raise (Error "TypeError : not record"))
    | ASSIGNF (e1, id, e2) ->
        let v1, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        let r = value_record v1 in
        (v2, Mem.store mem'' (r id) v2)

  let run (mem, env, pgm) =
    let v, _ = eval mem env pgm in
    v
end
