open M

type var = string

type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var

type typ_scheme = SimpleTyp of typ | GenTyp of (var list * typ)
type typ_env = (M.id * typ_scheme) list

let count = ref 0

let new_var () =
  let _ = count := !count + 1 in
  "x_" ^ string_of_int !count

let union_ftv ftv1 ftv2 =
  let ftv1' = List.filter (fun v -> not (List.mem v ftv2)) ftv1 in
  ftv1' @ ftv2

let sub_ftv ftv1 ftv2 = List.filter (fun v -> not (List.mem v ftv2)) ftv1

let rec ftv_of_typ = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v -> [ v ]

let ftv_of_scheme = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (vars, t) -> sub_ftv (ftv_of_typ t) vars

let ftv_of_env env =
  List.fold_left (fun acc (_, s) -> union_ftv acc (ftv_of_scheme s)) [] env

let generalize env t =
  let ftv = sub_ftv (ftv_of_typ t) (ftv_of_env env) in
  if ftv = [] then SimpleTyp t else GenTyp (ftv, t)

type subst = typ -> typ

let empty_subst t = t

let make_subst x t =
  let rec subst = function
    | TVar v -> if v = x then t else TVar v
    | TPair (t1, t2) -> TPair (subst t1, subst t2)
    | TLoc t1 -> TLoc (subst t1)
    | TFun (t1, t2) -> TFun (subst t1, subst t2)
    | t -> t
  in
  subst

let ( @@ ) s1 s2 t = s1 (s2 t)

let subst_scheme s = function
  | SimpleTyp t -> SimpleTyp (s t)
  | GenTyp (vars, t) ->
      let fresh_vars = List.map (fun _ -> new_var ()) vars in
      let s' =
        List.fold_left2
          (fun acc a b -> make_subst a (TVar b) @@ acc)
          empty_subst vars fresh_vars
      in
      GenTyp (fresh_vars, s (s' t))

let subst_env s env = List.map (fun (x, sch) -> (x, subst_scheme s sch)) env

let rec occurs x = function
  | TVar y -> x = y
  | TPair (t1, t2) | TFun (t1, t2) -> occurs x t1 || occurs x t2
  | TLoc t -> occurs x t
  | _ -> false

let rec unify t1 t2 =
  match (t1, t2) with
  | TInt, TInt | TBool, TBool | TString, TString -> empty_subst
  | TVar x, t | t, TVar x ->
      if t = TVar x then empty_subst
      else if occurs x t then raise (M.TypeError ("occurs check: " ^ x))
      else make_subst x t
  | TPair (a1, a2), TPair (b1, b2) | TFun (a1, a2), TFun (b1, b2) ->
      let s1 = unify a1 b1 in
      let s2 = unify (s1 a2) (s1 b2) in
      s2 @@ s1
  | TLoc a, TLoc b -> unify a b
  | _ -> raise (M.TypeError "unification failed")

let instantiate = function
  | SimpleTyp t -> t
  | GenTyp (vars, t) ->
      let subst =
        List.fold_left
          (fun acc x -> make_subst x (TVar (new_var ())) @@ acc)
          empty_subst vars
      in
      subst t

let lookup_env x env =
  try List.assoc x env
  with Not_found -> raise (M.TypeError ("unbound variable: " ^ x))

let rec convert_typ = function
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (convert_typ t1, convert_typ t2)
  | TLoc t -> M.TyLoc (convert_typ t)
  | TFun _ | TVar _ ->
      raise (M.TypeError "cannot convert function or unresolved type")

let rec is_value = function
  | M.CONST _ | M.VAR _ | M.FN _ -> true
  | M.PAIR (e1, e2) -> is_value e1 && is_value e2
  | _ -> false

let rec infer env = function
  | M.CONST (M.N _) -> (TInt, empty_subst)
  | M.CONST (M.B _) -> (TBool, empty_subst)
  | M.CONST (M.S _) -> (TString, empty_subst)
  | M.VAR x -> (instantiate (lookup_env x env), empty_subst)
  | M.FN (x, e) ->
      let tv = TVar (new_var ()) in
      let t_body, s = infer ((x, SimpleTyp tv) :: env) e in
      (TFun (s tv, t_body), s)
  | M.APP (e1, e2) ->
      let t1, s1 = infer env e1 in
      let t2, s2 = infer (subst_env s1 env) e2 in
      let tv = TVar (new_var ()) in
      let s3 = unify (s2 t1) (TFun (t2, tv)) in
      (s3 tv, s3 @@ s2 @@ s1)
  | M.LET (M.VAL (x, e1), e2) ->
      let t1, s1 = infer env e1 in
      let env1 = subst_env s1 env in
      let scheme = if is_value e1 then generalize env1 t1 else SimpleTyp t1 in
      let t2, s2 = infer ((x, scheme) :: env1) e2 in
      (t2, s2 @@ s1)
  | M.LET (M.REC (f, x, e1), e2) ->
      let a, b = (TVar (new_var ()), TVar (new_var ())) in
      let env1 = (f, SimpleTyp (TFun (a, b))) :: (x, SimpleTyp a) :: env in
      let t_body, s1 = infer env1 e1 in
      let s2 = unify (s1 b) t_body in
      let fun_type = s2 (s1 (TFun (a, b))) in
      let scheme = generalize (subst_env (s2 @@ s1) env) fun_type in
      let t2, s3 = infer ((f, scheme) :: subst_env (s2 @@ s1) env) e2 in
      (t2, s3 @@ s2 @@ s1)
  | M.IF (e1, e2, e3) ->
      let t1, s1 = infer env e1 in
      let s2 = unify t1 TBool in
      let t2, s3 = infer (subst_env (s2 @@ s1) env) e2 in
      let t3, s4 = infer (subst_env (s3 @@ s2 @@ s1) env) e3 in
      let s5 = unify (s4 t2) t3 in
      (s5 t3, s5 @@ s4 @@ s3 @@ s2 @@ s1)
  | M.BOP (op, e1, e2) -> (
      let t1, s1 = infer env e1 in
      let t2, s2 = infer (subst_env s1 env) e2 in
      match op with
      | M.ADD | M.SUB ->
          let s3 = unify (s2 t1) TInt in
          let s4 = unify (s3 t2) TInt in
          (TInt, s4 @@ s3 @@ s2 @@ s1)
      | M.AND | M.OR ->
          let s3 = unify (s2 t1) TBool in
          let s4 = unify (s3 t2) TBool in
          (TBool, s4 @@ s3 @@ s2 @@ s1)
      | M.EQ ->
          let s3 = unify (s2 t1) t2 in
          (TBool, s3 @@ s2 @@ s1))
  | M.READ -> (TInt, empty_subst)
  | M.WRITE e ->
      let t, s = infer env e in
      (t, s)
  | M.MALLOC e ->
      let t, s = infer env e in
      (TLoc t, s)
  | M.ASSIGN (e1, e2) -> (
      let t1, s1 = infer env e1 in
      let t2, s2 = infer (subst_env s1 env) e2 in
      match s1 t1 with
      | TLoc inner ->
          let s3 = unify (s2 inner) t2 in
          (s3 t2, s3 @@ s2 @@ s1)
      | _ -> raise (M.TypeError "not a location"))
  | M.BANG e -> (
      let t, s = infer env e in
      match s t with
      | TLoc inner -> (inner, s)
      | _ -> raise (M.TypeError "not a location"))
  | M.SEQ (e1, e2) ->
      let _, s1 = infer env e1 in
      let t2, s2 = infer (subst_env s1 env) e2 in
      (t2, s2 @@ s1)
  | M.PAIR (e1, e2) ->
      let t1, s1 = infer env e1 in
      let t2, s2 = infer (subst_env s1 env) e2 in
      (TPair (s2 t1, t2), s2 @@ s1)
  | M.FST e ->
      let t, s = infer env e in
      let a, b = (TVar (new_var ()), TVar (new_var ())) in
      let s' = unify (s t) (TPair (a, b)) in
      (s' a, s' @@ s)
  | M.SND e ->
      let t, s = infer env e in
      let a, b = (TVar (new_var ()), TVar (new_var ())) in
      let s' = unify (s t) (TPair (a, b)) in
      (s' b, s' @@ s)

let check e =
  let t, s = infer [] e in
  convert_typ (s t)
