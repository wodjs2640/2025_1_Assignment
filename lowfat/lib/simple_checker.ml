(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

let count = ref 0

let new_var () =
  let _ = count := !count + 1 in
  "x_" ^ string_of_int !count

type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
(* Modify, or add more if needed *)

(* TODO : Implement this function *)
let check : M.exp -> M.types =
 fun exp ->
  (* Type environment *)
  let empty_env = [] in
  let extend_env env x t = (x, t) :: env in
  let rec lookup_env env x =
    match env with
    | [] -> raise (M.TypeError ("Unbound variable: " ^ x))
    | (y, t) :: rest -> if x = y then t else lookup_env rest x
  in

  (* Type substitution *)
  let empty_subst = [] in
  let rec apply_subst subst t =
    match t with
    | TInt | TBool | TString -> t
    | TPair (t1, t2) -> TPair (apply_subst subst t1, apply_subst subst t2)
    | TLoc t' -> TLoc (apply_subst subst t')
    | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
    | TVar x -> ( try List.assoc x subst with Not_found -> t)
  in
  let apply_subst_env subst env =
    List.map (fun (x, t) -> (x, apply_subst subst t)) env
  in
  let compose_subst s1 s2 =
    List.map (fun (x, t) -> (x, apply_subst s1 t)) s2 @ s1
  in

  (* Occurs check *)
  let rec occurs x t =
    match t with
    | TVar y -> x = y
    | TPair (t1, t2) -> occurs x t1 || occurs x t2
    | TLoc t' -> occurs x t'
    | TFun (t1, t2) -> occurs x t1 || occurs x t2
    | _ -> false
  in

  (* Unification *)
  let rec unify t1 t2 =
    match (t1, t2) with
    | TInt, TInt | TBool, TBool | TString, TString -> empty_subst
    | TPair (t1, t2), TPair (t1', t2') ->
        let s1 = unify t1 t1' in
        let s2 = unify (apply_subst s1 t2) (apply_subst s1 t2') in
        compose_subst s2 s1
    | TLoc t, TLoc t' -> unify t t'
    | TFun (t1, t2), TFun (t1', t2') ->
        let s1 = unify t1 t1' in
        let s2 = unify (apply_subst s1 t2) (apply_subst s1 t2') in
        compose_subst s2 s1
    | TVar x, t | t, TVar x ->
        if TVar x = t then empty_subst
        else if occurs x t then raise (M.TypeError "Occurs check failed")
        else [ (x, t) ]
    | _ -> raise (M.TypeError "Cannot unify types")
  in

  (* Type inference *)
  let rec infer env exp =
    match exp with
    | M.CONST (M.N _) -> (TInt, empty_subst)
    | M.CONST (M.B _) -> (TBool, empty_subst)
    | M.CONST (M.S _) -> (TString, empty_subst)
    | M.VAR x ->
        let t = lookup_env env x in
        (t, empty_subst)
    | M.FN (x, e) ->
        let tx = TVar (new_var ()) in
        let env' = extend_env env x tx in
        let te, s = infer env' e in
        (TFun (apply_subst s tx, te), s)
    | M.APP (e1, e2) ->
        let t1, s1 = infer env e1 in
        let t2, s2 = infer (apply_subst_env s1 env) e2 in
        let tv = TVar (new_var ()) in
        let s3 = unify (apply_subst s2 t1) (TFun (t2, tv)) in
        (apply_subst s3 tv, compose_subst s3 (compose_subst s2 s1))
    | M.LET (decl, e) -> (
        match decl with
        | M.VAL (x, e1) ->
            let t1, s1 = infer env e1 in
            let env' = extend_env (apply_subst_env s1 env) x t1 in
            let t2, s2 = infer env' e in
            (t2, compose_subst s2 s1)
        | M.REC (f, x, e1) ->
            let tf = TVar (new_var ()) in
            let tx = TVar (new_var ()) in
            let env' = extend_env (extend_env env f tf) x tx in
            let te, s1 = infer env' e1 in
            let s2 = unify (apply_subst s1 tf) (TFun (apply_subst s1 tx, te)) in
            let s3 = compose_subst s2 s1 in
            let env'' =
              extend_env (apply_subst_env s3 env) f (apply_subst s3 tf)
            in
            let t2, s4 = infer env'' e in
            (t2, compose_subst s4 s3))
    | M.IF (e1, e2, e3) ->
        let t1, s1 = infer env e1 in
        let s2 = unify t1 TBool in
        let s = compose_subst s2 s1 in
        let t2, s3 = infer (apply_subst_env s env) e2 in
        let t3, s4 = infer (apply_subst_env (compose_subst s3 s) env) e3 in
        let s5 = unify (apply_subst s4 t2) t3 in
        let final_s =
          compose_subst s5 (compose_subst s4 (compose_subst s3 s))
        in
        (apply_subst s5 t3, final_s)
    | M.BOP (op, e1, e2) -> (
        let t1, s1 = infer env e1 in
        let t2, s2 = infer (apply_subst_env s1 env) e2 in
        match op with
        | M.ADD | M.SUB ->
            let s3 = unify (apply_subst s2 t1) TInt in
            let s4 = unify (apply_subst s3 t2) TInt in
            (TInt, compose_subst s4 (compose_subst s3 (compose_subst s2 s1)))
        | M.AND | M.OR ->
            let s3 = unify (apply_subst s2 t1) TBool in
            let s4 = unify (apply_subst s3 t2) TBool in
            (TBool, compose_subst s4 (compose_subst s3 (compose_subst s2 s1)))
        | M.EQ ->
            let s3 = unify (apply_subst s2 t1) t2 in
            let final_s = compose_subst s3 (compose_subst s2 s1) in
            (TBool, final_s))
    | M.READ -> (TInt, empty_subst)
    | M.WRITE e ->
        let t, s = infer env e in
        let valid_types = [ TInt; TBool; TString ] in
        let rec try_unify_with_valid types =
          match types with
          | [] ->
              raise (M.TypeError "WRITE argument must be int, bool, or string")
          | hd :: tl -> (
              try
                let s' = unify t hd in
                (hd, compose_subst s' s)
              with M.TypeError _ -> try_unify_with_valid tl)
        in
        try_unify_with_valid valid_types
    | M.MALLOC e ->
        let t, s = infer env e in
        (TLoc t, s)
    | M.ASSIGN (e1, e2) ->
        let t1, s1 = infer env e1 in
        let t2, s2 = infer (apply_subst_env s1 env) e2 in
        let tv = TVar (new_var ()) in
        let s3 = unify (apply_subst s2 t1) (TLoc tv) in
        let s4 = unify (apply_subst s3 t2) (apply_subst s3 tv) in
        let final_s =
          compose_subst s4 (compose_subst s3 (compose_subst s2 s1))
        in
        (apply_subst s4 (apply_subst s3 tv), final_s)
    | M.BANG e ->
        let t, s = infer env e in
        let tv = TVar (new_var ()) in
        let s' = unify t (TLoc tv) in
        (apply_subst s' tv, compose_subst s' s)
    | M.SEQ (e1, e2) ->
        let _, s1 = infer env e1 in
        let t2, s2 = infer (apply_subst_env s1 env) e2 in
        (t2, compose_subst s2 s1)
    | M.PAIR (e1, e2) ->
        let t1, s1 = infer env e1 in
        let t2, s2 = infer (apply_subst_env s1 env) e2 in
        (TPair (apply_subst s2 t1, t2), compose_subst s2 s1)
    | M.FST e ->
        let t, s = infer env e in
        let t1 = TVar (new_var ()) in
        let t2 = TVar (new_var ()) in
        let s' = unify t (TPair (t1, t2)) in
        (apply_subst s' t1, compose_subst s' s)
    | M.SND e ->
        let t, s = infer env e in
        let t1 = TVar (new_var ()) in
        let t2 = TVar (new_var ()) in
        let s' = unify t (TPair (t1, t2)) in
        (apply_subst s' t2, compose_subst s' s)
  in

  (* Convert internal type to M.types *)
  let rec typ_to_m_types t =
    match t with
    | TInt -> M.TyInt
    | TBool -> M.TyBool
    | TString -> M.TyString
    | TPair (t1, t2) -> M.TyPair (typ_to_m_types t1, typ_to_m_types t2)
    | TLoc t' -> M.TyLoc (typ_to_m_types t')
    | TFun (t1, t2) -> M.TyArrow (typ_to_m_types t1, typ_to_m_types t2)
    | TVar _ -> raise (M.TypeError "Unresolved type variable")
  in

  try
    let t, s = infer empty_env exp in
    let final_type = apply_subst s t in
    typ_to_m_types final_type
  with
  | M.TypeError msg -> raise (M.TypeError msg)
  | _ -> raise (M.TypeError "Type checking failed")
