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

type type_env = (M.id * typ) list
type type_subst = (var * typ) list

let rec apply_subst subst t =
  match t with
  | TInt | TBool | TString -> t
  | TPair (t1, t2) -> TPair (apply_subst subst t1, apply_subst subst t2)
  | TLoc t -> TLoc (apply_subst subst t)
  | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
  | TVar v -> (
      match List.assoc_opt v subst with
      | Some t' -> apply_subst subst t'
      | None -> t)

let rec unify t1 t2 =
  match (t1, t2) with
  | TInt, TInt | TBool, TBool | TString, TString -> []
  | TPair (t1, t2), TPair (t1', t2') ->
      let s1 = unify t1 t1' in
      let s2 = unify (apply_subst s1 t2) (apply_subst s1 t2') in
      s1 @ s2
  | TLoc t, TLoc t' -> unify t t'
  | TFun (t1, t2), TFun (t1', t2') ->
      let s1 = unify t1 t1' in
      let s2 = unify (apply_subst s1 t2) (apply_subst s1 t2') in
      s1 @ s2
  | TVar v, t | t, TVar v -> if t = TVar v then [] else [ (v, t) ]
  | TInt, t | t, TInt ->
      if t = TInt then []
      else raise (M.TypeError "Arithmetic operations require integer operands")
  | TBool, t | t, TBool ->
      if t = TBool then []
      else raise (M.TypeError "Logical operations require boolean operands")
  | TString, t | t, TString ->
      if t = TString then []
      else raise (M.TypeError "String operations require string operands")
  | _ -> raise (M.TypeError "Type mismatch")

let rec type_to_m_type t =
  match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (type_to_m_type t1, type_to_m_type t2)
  | TLoc t -> M.TyLoc (type_to_m_type t)
  | TFun (t1, t2) -> M.TyArrow (type_to_m_type t1, type_to_m_type t2)
  | TVar _ -> raise (M.TypeError "Type variable in final type")

let rec check_type env exp =
  match exp with
  | M.CONST (M.N _) -> TInt
  | M.CONST (M.B _) -> TBool
  | M.CONST (M.S _) -> TString
  | M.VAR x -> (
      match List.assoc_opt x env with
      | Some t -> t
      | None -> raise (M.TypeError ("Unbound variable: " ^ x)))
  | M.FN (x, e) ->
      let arg_type = TVar (new_var ()) in
      let body_type = check_type ((x, arg_type) :: env) e in
      TFun (arg_type, body_type)
  | M.APP (e1, e2) -> (
      let t1 = check_type env e1 in
      let t2 = check_type env e2 in
      match t1 with
      | TFun (arg_type, ret_type) ->
          let subst = unify arg_type t2 in
          apply_subst subst ret_type
      | _ -> raise (M.TypeError "Not a function"))
  | M.LET (decl, e) -> (
      match decl with
      | M.VAL (x, e1) ->
          let t1 = check_type env e1 in
          check_type ((x, t1) :: env) e
      | M.REC (f, x, e1) ->
          let arg_type = TVar (new_var ()) in
          let ret_type = TVar (new_var ()) in
          let env' = (f, TFun (arg_type, ret_type)) :: env in
          let body_type = check_type ((x, arg_type) :: env') e1 in
          let subst = unify body_type ret_type in
          let final_type = apply_subst subst (TFun (arg_type, ret_type)) in
          check_type ((f, final_type) :: env) e)
  | M.IF (e1, e2, e3) ->
      let t1 = check_type env e1 in
      let _ = unify t1 TBool in
      let t2 = check_type env e2 in
      let t3 = check_type env e3 in
      let subst = unify t2 t3 in
      apply_subst subst t2
  | M.BOP (op, e1, e2) -> (
      let t1 = check_type env e1 in
      let t2 = check_type env e2 in
      match op with
      | M.ADD | M.SUB ->
          let _ = unify t1 TInt in
          let _ = unify t2 TInt in
          TInt
      | M.AND | M.OR ->
          let subst1 = unify t1 TBool in
          let _ = unify (apply_subst subst1 t2) TBool in
          TBool
      | M.EQ ->
          let _ = unify t1 t2 in
          TBool)
  | M.READ -> TInt
  | M.WRITE e ->
      let t = check_type env e in
      if t = TInt || t = TBool || t = TString then t
      else raise (M.TypeError "WRITE operand must be int/bool/string")
  | M.MALLOC e ->
      let t = check_type env e in
      TLoc t
  | M.ASSIGN (e1, e2) -> (
      let t1 = check_type env e1 in
      let t2 = check_type env e2 in
      match t1 with
      | TLoc t ->
          let _ = unify t t2 in
          TInt
      | _ ->
          raise (M.TypeError "Left-hand side of assignment must be a location"))
  | M.BANG e -> (
      let t = check_type env e in
      match t with
      | TLoc t' -> t'
      | _ -> raise (M.TypeError "Dereference requires a location"))
  | M.SEQ (e1, e2) ->
      let _ = check_type env e1 in
      check_type env e2
  | M.PAIR (e1, e2) ->
      let t1 = check_type env e1 in
      let t2 = check_type env e2 in
      TPair (t1, t2)
  | M.FST e -> (
      let t = check_type env e in
      match t with
      | TPair (t1, _) -> t1
      | _ -> raise (M.TypeError "First projection requires a pair"))
  | M.SND e -> (
      let t = check_type env e in
      match t with
      | TPair (_, t2) -> t2
      | _ -> raise (M.TypeError "Second projection requires a pair"))

let check exp =
  let t = check_type [] exp in
  type_to_m_type t
