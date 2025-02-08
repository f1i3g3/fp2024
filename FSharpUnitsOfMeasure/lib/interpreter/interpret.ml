(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Misc
open Checks

module type ERROR_MONAD = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module Env (M : ERROR_MONAD) = struct
  open M

  let empty = Base.Map.empty (module Base.String)

  let find env id =
    match Base.Map.find env id with
    | Some x -> return x
    | None -> fail (Unbound_identificator id)
  ;;

  let extend env key value = Base.Map.update env key ~f:(fun _ -> value)
end

module Eval (M : ERROR_MONAD) : sig
  (* val interpret : program -> (environment, error) M.t *)
end = struct
  open M
  open Env (M)

  let eval_const = function
    | Const_int i -> return (VInt i)
    | Const_float f -> return (VFloat f)
    | Const_bool b -> return (VBool b)
    | Const_char c -> return (VChar c)
    | Const_string s -> return (VString s)
    | _ -> fail Type_mismatch
  ;;

  let rec eval_pat env p v =
    match p, v with
    | Pattern_wild, _ -> Some env
    | Pattern_const c, v ->
      (match c, v with
       | Const_int i1, VInt i2 when i1 = i2 -> Some env
       | Const_float f1, VFloat f2 when Float.equal f1 f2 -> Some env
       | Const_bool b1, VBool b2 when Bool.equal b1 b2 -> Some env
       | Const_char c1, VChar c2 when Char.equal c1 c2 -> Some env
       | Const_string s1, VString s2 when String.equal s1 s2 -> Some env
       | _ -> None)
    | Pattern_ident_or_op id, v -> Some (extend env id v)
    | Pattern_typed (p, _), v -> eval_pat env p v
    | Pattern_option p, VOption v ->
      (match p, v with
       | Some p', Some v' -> eval_pat env p' v'
       | None, None -> Some env
       | _ -> None)
    | Pattern_or (p1, p2), v ->
      let p1' = eval_pat env p1 v in
      if Option.is_none p1' then eval_pat env p2 v else None
    | Pattern_list pl, VList vl -> eval_pat_list env pl vl
    | Pattern_tuple (p1, p2, prest), VTuple (v1, v2, vrest) ->
      let pl = p1 :: p2 :: prest in
      let vl = v1 :: v2 :: vrest in
      eval_pat_list env pl vl
    | _ -> None

  and eval_pat_list env pl vl =
    if List.length pl <> List.length vl
    then None
    else (
      let f acc p v =
        match acc with
        | None -> None
        | Some env' -> eval_pat env' p v
      in
      List.fold_left2 f (Some env) pl vl)
  ;;

  let eval_binop f v1 v2 =
    match f, v1, v2 with
    | "+", VInt i1, VInt i2 -> return (VInt (i1 + i2))
    | "-", VInt i1, VInt i2 -> return (VInt (i1 - i2))
    | "*", VInt i1, VInt i2 -> return (VInt (i1 * i2))
    | "/", VInt i1, VInt i2 when i2 <> 0 -> return (VInt (i1 / i2))
    | "/", VInt _, VInt _ -> fail Division_by_zero
    | "+.", VFloat f1, VFloat f2 -> return (VFloat (f1 +. f2))
    | "-.", VFloat f1, VFloat f2 -> return (VFloat (f1 -. f2))
    | "*.", VFloat f1, VFloat f2 -> return (VFloat (f1 *. f2))
    | "/.", VFloat f1, VFloat f2 when f2 <> 0.0 -> return (VFloat (f1 /. f2))
    | "/.", VFloat _, VFloat _ -> fail Division_by_zero
    | "<=", VInt i1, VInt i2 -> return (VBool (i1 <= i2))
    | "<", VInt i1, VInt i2 -> return (VBool (i1 < i2))
    | ">=", VInt i1, VInt i2 -> return (VBool (i1 >= i2))
    | ">", VInt i1, VInt i2 -> return (VBool (i1 > i2))
    | "=", VInt i1, VInt i2 -> return (VBool (i1 = i2))
    | "<>", VInt i1, VInt i2 -> return (VBool (i1 <> i2))
    | "<=", VFloat f1, VFloat f2 -> return (VBool (f1 <= f2))
    | "<", VFloat f1, VFloat f2 -> return (VBool (f1 < f2))
    | ">=", VFloat f1, VFloat f2 -> return (VBool (f1 >= f2))
    | ">", VFloat f1, VFloat f2 -> return (VBool (f1 > f2))
    | "=", VFloat f1, VFloat f2 -> return (VBool (f1 = f2))
    | "<>", VFloat f1, VFloat f2 -> return (VBool (f1 <> f2))
    | "||", VBool b1, VBool b2 -> return (VBool (b1 || b2))
    | "&&", VBool b1, VBool b2 -> return (VBool (b1 && b2))
    | "::", v, VList vl -> return (VList (v :: vl))
    | _ -> fail Unsupported_operation
  ;;

  let rec eval_expr env =
    let eval_list env lst =
      List.fold_left
        (fun acc e ->
          let* acc = acc in
          let* v = eval_expr env e in
          return (v :: acc))
        (return [])
        lst
      >>| fun l -> List.rev l
    in
    function
    | Expr_const c -> eval_const c
    | Expr_ident_or_op name -> find env name
    | Expr_typed (e, _) -> eval_expr env e
    | Expr_list el ->
      let* vl = eval_list env el in
      return (VList vl)
    | Expr_tuple (e1, e2, erest) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      let* vrest = eval_list env erest in
      return (VTuple (v1, v2, vrest))
    | Expr_lam (p, e) -> return (VFun (Nonrecursive, p, e, env))
    | Expr_ifthenelse (c, t, Some e) ->
      let* cval = eval_expr env c in
      (match cval with
       | VBool true -> eval_expr env t
       | VBool false -> eval_expr env e
       | _ -> fail Type_mismatch)
    | Expr_ifthenelse (c, t, None) ->
      let* cval = eval_expr env c in
      (match cval with
       | VBool true -> eval_expr env t
       | VBool false -> return VUnit
       | _ -> fail Type_mismatch)
    | Expr_option (Some e) ->
      let* v = eval_expr env e in
      return (VOption (Some v))
    | Expr_option None -> return (VOption None)
    | Expr_match (e, rhd, rtl) ->
      let* v = eval_expr env e in
      eval_rules env v (rhd :: rtl)
    | Expr_function (rhd, rtl) -> return (VFunction (rhd, rtl))
    | Expr_apply (Expr_apply (Expr_ident_or_op f, e1), e2) when is_builtin_op f ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      eval_binop f v1 v2
    | Expr_apply (func, arg) ->
      let* v1 = eval_expr env func in
      let* v2 = eval_expr env arg in
      (match v1 with
       | VFunction (rhd, rtl) -> eval_rules env v2 (rhd :: rtl)
       (* | VFun ... *)
       | _ -> fail Type_mismatch)
    | Expr_let (Nonrecursive, bhd, btl, ebody) ->
      fail Not_implemented
    | Expr_let (Recursive, bhd, btl, ebody) ->
      fail Not_implemented

  and eval_rules env v = function
    | Rule (p, e) :: tl ->
      let env' = eval_pat env p v in
      (match env' with
       | None -> eval_rules env v tl
       | Some env'' -> eval_expr env'' e)
    | [] -> fail Match_failure
  ;;
end