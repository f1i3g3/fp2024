(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Monads.TYPECHECK
open Common

let value_to_type = function
  | ValInt _ -> TypeInt
  | ValChar _ -> TypeChar
  | ValBool _ -> TypeBool
  | ValString _ -> TypeString
  | ValNull -> TypeInt (* TODO! *)
;;

let vartype_to_type = function
  | TypeVar t -> t
;;

let vardecl_to_type = function
  | Var (t, _) -> return (vartype_to_type t)
;;

let name_to_obj_ctx n = read_local_el n

let eq f e1 e2 =
  match f e1 e2 with
  | true -> return e1
  | false -> fail (TCError TypeMismatch)
;;

let eq_type t1 t2 = eq equal__type t1 t2
let eq_ident n1 n2 = eq equal_ident n1 n2

let eq_ident_return_ctx n1 n2 m f =
  match equal_ident n1 n2 with
  | true -> Some (f m)
  | false -> None
;;

let get_class_memb id memb =
  match memb with
  | VarField (_, _, f_id, _) -> eq_ident_return_ctx f_id id memb (fun f -> Field f)
  | Method (_, _, m_id, _, _) -> eq_ident_return_ctx m_id id memb (fun m -> Method m)
;;

let get_class_name = function
  | Class (_, id, _) -> id
;;

let find_memb_from_obj obj_id id =
  let find_memb b id f = List.find_map (f id) b in
  let find_class_memb b id = find_memb b id get_class_memb in
  read_global_el obj_id
  >>= function
  | TCClass (Class (_, _, b)) -> find_class_memb b id |> return
;;

let is_public obj_id ctx mds =
  let is_m_public = function
    | MPublic -> return (Some ctx)
    | _ -> fail (TCError NotImplemented)
  in
  let rec is_m_list_public = function
    | [] ->
      read_global_el obj_id
      >>= (function
       | TCClass _ -> fail (TCError AccessError))
    | x :: xs ->
      is_m_public x
      >>= (function
       | Some ctx -> return ctx
       | None -> is_m_list_public xs)
    (* TODO: bad code! *)
  in
  is_m_list_public mds
;;

let find_obj_memb_with_fail n_obj n_mem =
  find_memb_from_obj n_obj n_mem
  >>= function
  | Some memb ->
    (match memb with
     | Field (VarField (mds, _, _, _)) -> is_public n_obj memb mds
     | Method (Method (mds, _, _, _, _)) -> is_public n_obj memb mds
     | _ -> fail (TCError (ImpossibleResult "Object can only have fields and methods")))
  | None -> fail (TCError (OtherError "Class member not found"))
;;

let find_memb_type = function
  | VarType (TypeVar t)
  | Field (VarField (_, TypeVar t, _, _))
  | Field (Method (_, t, _, _, _)) -> return t
  | _ -> fail (TCError TypeMismatch)
;;

let typecheck_method_args (Params params) (Args args) expr_tc =
  let params_to_list_of_type p =
    List.map
      (function
        | Var (t, _) -> vartype_to_type t)
      p
  in
  let args_to_list_of_type a = map (fun x -> expr_tc x >>= fun x -> find_memb_type x) a in
  let compare_two_lists l1 l2 eq rez =
    match List.compare_lengths l1 l2 with
    | 0 ->
      (match List.equal eq l1 l2 with
       | true -> return rez
       | false -> fail (TCError (OtherError "Method invocation check error")))
    | _ -> fail (TCError (OtherError "Method invocation check error"))
  in
  args_to_list_of_type args
  >>= fun args ->
  compare_two_lists (params_to_list_of_type params) args equal__type params
;;

let find_expr_type e expr_tc = expr_tc e >>= fun e -> find_memb_type e

let typecheck_bin_op b e1 e2 expr_tc =
  let compare_two_expr_type e1 e2 =
    find_expr_type e1 expr_tc
    >>= fun e1 -> find_expr_type e2 expr_tc >>= fun e2 -> eq_type e1 e2
  in
  let compare_three_expr_type e1 e2 t =
    compare_two_expr_type e1 e2 >>= fun e -> eq_type e t
  in
  let return_rez rez = return (VarType (TypeVar rez)) in
  match b with
  | OpAdd | OpMul | OpSub | OpDiv | OpMod ->
    compare_three_expr_type e1 e2 (TypeBase TypeInt) *> return_rez (TypeBase TypeInt)
  | OpLess | OpLessEqual | OpMore | OpMoreEqual ->
    compare_three_expr_type e1 e2 (TypeBase TypeInt) *> return_rez (TypeBase TypeBool)
  | OpEqual | OpNonEqual -> compare_two_expr_type e1 e2 *> return_rez (TypeBase TypeBool)
  | OpAnd | OpOr ->
    compare_three_expr_type e1 e2 (TypeBase TypeBool) *> return_rez (TypeBase TypeBool)
  | OpAssign ->
    find_expr_type e1 expr_tc >>= fun e -> compare_two_expr_type e1 e2 *> return_rez e
;;

let typecheck_un_op u e expr_tc =
  let tc_un_op u e =
    find_expr_type e expr_tc
    >>= fun t ->
    match u with
    | OpNot -> eq_type t (TypeBase TypeBool)
  in
  tc_un_op u e >>= fun t -> return (VarType (TypeVar t))
;;

let tc_method_args (Params params) (Args args) expr_tc =
  let params_to_list_of_type p =
    List.map
      (function
        | Var (t, _) -> vartype_to_type t)
      p
  in
  let args_to_list_of_type a = map (fun x -> expr_tc x >>= fun x -> find_memb_type x) a in
  let compare_two_lists l1 l2 eq rez =
    match List.compare_lengths l1 l2 with
    | 0 ->
      (match List.equal eq l1 l2 with
       | true -> return rez
       | false -> fail (TCError (OtherError "Method invocation check error")))
    | _ -> fail (TCError (OtherError "Method invocation check error"))
  in
  args_to_list_of_type args
  >>= fun args -> compare_two_lists (params_to_list_of_type params) args equal__type params
;;
let tc_method_invoke e args expr_tc =
  expr_tc e
  >>= function
  | Ast.Method (_, tp, _, pms, _) ->
    tc_method_args pms args expr_tc
    *>
      (match tp with
      | TypeBase _ -> return (VarType (TypeVar tp))
      | TypeVoid -> fail (TCError (OtherError "Method invocation check error")))
  | _ -> fail (TCError (OtherError "Method invocation check error"))
;;

let typecheck_expr =
  let rec tc_expr_ = function
    | EId n -> name_to_obj_ctx n
    | EFuncCall (e, args) -> tc_method_invoke e args tc_expr_
    | EBinOp (b, e1, e2) -> typecheck_bin_op b e1 e2 tc_expr_
    | EUnOp (u, e) -> typecheck_un_op u e tc_expr_
    | _ -> fail (TCError NotImplemented)
  in
  tc_expr_
;;

let typecheck_expr_with_type e = typecheck_expr e >>= fun x -> find_memb_type x
let eq_type_with_expr t e = typecheck_expr_with_type e >>= fun e_t -> eq_type e_t t

let save_decl n ctx =
  read_local_el_opt n
  >>= function
  | None -> write_local_el n ctx
  | Some _ -> fail (TCError (OtherError "This variable is already declared"))
;;

let apply_local f = read_local >>= fun old_l -> f *> write_local old_l

let rec typecheck_stmt =
  let is_expr_bool e =
    typecheck_expr_with_type e >>= fun t -> eq_type t (TypeBase TypeBool)
  in
  let typecheck_stmt_expr expr =
    match expr with
    | EFuncCall (e, args) ->
      (* TODO FuncCall!! *)
      typecheck_expr e
      >>= (function
       | Method (Method (_, TypeVoid, _, pms, _)) ->
         typecheck_method_args pms args typecheck_expr *> return ()
       | _ -> fail (TCError TypeMismatch))
    | EBinOp (OpAssign, _, _) -> typecheck_expr expr *> return ()
    | _ -> fail (TCError TypeMismatch)
  in
  let typecheck_decl t n = function
    | Some e -> eq_type_with_expr t e *> save_decl n (VarType (TypeVar t)) *> return ()
    | None -> save_decl n (VarType (TypeVar t)) *> return ()
  in
  let typecheck_return e_opt =
    read_meth_type
    >>= fun m_t ->
    match m_t, e_opt with
    | Some TypeVoid, None -> return ()
    | Some (TypeBase t), Some e ->
      (eq_type_with_expr (TypeBase t) e
       <|> fail (TCError (OtherError "Returned type does not match the function type")))
      *> return ()
    | _ -> fail (TCError TypeMismatch)
  in
  let opt_unpack f = function
    | None -> return ()
    | Some s -> f s *> return ()
  in
  let typecheck_for_state init cond iter =
    let typecheck_init = function
      | None -> return ()
      | Some (SDecl (Var (TypeVar t, n), e)) -> typecheck_decl t n e
      | _ -> fail (TCError TypeMismatch)
    in
    let typecheck_cond = opt_unpack is_expr_bool cond in
    let typecheck_iter = opt_unpack typecheck_stmt_expr iter in
    lift3 (fun _ _ _ -> ()) (typecheck_init init) typecheck_cond typecheck_iter
  in
  let typecheck_if_state cond b s_opt tc_st =
    let typecheck_cond = is_expr_bool cond in
    let typecheck_state = function
      | Some st -> tc_st st
      | None -> return ()
    in
    lift3 (fun _ _ _ -> ()) typecheck_cond (tc_st b) (typecheck_state s_opt)
  in
  function
  | SExpr expr -> typecheck_stmt_expr expr
  | SDecl (Var (TypeVar t, n), e) -> typecheck_decl t n e
  | SReturn e -> typecheck_return e
  | SWhile (e, s) -> apply_local (is_expr_bool e *> typecheck_stmt s)
  | SFor (init, cond, iter, b) ->
    apply_local (typecheck_for_state init cond iter *> typecheck_stmt b)
  | SIf (e, b, s_opt) -> apply_local (typecheck_if_state e b s_opt typecheck_stmt)
  | SBlock st_l -> apply_local (iter typecheck_stmt st_l)
  | SBreak | SContinue -> fail (TCError NotImplemented)
;;

let tc_member mem =
  let tc_class_field f_type = function
    | Some e -> eq_type_with_expr (vartype_to_type f_type) e *> return ()
    | None -> return ()
  in
  let save_params_to_l (Params params) =
    let f = function
      | Var (t, n) -> write_local_el n (VarType t)
    in
    iter f params
  in
  let tc_meth typ params body =
    apply_local (write_meth_type typ *> save_params_to_l params *> typecheck_stmt body)
  in
  let tc_class_method (mds, tp, id, pms, b) =
    match equal_ident id (Id "Main") with
    | true ->
      (match mds, pms, tp with
       | [ MStatic ], Params [], TypeBase TypeInt | [ MStatic ], Params [], TypeVoid ->
         tc_meth tp (Params []) b *> read_main_class
         >>= (function
          | None -> get_curr_class_name >>= fun n -> write_main_class (Some n)
          | Some _ -> fail (TCError (OtherError "Main method already exists")))
       | _, _, _ ->
         fail
           (TCError
              (OtherError
                 "Main must be a static method, have no params and return only int or \
                  void")))
    | false -> tc_meth tp pms b
  in
  match mem with
  | VarField (_, tp, _, e_opt) -> tc_class_field tp e_opt
  | Method (mds, tp, id, pms, b) -> tc_class_method (mds, tp, id, pms, b)
;;

let save_global id ctx =
  read_global_el_opt id
  >>= function
  | None -> write_global_el id ctx
  | Some _ -> fail (TCError (OtherError "This variable is already declared"))
;;

let typecheck_obj cl =
  let write_mems b =
    let f mem =
      match mem with
      | VarField (_, _, id, _) -> save_decl id (Field mem)
      | Method (_, _, id, _, _) -> save_decl id (Method mem)
    in
    iter f b
  in
  let tc_mems b = iter tc_member b in
  let save_class cl = save_global (get_class_name cl) (TCClass cl) in
  match cl with
  | Class (mds, id, b) ->
    write_curr_class_name id
    *> apply_local (write_mems b *> save_class cl *> tc_mems b)
    *> return ()
;;

let typecheck prog = run (typecheck_obj prog) (IdMap.empty, IdMap.empty, None, None, None)
let typecheck_main prog = typecheck prog |> fun ((_, _, _, _, main), res) -> main, res
