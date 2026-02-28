(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser
open Typecheck
open Common
open Common.Interpret
open Monads.INTERPRET

let is_val = function
  | Value (x, _) -> return x
  | _ -> fail (IError (OtherError "It's not a value"))
;;

let is_init x =
  is_val x
  >>= function
  | Init x -> return x
  | _ -> fail (IError (OtherError "Value is not initialized"))
;;

let is_init_val x =
  is_val x
  >>= function
  | Init (IValue x) -> return x
  | _ -> fail (IError (OtherError "Value is not initialized"))
;;

let is_code = function
  | Code c -> return c
  | _ -> fail (IError (OtherError "It's not a method"))
;;

let is_int x =
  is_init_val x
  >>= function
  | ValInt x -> return x
  | _ -> fail (IError TypeMismatch)
;;

let is_bool x =
  is_init_val x
  >>= function
  | ValBool x -> return x
  | _ -> fail (IError TypeMismatch)
;;

let v_int x = ValInt x
let v_bool x = ValBool x
let v_bool_not x = ValBool (not x)
let v_int_minus x = ValInt (-x)
let interpret_const c = return (Value (Init (IValue c), None)) (* TODO *)
let interpret_id n = find_local_el n

let interpret_func_call args i_expr code i_stmt =
  (* TODO *)
  let get_args = map i_expr args in
  is_code code
  >>= function
  | IMethod (m, body) ->
    read_local_adr
    >>= fun adr ->
    get_args >>= fun args -> run_method args m.m_params adr m.m_type (i_stmt body)
  | IConstructor _ ->
    fail (IError (OtherError "Multiple constructors are not implemented"))
;;

let i_assign e1 e2 i_expr =
  let is_val_with_idx = function
    | Value (_, i) -> return i
    | _ ->
      fail
        (IError (OtherError "The assignment operator must assign a value to the variable"))
  in
  i_expr e2
  >>= is_init
  >>= fun v ->
  match e1 with
  | EId n ->
    (read_local
     >>= (fun (idx, l) ->
           read_local_el n
           >>= is_val_with_idx
           >>= fun i -> write_local (idx, IdMap.add n (Value (Init v, i)) l))
     <|> (read_local_adr
          >>= fun adr ->
          read_memory_obj adr
          >>= fun obj ->
          IdMap.find_opt n obj.mems
          |> function
          | Some (f, _) ->
            write_memory_obj adr { obj with mems = IdMap.add n (f, Init v) obj.mems }
          | None -> fail (IError TypeMismatch)))
    *> find_local_el n
  | _ ->
    fail
      (IError (OtherError "The assignment operator must assign a value to the variable"))
;;

let i_bin_op bin_op e1 e2 i_expr =
  let r_val op v f =
    lift2 (fun e1 e2 -> e1, e2) (i_expr e1 >>= f) (i_expr e2 >>= f)
    >>= fun (c1, c2) -> return (Value (Init (IValue (v (op c1 c2))), None))
  in
  let int_r_int op = r_val op v_int is_int in
  let int_r_bool op = r_val op v_bool is_int in
  let bool_r_bool op = r_val op v_bool is_bool in
  let not_equal_val_type c1 c2 =
    equal_val_type c1 c2
    |> function
    | true -> false
    | false -> true
  in
  let eq op = r_val op v_bool is_init_val in
  match bin_op with
  | OpAdd -> int_r_int ( + )
  | OpMul -> int_r_int ( * )
  | OpSub -> int_r_int ( - )
  | OpDiv -> int_r_int ( / )
  | OpMod -> int_r_int ( mod )
  | OpEqual -> eq equal_val_type
  | OpNonEqual -> eq not_equal_val_type
  | OpLess -> int_r_bool ( < )
  | OpLessEqual -> int_r_bool ( <= )
  | OpMore -> int_r_bool ( > )
  | OpMoreEqual -> int_r_bool ( >= )
  | OpAnd -> bool_r_bool ( && )
  | OpOr -> bool_r_bool ( || )
  | OpAssign -> i_assign e1 e2 i_expr
;;

let i_un_op un_op e i_expr i_stmt =
  let res f v = i_expr e >>= f >>= fun x -> interpret_const (v x) in
  match un_op with
  | OpNot -> res is_bool v_bool_not
;;

let i_expr i_statement =
  let check_return = function
    | Some x -> return (Value (x, None))
    | None -> fail (IError (OtherError "Void cannot be used with expr"))
  in
  let rec i_expr_ = function
    | EId n -> interpret_id n
    | EBinOp (bin_op, e1, e2) -> i_bin_op bin_op e1 e2 i_expr_
    | EUnOp (un_op, e) -> i_un_op un_op e i_expr_ i_statement
    | EFuncCall (e, Args args) ->
      (match e with
       | EId n ->
         interpret_id n
         >>= fun el -> interpret_func_call args i_expr_ el i_statement >>= check_return
       | _ -> fail (IError (ImpossibleResult "Check during typecheck")))
    | _ -> fail (IError NotImplemented)
  in
  i_expr_
;;

let i_stmt_expr expr i_expr i_stmt =
  (* TODO !!!!!!!!!!! *)
  match expr with
  | EFuncCall (e, Args args) ->
    i_expr e
    >>= fun code ->
    i_method_invoke args i_expr code i_stmt
    >>= (function
     | None -> return ()
     | Some _ ->
       fail (IError (OtherError "The statement can only have a method of void type")))
  | EBinOp (OpAssign, _, _) -> i_expr expr *> return ()
  | _ -> fail (IError TypeMismatch)
;;

let bool_expr i_stmt e = i_expr i_stmt e >>= is_bool

let i_if_state i_stmt e b s_opt =
  bool_expr i_stmt e
  >>= function
  | true -> i_stmt b
  | false ->
    (match s_opt with
     | Some b -> i_stmt b
     | None -> return ())
;;

let rec cycle f1 f2 =
  f1
  >>= function
  | true -> f2 *> cycle f1 f2
  | false -> return ()
;;

let i_while_state i_stmt e s = cycle (bool_expr i_stmt e) (i_stmt s)

let i_for_state i_stmt init cond iter b =
  let get_init =
    match init with
    | Some init -> i_stmt init
    | None -> return ()
  in
  let get_cond =
    match cond, iter with
    | Some c, Some i -> i_expr i_stmt i *> bool_expr i_stmt c
    | Some c, None -> bool_expr i_stmt c
    | None, Some i -> i_expr i_stmt i *> return true
    | None, None -> return true
  in
  get_init *> cycle get_cond (i_stmt b)
;;

let local f =
  let helper idx k v acc =
    match v with
    | Value (v, Some (Idx cur_idx)) ->
      (match cur_idx <= idx with
       | true -> IdMap.add k (Value (v, Some (Idx cur_idx))) acc
       | false -> acc)
    | Code c -> IdMap.add k (Code c) acc
    | _ -> acc
  in
  read_local
  >>= fun (Idx i, _) ->
  f *> read_local
  >>= fun (_, l) -> write_local (Idx i, IdMap.fold (helper i) l IdMap.empty)
;;

let interpret_stmt =
  let rec i_stmt = function
    | SExpr e -> i_sexpr e (i_expr i_stmt) i_stmt
    | SDecl (Var (TypeVar _, n), e) ->
      get_new_idx
      >>= fun new_idx ->
      (match e with
       | Some e ->
         i_expr i_stmt e
         >>= (function
          | Value (v, _) -> write_new_local_el n (Value (v, Some new_idx))
          | _ -> fail (IError (ImpossibleResult "Check during typecheck")))
       | None -> write_new_local_el n (Value (NotInit, Some new_idx)))
    | SReturn e ->
      (match e with
       | Some e -> i_expr i_stmt e >>= is_val >>= fun r -> func_return (Some r)
       | None -> func_return None)
    | SWhile (e, s) -> local (i_while_state i_stmt e s)
    | SFor (init, cond, iter, b) -> local (i_for_state i_stmt init cond iter b)
    | SIf (e, b, s_opt) -> local (i_if_state i_stmt e b s_opt)
    | SBlock st_l -> local (iter i_stmt st_l)
    | SBreak | SContinue -> fail (IError NotImplemented)
  in
  i_stmt
;;

let get_meth_from_class cl name =
  let f acc = function
    | IMethod (m, b) when equal_name m.m_name name -> return (Some (m, b))
    | _ -> return acc
  in
  fold_left f None cl.cl_body
;;

let run_interpreter cl_with_main g_env =
  let get_g_env =
    let f = function
      | IntrClass cl -> write_global_el cl.cl_id (IntrClass cl)
    in
    f g_env
  in
  let get_l_env =
    let save_constr cl =
      write_new_local_el
        cl.cl_name
        (Code
           (IConstructor
              ( { c_modifier = [ MPublic ]; c_id = cl.cl_id; c_params = Params [] }
              , SBlock [] )))
      <|> return ()
    in
    let f =
      match cl with
      | Class _ -> save_constr cl
    in
    f g_env
  in
  get_g_env *> get_l_env *> read_global_el cl_with_main
  >>= function
  | _ -> fail (IError NotImplemented)
;;

let interpret str =
  match apply_parser parse_prog str with
  | Result.Ok (Program pr) ->
    (match typecheck_main pr with
     | Some cl_with_main, Result.Ok _ ->
       run (run_interpreter cl_with_main pr)
       |> (function
        | _, Signal (Pipe x) -> Result.Ok x
        | _, IError er -> Result.Error er
        | _, _ ->
          Result.Error (IError (ImpossibleResult "Run_method returns return or error")))
     | None, Result.Ok _ -> Result.Error (IError (OtherError "Main method not found"))
     | _, Result.Error er -> Result.Error er)
  | Result.Error e -> Result.Error (TCError (OtherError e))
;;
(* TODO: not finished, should add more combinators *)
