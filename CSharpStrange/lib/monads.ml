(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common

module STATEERROR = struct
  type ('st, 'a) t = 'st -> 'st * ('a, error) Result.t

  let return : 'a -> ('st, 'a) t = fun x st -> st, Result.Ok x
  let fail : 'a -> ('st, 'b) t = fun e st -> st, Result.Error e

  let ( >>= ) : ('st, 'a) t -> ('a -> ('st, 'b) t) -> ('st, 'b) t =
    fun x f st ->
    let st, x = x st in
    match x with
    | Result.Ok x -> f x st
    | Result.Error e -> fail e st
  ;;

  let ( *> ) : ('st, 'a) t -> ('st, 'b) t -> ('st, 'b) t = fun x1 x2 -> x1 >>= fun _ -> x2

  let ( <|> ) : ('st, 'a) t -> ('st, 'a) t -> ('st, 'a) t =
    fun x1 x2 st ->
    let st, x = x1 st in
    match x with
    | Result.Ok x -> return x st
    | Result.Error _ -> x2 st
  ;;

  let ( >>| ) : ('st, 'a) t -> ('a -> 'b) -> ('st, 'b) t =
    fun x f st ->
    let st, x = x st in
    match x with
    | Result.Ok x -> return (f x) st
    | Result.Error e -> fail e st
  ;;

  let lift2 : ('a -> 'b -> 'c) -> ('st, 'a) t -> ('st, 'b) t -> ('st, 'c) t =
    fun f a b -> a >>= fun r_a -> b >>= fun r_b -> return @@ f r_a r_b
  ;;

  let lift3
    : ('a -> 'b -> 'c -> 'd) -> ('st, 'a) t -> ('st, 'b) t -> ('st, 'c) t -> ('st, 'd) t
    =
    fun f a b c -> lift2 f a b >>= fun f -> c >>| f
  ;;

  let read : ('st, 'st) t = fun st -> return st st
  let write : 'st -> ('st, unit) t = fun new_st _ -> new_st, Result.Ok ()

  let map : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t =
    fun f list ->
    let f acc el = acc >>= fun acc -> f el >>= fun el -> return (el :: acc) in
    List.fold_left f (return []) list >>| List.rev
  ;;

  let iter : ('a -> ('st, unit) t) -> 'a list -> ('st, unit) t =
    fun f list ->
    let f acc elem = acc *> f elem *> return () in
    List.fold_left f (return ()) list
  ;;

  let run : ('st, 'a) t -> 'st -> 'st * ('a, error) Result.t = fun f st -> f st
end

module TYPECHECK = struct
  open Ast
  open Common.TypeCheck
  include STATEERROR

  type 'a t = (TypeCheck.state, 'a) STATEERROR.t

  let return_with_fail = function
    | Some x -> return x
    | None -> fail (TCError OccursCheck)
  ;;

  let read_local : 'a IdMap.t t =
    read
    >>= function
    | _, l, _, _, _ -> return l
  ;;

  let read_local_el id f = read_local >>= fun l -> IdMap.find_opt id l |> f
  let read_local_el_opt id = read_local_el id return
  let read_local_el id = read_local_el id return_with_fail

  let write_local n_l =
    read
    >>= function
    | g, _, n, m, main -> write (g, n_l, n, m, main)
  ;;

  let write_local_el el_id el_ctx =
    read_local >>= fun l -> write_local (IdMap.add el_id el_ctx l)
  ;;

  let write_meth_type_opt t =
    read
    >>= function
    | g, l, n, _, main -> write (g, l, n, t, main)
  ;;

  let write_meth_type t = write_meth_type_opt (Some t)

  let read_global : 'a IdMap.t t =
    read
    >>= function
    | g, _, _, _, _ -> return g
  ;;

  let read_global_el id f = read_global >>= fun g -> IdMap.find_opt id g |> f
  let read_global_el_opt id = read_global_el id return
  let read_global_el id = read_global_el id return_with_fail

  let read_meth_type : _type option t =
    read
    >>= function
    | _, _, _, m_t, _ -> return m_t
  ;;

  let read_main_class : class_with_main option t =
    read
    >>= function
    | _, _, _, _, main -> return main
  ;;

  let write_main_class main =
    read
    >>= function
    | g, l, n, t, _ -> write (g, l, n, t, main)
  ;;

  let write_global n_g =
    read
    >>= function
    | _, l, n, m, main -> write (n_g, l, n, m, main)
  ;;

  let write_global_el el_id el_ctx =
    read_global >>= fun g -> write_global (IdMap.add el_id el_ctx g)
  ;;

  let get_curr_class_name : curr_class t =
    read
    >>= function
    | _, _, Some n, _, _ -> return n
    | _ ->
      fail (TCError (ImpossibleResult "Current class can be 'none' only before running"))
  ;;

  let write_curr_class_name n =
    read
    >>= function
    | g, l, _, t, main -> write (g, l, Some n, t, main)
  ;;
end

module INTERPRET = struct
  open Ast
  open Common
  open Common.Interpret

  type ('a, 'r) runtime_signal =
    | Pipe of 'a
    | Return of 'r

  type ('a, 'r, 'e) result =
    | Signal of ('a, 'r) runtime_signal
    | IError of 'e

  type st = Common.Interpret.state
  type ('a, 'r) t = st -> st * ('a, 'r, error) result

  let return : 'a -> ('a, 'r) t = fun x st -> st, Signal (Pipe x)
  let fail : 'e -> ('a, 'r) t = fun er st -> st, IError er
  let func_return : 'r -> ('a, 'r) t = fun x st -> st, Signal (Return x)

  let ( >>= ) : ('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t =
    fun x f st ->
    let st, x = x st in
    match x with
    | Signal (Pipe x) -> f x st
    | Signal (Return r) -> func_return r st
    | IError er -> fail er st
  ;;

  let ( <|> ) : ('a, 'r) t -> ('a, 'r) t -> ('a, 'r) t =
    fun x1 x2 st ->
    let st, x = x1 st in
    match x with
    | Signal (Pipe x) -> return x st
    | IError _ -> x2 st
    | Signal (Return r) -> func_return r st
  ;;

  let ( >>| ) : ('a, 'r) t -> ('a -> 'b) -> ('b, 'r) t =
    fun x f st ->
    let st, x = x st in
    match x with
    | Signal (Pipe x) -> return (f x) st
    | Signal (Return r) -> func_return r st
    | IError er -> fail er st
  ;;

  let ( *> ) : ('a, 'r) t -> ('b, 'r) t -> ('b, 'r) t = fun x1 x2 -> x1 >>= fun _ -> x2

  let fold_left f acc l =
    let foo acc a = acc >>= fun acc -> f acc a >>= return in
    List.fold_left foo (return acc) l
  ;;

  let map f list =
    let f' acc el = acc >>= fun acc -> f el >>= fun el -> return (el :: acc) in
    List.fold_left f' (return []) list >>| List.rev
  ;;

  let iter f list =
    let foo acc el = acc *> f el *> return () in
    List.fold_left foo (return ()) list
  ;;

  let lift2 f a b = a >>= fun r_a -> b >>= fun r_b -> return @@ f r_a r_b

  let run : ('a, 'r) t -> st * ('a, 'r, error) result =
    fun f -> f (IdMap.empty, (Idx 0, IdMap.empty), Adr 0, (Adr 0, AdrMap.empty))
  ;;

  let pipe_adr_with_fail (Adr a) = function
    | Some x -> return x
    | None -> fail (IError (AddressNotFound a))
  ;;

  let pipe_id_with_fail (Id n) = function
    | Some x -> return x
    | None -> fail (IError (NoVariable n))
  ;;

  let read : (st, 'r) t = fun st -> return st st
  let write : st -> (unit, 'r) t = fun new_st _ -> new_st, Signal (Pipe ())

  let read_local =
    read
    >>= function
    | _, l, _, _ -> return l
  ;;

  let read_local_el f name = read_local >>= fun (_, l) -> IdMap.find_opt name l |> f
  let read_local_el_opt name = read_local_el return name
  let read_local_el id = read_local_el (pipe_id_with_fail id) id

  let read_local_adr =
    read
    >>= function
    | _, _, adr, _ -> return adr
  ;;

  let read_memory =
    read
    >>= function
    | _, _, _, m -> return m
  ;;

  let read_memory_obj adr =
    read_memory >>= fun (_, m) -> AdrMap.find_opt adr m |> pipe_adr_with_fail adr
  ;;

  let write_memory n_m =
    read
    >>= function
    | g, l, adr, _ -> write (g, l, adr, n_m)
  ;;

  let write_memory_obj obj_adr obj_ctx =
    read_memory >>= fun (adr, m) -> write_memory (adr, AdrMap.add obj_adr obj_ctx m)
  ;;

  let write_local n_l =
    read
    >>= function
    | g, _, adr, m -> write (g, n_l, adr, m)
  ;;

  let write_local_el el_id el_ctx =
    read_local >>= fun (idx, l) -> write_local (idx, IdMap.add el_id el_ctx l)
  ;;

  let write_new_local_el (Id el_id) el_ctx =
    read_local_el_opt (Id el_id)
    >>= function
    | Some _ -> fail (IError (VarDeclared el_id))
    | None -> write_local_el (Id el_id) el_ctx
  ;;

  let read_global =
    read
    >>= function
    | g, _, _, _ -> return g
  ;;

  let read_global_el name =
    read_global >>= fun g -> IdMap.find_opt name g |> pipe_id_with_fail name
  ;;

  let write_global n_g =
    read
    >>= function
    | _, l, adr, m -> write (n_g, l, adr, m)
  ;;

  let write_global_el el_name el_ctx =
    read_global >>= fun g -> write_global (IdMap.add el_name el_ctx g)
  ;;

  let find_local_el id =
    let rec find_memory_obj adr =
      read_memory_obj adr
      >>= fun obj ->
      IdMap.find_opt id obj.mems
      |> function
      | Some (_, vl) -> return (Value (vl, None))
      | None ->
        (match obj.p_adr with
         | Some p_adr -> find_memory_obj p_adr
         | None -> fail (IError TypeMismatch))
    in
    let find_global_el adr =
      let f acc = function
        | IMethod (m, b) when equal_ident m.m_id id ->
          return (Some (Code (IMethod (m, b))))
        | _ -> return acc
      in
      read_memory_obj adr
      >>= fun obj ->
      read_global_el obj.cl_name
      >>= function
      | IntrClass cl ->
        fold_left f None cl.cl_body
        >>= (function
         | Some vl -> return vl
         | None ->
           (match id with
            | Id n -> fail (IError (NoVariable n))))
    in
    read_local_el id
    <|> (read_local_adr >>= fun adr -> find_memory_obj adr <|> find_global_el adr)
  ;;
end
