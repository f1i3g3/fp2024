type tc_error =
  | NotImplemented
  | OccursCheck
  | AccessError
  | ImpossibleResult of string
  | TypeMismatch
  | OtherError of string
[@@deriving show { with_path = false }]
(* TODO!! *)

type error = TCError of tc_error [@@deriving show { with_path = false }]

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
  open Common
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
