(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type tc_error =
  | NotImplemented
  | OccursCheck
  | AccessError
  | ImpossibleResult of string
  | TypeMismatch
  | OtherError of string
[@@deriving show { with_path = false }]

type interpret_error =
  | NotImplemented
  | NoVariable of string
  | AddressNotFound of int
  | VarDeclared of string
  | TypeMismatch
  | ImpossibleResult of string
  | OtherError of string
[@@deriving show { with_path = false }]

type error =
  | TCError of tc_error
  | IError of interpret_error
[@@deriving show { with_path = false }]

module Id = struct
  type t = ident

  let compare = compare
end

module IdMap = Map.Make (Id)

type adr = Adr of int [@@deriving show { with_path = false }]

module Adr = struct
  type t = adr

  let compare = compare
end

module AdrMap = Map.Make (Adr)

type obj_content =
  (* TODO *)
  | VarType of var_type
  | Method of field
  | Field of field
[@@deriving show { with_path = false }, eq]

type context = TCClass of c_sharp_class

module TypeCheck = struct
  type global_env = context IdMap.t
  type local_env = obj_content IdMap.t
  type curr_class = ident
  type class_with_main = ident

  type state =
    global_env * local_env * curr_class option * _type option * class_with_main option
end

module Interpret = struct
  type idx = Idx of int [@@deriving show { with_path = false }]

  (* TODO: proper records! *)
  type meth =
    { m_modifiers : modifier list
    ; m_type : _type
    ; m_id : ident
    ; m_params : params
    }
  [@@deriving show { with_path = false }, eq]

  type constr =
    { c_modifier : modifier list
    ; c_id : ident
    ; c_params : params
    }
  [@@deriving show { with_path = false }, eq]

  type code =
    | IConstructor of constr * stmt
    | IMethod of meth * stmt
  [@@deriving show { with_path = false }, eq]
  (* TODO: proper records! *)

  type class_ =
    { cl_modifiers : modifier list
    ; cl_id : ident
    ; cl_body : code list
    }
  [@@deriving show { with_path = false }, eq]

  type el =
    | IClass of adr
    | IValue of val_type
  [@@deriving show { with_path = false }]

  type vl =
    | Init of el
    | NotInit
  [@@deriving show { with_path = false }]

  type local_el =
    | Code of code
    | Value of vl * idx option

  type local_env = idx (* new idx *) * local_el IdMap.t

  type obj =
    { mems : (field * vl) IdMap.t
    ; cl_name : ident
    ; p_adr : adr option
    ; inh_adr : adr option
    }

  type context = IntrClass of class_ [@@deriving show { with_path = false }]
  type memory = adr * obj AdrMap.t
  type local_adr = adr
  type global_env = context IdMap.t
  type state = global_env * local_env * local_adr * memory
end
