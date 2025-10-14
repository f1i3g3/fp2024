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

val pp_tc_error : Format.formatter -> tc_error -> unit
val show_tc_error : tc_error -> string

type interpret_error =
  | NotImplemented
  | NoVariable of string
  | AddressNotFound of int
  | VarDeclared of string
  | TypeMismatch
  | ImpossibleResult of string
  | OtherError of string

val pp_interpret_error : Format.formatter -> interpret_error -> unit
val show_interpret_error : interpret_error -> string

type error =
  | TCError of tc_error
  | IError of interpret_error

val pp_error : Format.formatter -> error -> unit
val show_error : error -> string


module Id : sig
  type t = ident
  val compare : 't -> 't -> int
end

module IdMap : sig
  include Map.S with type key = Ast.ident
end

type adr = Adr of int 

val pp_adr : Format.formatter -> adr -> unit
val show_adr : adr -> string

module Adr : sig
  type t = adr
  val compare : 't -> 't -> int
end

module AdrMap : sig
  include Map.S with type key = adr
end

type obj_content =
  | VarType of Ast.var_type
  | Method of Ast.field
  | Field of Ast.field

val pp_obj_content : Format.formatter -> obj_content -> unit
val show_obj_content : obj_content -> string
val equal_obj_content : obj_content -> obj_content -> bool

type context = TCClass of c_sharp_class

module TypeCheck : sig
  type global_env = context IdMap.t
  type local_env = obj_content IdMap.t
  type curr_class = ident
  type class_with_main = ident
  
  type state = 
    global_env * local_env * curr_class option * _type option * class_with_main option
end

module Interpret : sig
  type idx = Idx of int 

  val pp_idx : Format.formatter -> idx -> unit
  val show_idx : idx -> string

  type meth = {
    m_modifiers : modifier list;
    m_type : _type;
    m_id : ident;
    m_params : params;
  }

  type constr = {
    c_modifier : modifier list;
    c_id : ident;
    c_params : params;
  }

  type code =
    | IConstructor of constr * stmt
    | IMethod of meth * stmt

  val pp_code : Format.formatter -> code -> unit
  val show_code : code -> string

  type class_ = {
    cl_modifiers : modifier list;
    cl_id : ident;
    cl_body : code list;
  }

  type el =
    | IClass of adr
    | IValue of val_type

  val pp_el : Format.formatter -> el -> unit
  val show_el : el -> string

  type vl =
    | Init of el
    | NotInit

  val pp_vl : Format.formatter -> vl -> unit
  val show_vl : vl -> string

  type local_el =
    | Code of code
    | Value of vl * idx option

  type local_env = idx * local_el IdMap.t

  type obj = {
    mems : (field * vl) IdMap.t;
    cl_name : ident;
    p_adr : adr option;
    inh_adr : adr option;
  }

  type context = IntrClass of class_

  val pp_context : Format.formatter -> context -> unit
  val show_context : context -> string

  type memory = adr * obj AdrMap.t
  type local_adr = adr
  type global_env = context IdMap.t
  type state = global_env * local_env * local_adr * memory
end
