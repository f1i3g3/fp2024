(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Monads.TYPECHECK
open Common

val typecheck : c_sharp_class -> TypeCheck.state * (unit, error) result
val typecheck_main : c_sharp_class -> Ast.ident option * (unit, Common.error) result
