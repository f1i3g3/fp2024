open Ast

module Id = struct
  type t = ident

  let compare = compare
end

module IdMap = Map.Make (Id)

type obj_content = (* TODO *)
  | VarType of var_type
  | Method of field
  | Field of field
[@@deriving show { with_path = false }, eq]

type context =
  | TCClass of c_sharp_class

module TypeCheck = struct
  type global_env = context IdMap.t
  type local_env = obj_content IdMap.t
  type curr_class = ident
  type class_with_main = ident

  type state =
    global_env
    * local_env
    * curr_class option
    * _type option
    * class_with_main option
end