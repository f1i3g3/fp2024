(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module StringMap = Map.Make (String)
module Int64Map = Map.Make (Int64)
open Ast

type state =
  { registers : int64 StringMap.t
  ; pc : int64
  }

let handle_alternative_names = function
  | Zero -> X0
  | Ra -> X1
  | Sp -> X2
  | Gp -> X3
  | Tp -> X4
  | T0 -> X5
  | T1 -> X6
  | T2 -> X7
  | S0 | Fp -> X8
  | S1 -> X9
  | A0 -> X10
  | A1 -> X11
  | A2 -> X12
  | A3 -> X13
  | A4 -> X14
  | A5 -> X15
  | A6 -> X16
  | A7 -> X17
  | S2 -> X18
  | S3 -> X19
  | S4 -> X20
  | S5 -> X21
  | S6 -> X22
  | S7 -> X23
  | S8 -> X24
  | S9 -> X25
  | S10 -> X26
  | S11 -> X27
  | T3 -> X28
  | T4 -> X29
  | T5 -> X30
  | T6 -> X31
  | x -> x
;;

let init_state () =
  let registers =
    List.fold_left
      (fun acc reg -> StringMap.add (show_register reg) 0L acc)
      StringMap.empty
      [ X0
      ; X1
      ; X2
      ; X3
      ; X4
      ; X5
      ; X6
      ; X7
      ; X8
      ; X9
      ; X10
      ; X11
      ; X12
      ; X13
      ; X14
      ; X15
      ; X16
      ; X17
      ; X18
      ; X19
      ; X20
      ; X21
      ; X22
      ; X23
      ; X24
      ; X25
      ; X26
      ; X27
      ; X28
      ; X29
      ; X30
      ; X31
      ]
  in
  { registers; pc = 0L }
;;

let get_register_value state reg =
  StringMap.find_opt (show_register (handle_alternative_names reg)) state.registers
  |> Option.value ~default:0L
;;

let set_register_value state reg value =
  { state with
    registers =
      StringMap.add (show_register (handle_alternative_names reg)) value state.registers
  }
;;

let nth_opt_int64 l n =
  if n < 0L then
    failwith "Index cannot be negative"
  else
    let rec nth_aux l n =
      match l with
      | [] -> None
      | a::l -> if n = 0L then Some a else nth_aux l (Int64.sub n 1L)
    in nth_aux l n

let set_pc state new_pc = { state with pc = new_pc }
let increment_pc state = { state with pc = Int64.add state.pc 1L }

let resolve_label_including_directives program label =
  let rec aux idx = function
    | [] -> -1
    | LabelExpr lbl :: _ when lbl = label -> idx
    | _ :: tl -> aux (idx + 1) tl
  in
  aux 0 program
;;

let resolve_label_excluding_directives program label =
  let rec aux idx = function
    | [] -> -1
    | LabelExpr lbl :: _ when lbl = label -> idx
    | InstructionExpr _ :: tl -> aux (idx + 1) tl
    | _ :: tl -> aux idx tl
  in
  aux 0 program
;;

type address_info =
  | Immediate of int
  | Label of int

let get_address12_value program = function
  | ImmediateAddress12 value -> Immediate (value)
  | LabelAddress12 label -> Label (resolve_label_excluding_directives program label)
;;

let get_address20_value program = function
  | ImmediateAddress20 value -> Immediate (value)
  | LabelAddress20 label -> Label (resolve_label_excluding_directives program label)
;;

let get_address32_value program = function
  | ImmediateAddress32 value -> Immediate (value)
  | LabelAddress32 label -> Label (resolve_label_excluding_directives program label)
;;

(* Get index from including directives and labels to excluding *)
let resolve_address_incl_to_excl program immediate64_value =
  let rec traverse_program index remaining_value = function
    | [] -> failwith "End of program reached before resolving immediate address from including directives to excluding"
    | DirectiveExpr _ :: rest | LabelExpr _ :: rest -> 
        traverse_program index (Int64.sub remaining_value 1L) rest
    | InstructionExpr _ :: rest -> 
        if remaining_value = 1L then
          (Int64.add index 1L)
        else
          traverse_program (Int64.add index 1L) (Int64.sub remaining_value 1L) rest
  in
  traverse_program 0L immediate64_value program
;;

(* Get index from excluding directives and labels to including *)
let resolve_address_excl_to_incl program immediate64_value =
  let rec traverse_program index remaining_value = function
    | [] -> failwith "End of program reached before resolving immediate address from excluding directives to including"
    | DirectiveExpr _ :: rest | LabelExpr _ :: rest -> 
        traverse_program (Int64.add index 1L) remaining_value rest
    | InstructionExpr _ :: rest -> 
        if remaining_value = 1L then
          (Int64.add index 1L)
        else
          traverse_program (Int64.add index 1L) (Int64.sub remaining_value 1L) rest
  in
  traverse_program 0L immediate64_value program
;;

let rec interpret state program =
  match nth_opt_int64 program state.pc with
  | None -> state
  | Some (InstructionExpr instr) ->
    let new_state = execute_instruction state instr program in
    interpret (increment_pc new_state) program
  | Some (LabelExpr _) -> interpret (increment_pc state) program
  | Some (DirectiveExpr _) -> interpret (increment_pc state) program

and execute_instruction state instr program =
  match instr with
  | Add (rd, rs1, rs2) ->
    let val1 = get_register_value state rs1 in
    let val2 = get_register_value state rs2 in
    let result = Int64.add val1 val2 in
    set_register_value state rd result
  | Sub (rd, rs1, rs2) ->
    let val1 = get_register_value state rs1 in
    let val2 = get_register_value state rs2 in
    let result = Int64.sub val1 val2 in
    set_register_value state rd result
  | Xor (rd, rs1, rs2) ->
    let val1 = get_register_value state rs1 in
    let val2 = get_register_value state rs2 in
    let result = Int64.logxor val1 val2 in
    set_register_value state rd result
  | Or (rd, rs1, rs2) ->
    let val1 = get_register_value state rs1 in
    let val2 = get_register_value state rs2 in
    let result = Int64.logor val1 val2 in
    set_register_value state rd result
  | And (rd, rs1, rs2) ->
    let val1 = get_register_value state rs1 in
    let val2 = get_register_value state rs2 in
    let result = Int64.logand val1 val2 in
    set_register_value state rd result
  | Sll (rd, rs1, rs2) ->
    let val1 = get_register_value state rs1 in
    let val2 = get_register_value state rs2 in
    let result = Int64.shift_left val1 (Int64.to_int val2) in
    set_register_value state rd result
  | Srl (rd, rs1, rs2) ->
    let val1 = get_register_value state rs1 in
    let val2 = get_register_value state rs2 in
    let result = Int64.shift_right_logical val1 (Int64.to_int val2) in
    set_register_value state rd result
  | Sra (rd, rs1, rs2) ->
    let val1 = get_register_value state rs1 in
    let val2 = get_register_value state rs2 in
    let result = Int64.shift_right val1 (Int64.to_int val2) in
    set_register_value state rd result
  | Slt (rd, rs1, rs2) ->
    let val1 = get_register_value state rs1 in
    let val2 = get_register_value state rs2 in
    let result = if Int64.compare val1 val2 < 0 then 1L else 0L in
    set_register_value state rd result
  | Sltu (rd, rs1, rs2) ->
    let val1 = get_register_value state rs1 in
    let val2 = get_register_value state rs2 in
    let result = if Int64.unsigned_compare val1 val2 < 0 then 1L else 0L in
    set_register_value state rd result
  | Jalr (rd, rs1, imm) ->
    let val_rs1 = get_register_value state rs1 in
    let address_info = get_address12_value program imm in
    let new_pc =       
      match address_info with
        | Immediate imm_value ->
          resolve_address_excl_to_incl program (Int64.shift_right_logical (Int64.add val_rs1 (Int64.of_int imm_value)) 2)
        | Label excluding_directives_label_offset ->
          resolve_address_excl_to_incl program (Int64.add (Int64.of_int excluding_directives_label_offset) (Int64.shift_right_logical val_rs1 2))
    in
    let new_state = set_register_value state rd (Int64.add state.pc 1L) in
    set_pc new_state new_pc
  | Jr rs1 ->
    let val_rs1 = get_register_value state rs1 in
    let new_pc = resolve_address_excl_to_incl program (Int64.shift_right_logical val_rs1 2) in
    let new_state = set_register_value state X0 (Int64.add state.pc 1L) in
    set_pc new_state new_pc
  | J imm_value ->
    let address_info = get_address20_value program imm_value in
    let new_pc = 
      match address_info with
      | Immediate imm_value ->
        resolve_address_excl_to_incl program (Int64.add (Int64.of_int (imm_value lsr 2)) (resolve_address_incl_to_excl program state.pc))
      | Label excluding_directives_label_offset ->
        resolve_address_excl_to_incl program (Int64.add (Int64.of_int excluding_directives_label_offset) (resolve_address_incl_to_excl program state.pc))
    in
    let new_state = set_register_value state X0 (Int64.add state.pc 1L) in
    set_pc new_state new_pc
  | _ -> state
;;

let show_state state =
  let registers_str =
    StringMap.fold
      (fun reg value acc -> acc ^ Printf.sprintf "%s: %Ld\n" reg value)
      state.registers
      ""
  in
  let pc_str = Printf.sprintf "PC: %Ld" state.pc in
  registers_str ^ pc_str
;;

let _ =
  let initial_state = init_state () in
  let program =
    [ InstructionExpr (Add (X1, X2, X3))
    ; LabelExpr "start"
    ; InstructionExpr (Sub (X4, X5, X6))
    ; InstructionExpr (Jalr (X1, X2, ImmediateAddress12 4))
    ; InstructionExpr (Jr X3)
    ; InstructionExpr (J (ImmediateAddress20 8))
    ]
  in
  let final_state = interpret initial_state program in
  Printf.printf "Final State: %s\n" (show_state final_state)
;;
