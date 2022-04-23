open Parser

module Lowering = struct
  (* type value = | Llvm.llvalue | unit *)

  type 'a state = 'a * Llvm.llvalue * Llvm.llbuilder

  let get_state state f = f state

  let modify_state (state : 'a state) f : 'a state =
    let value, fn, block = state in
    f (value, fn, block)

  let set_unit_state state =
    modify_state state (fun (_, fn, block) -> ((), fn, block))

  let rec iter visitor state = function
    | [] -> state
    | head :: tail ->
      let next_state = visitor state head in
      iter visitor next_state tail

  let context = Llvm.global_context ()
  let module_ = Llvm.create_module context "program"
  let main_type = Llvm.function_type (Llvm.i32_type context) [||]
  let main = Llvm.define_function "main" main_type module_

  let visit_int_literal int_literal state =
    match int_literal with
    | Parser.IntLiteral int_value ->
      let value = Llvm.const_int (Llvm.i32_type context) int_value in
      modify_state state (fun (_, fn, block) -> (value, fn, block))
    | _ -> state

  let visit_block block visitor state =
    let first_state =
      modify_state state (fun (value, fn, _) ->
          let new_block = Llvm.append_block context "bb" main in
          (value, fn, Llvm.builder_at_end context new_block))
    in
    match block with
    | Parser.Block statements ->
      let next_state = iter visitor first_state statements in
      (* set_unit_state next_state *)
      next_state
    (* REVIEW: What if it's not an actual block ast? *)
    | _ -> first_state

  let visit_return_stmt return_stmt visitor state =
    match return_stmt with
    | Parser.ReturnStmt expr ->
      let next_state = visitor state expr in
      get_state next_state (fun (value, fn, builder) ->
          (* TODO: Must we ensure that the value isn't unit (or None)? *)
          (Llvm.build_ret value builder, fn, builder))
    | _ -> state

  let visit_function fn visitor state =
    match fn with
    | Parser.Function { name = _; body } -> visit_block body visitor state
    | _ -> state

  let rec visit state = function
    | Parser.Function _ as fn -> visit_function fn visit state
    | Parser.Block _ as block -> visit_block block visit state
    | Parser.ReturnStmt _ as return_stmt ->
      visit_return_stmt return_stmt visit state
    | Parser.IntLiteral _ as int_literal -> visit_int_literal int_literal state
    | _ -> state

  let make_initial_state =
    let entry_block = Llvm.entry_block main in
    let builder = Llvm.builder_at_end context entry_block in
    (* TODO: Not using unit or Option/None value. *)
    (Llvm.const_null (Llvm.i1_type context), main, builder)

  let get_output () = Llvm.string_of_llmodule module_
end
