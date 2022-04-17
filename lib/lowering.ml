open Parser

type 'a state = 'a * Llvm.llvalue * Llvm.llbasicblock

let modify_state (state : 'a state) f : 'a state =
  let value, fn, block = state in
  f (value, fn, block)

let context = Llvm.global_context ()
let module_ = Llvm.create_module context "program"
let main_type = Llvm.function_type (Llvm.i32_type context) [||]
let main = Llvm.define_function "main" main_type module_
let builder = Llvm.builder context

let visit_block_state block (visit : unit state -> Parser.ast -> unit)
    (state : unit state) : unit state =
  let next_state =
    modify_state state (fun (value, fn, _) ->
        (value, fn, Llvm.append_block context "block" main))
  in
  match block with
  | Parser.Block statements ->
    List.iter (fun el -> visit next_state el) statements;
    next_state
  | _ ->
    ();
    next_state
(* let (statements) = block in List.iter statements visit in modify_state state
   (fun (value, fn, _) -> (value, fn, new_llvm_block)) *)

let visit_block block visit =
  (* Llvm.append_block context "block" main; *)
  match block with
  | Parser.Block statements -> begin
    match statements with [] -> () | lst -> List.iter visit lst
  end
  | _ -> ()

module Lowering = struct
  (* let visit_return_stmt visit return_stmt *)

  let rec visit (state : unit state) = function
    (* | Parser.Block _ as block -> visit_block block visit *)
    (* | Parser.ReturnStmt _ as return_stmt -> visit *)
    | _ -> ()

  let run () = print_endline (Llvm.string_of_llvalue main)
end
