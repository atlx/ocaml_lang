open Ocaml_lang.Lexer
open Ocaml_lang.Parser
open Ocaml_lang.Lowering

let () =
  let tokens =
    read_line () |> Lexer.lex
    |> List.filter (fun tok -> tok != Lexer.Whitespace)
  in
  match Parser.parse tokens with
  | Ok ast ->
    (List.map Parser.string_of_ast) ast |> String.concat ", " |> print_endline;
    ignore (Lowering.iter Lowering.visit Lowering.make_initial_state ast);
    print_endline (Lowering.get_output ())
  | Error msg -> print_endline ("parse error: " ^ msg)
