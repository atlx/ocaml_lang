open Ocaml_lang.Lexer
open Ocaml_lang.Parser

let () =
  let tokens =
    read_line () |> Lexer.lex
    |> List.filter (fun tok -> tok != Lexer.Whitespace)
  in
  match Parser.parse tokens with
  | Ok ast ->
    (List.map Parser.string_of_ast) ast |> String.concat ", " |> print_endline
  | Error msg -> print_endline ("parse error: " ^ msg)
