open Ocaml_lang.Lexer

let () =
  read_line () |> Lexer.lex
  |> List.filter (fun tok -> tok != Lexer.Whitespace)
  |> List.map Lexer.string_of_token
  |> String.concat ", " |> print_endline
