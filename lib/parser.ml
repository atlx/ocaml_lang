module Parser = struct
  type ast = Name of string

  let todo () = failwith "TODO: Not yet implemented"

  (* let expect token = function | [] -> failwith "Expected token: " ^
     Lexer.string_of_token token ^ " but got end of file" | head :: tail -> todo
     () *)

  (* let parse_name = function [] -> todo () | head :: tail -> 2 *)
  let () = print_endline "hello world from parser"
end
