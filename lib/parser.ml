open Lexer

module Parser = struct
  type ast = Name of string | Function of string

  let todo () = failwith "TODO: Not yet implemented"

  let parse_name = function
    | [] -> Error "reached end of file"
    | Lexer.Identifier name :: tail -> Ok (Name name, tail)
    | _ -> Error "expected identifier"

  let parse_fn = function
    | [] -> Error "reached end of file"
    | Lexer.Fn :: Lexer.Identifier name :: tail -> Ok (Function name, tail)
    | _ -> Error "expected function keyword"

  let parse tokens =
    let rec aux tokens acc =
      match tokens with
      | [] -> Ok (List.rev acc)
      | head :: _ -> begin
        match head with
        | Lexer.Identifier _ -> begin
          let parse_result = parse_name tokens in
          match parse_result with
          | Ok (name, tail) -> aux tail (name :: acc)
          | Error msg -> Error msg
        end
        | _ -> Error "unrecognized construct"
      end
    in
    aux tokens []

  (* in if error then Error message else Ok (Name "hello") *)

  let () = print_endline "hello world from parser"
end
