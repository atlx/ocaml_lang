open Lexer

module Parser = struct
  type ast =
    | Name of string
    | Function of { name : string; body : ast }
    | Block of ast list
    | ReturnStmt of ast
    | IntLiteral of int

  (** Convert a ast node to a string. Not tail-recursive. *)
  let rec string_of_ast = function
    | Name value -> "(name: " ^ value ^ ")"
    | Function { name; _ } -> "(function: " ^ name ^ ")"
    (* TODO: Display statements. *)
    | Block _ -> "(block: <statements>)"
    | ReturnStmt value -> "(return: " ^ string_of_ast value ^ ")"
    | IntLiteral value -> "(int: " ^ Int.to_string value ^ ")"

  let unexpected_token actual expected =
    Error
      begin
        "expected " ^ expected ^ ", got "
        ^ Lexer.string_of_token actual
      end

  (* let try_parse (f : Lexer.token list -> (ast * Lexer.token list, string)
     result) t = match f t with Ok v -> v | Error msg -> Error msg *)

  let parse_name = function
    | [] -> Error "reached end of file"
    | Lexer.Identifier name :: tail -> Ok (Name name, tail)
    | token :: _ -> unexpected_token token "identifier"

  let parse_expr = function
    | [] -> Error "reached end of file"
    (* TODO: Unsafe code. Raises exception on integer parsing failure. *)
    | Lexer.Integer value :: tail -> Ok (IntLiteral (int_of_string value), tail)
    | token :: _ -> unexpected_token token "expression"

  (* let ( := ) t x = match x with | Ok (value, tail) -> Ok (t value, tail) |
     Error msg -> Error msg *)

  let parse_return_stmt = function
    | [] -> Error "reached end of file"
    | Lexer.Bang :: tail -> begin
      match parse_expr tail with
      | Ok (value, tail) -> Ok (ReturnStmt value, tail)
      | error -> error
    end
    | token :: _ -> unexpected_token token (Lexer.string_of_token Lexer.Bang)

  let parse_block = function
    | [] -> Error "reached end of file"
    | Lexer.BraceL :: prime_tail ->
      let rec aux acc next_tail =
        match next_tail with
        | [] -> Error "reached end of file"
        | Lexer.BraceL :: Lexer.BraceR :: tail -> Ok (Block [], tail)
        | Lexer.BraceR :: tail -> Ok (Block (List.rev acc), tail)
        | Lexer.Bang :: _ -> begin
          match parse_return_stmt next_tail with
          | Ok (return_stmt, tail) -> aux (return_stmt :: acc) tail
          | error -> error
        end
        | token :: _ ->
          unexpected_token token (Lexer.string_of_token Lexer.BraceL)
      in
      aux [] prime_tail
    | token :: _ -> unexpected_token token (Lexer.string_of_token Lexer.BraceL)

  let parse_fn = function
    | [] -> Error "reached end of file"
    | Lexer.At :: Lexer.Identifier name :: tail -> begin
      match parse_block tail with
      | Ok (body, next_tail) -> Ok (Function { name; body }, next_tail)
      | error -> error
    end
    | token :: _ -> unexpected_token token (Lexer.string_of_token Lexer.At)

  (* TODO: What if we match the [Lexer.EOF] token? Then return? Consider having
     it be a base case. *)
  let parse tokens =
    let rec aux tokens acc =
      match tokens with
      | [] -> Ok (List.rev acc)
      | head :: _ -> begin
        let result =
          match head with
          | Lexer.Identifier _ -> parse_name tokens
          | Lexer.At -> parse_fn tokens
          | Lexer.BraceL -> parse_block tokens
          | _ -> unexpected_token head "top-level construct"
        in
        match result with
        | Ok (name, tail) -> aux tail (name :: acc)
        | Error msg -> Error msg
      end
    in
    aux tokens []
end
