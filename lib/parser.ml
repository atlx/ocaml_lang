open Lexer

module Parser = struct
  type ast =
    | Name of string
    | Function of { name : string; body : ast }
    | Block of ast list
    | ReturnStmt of ast
    | IntLiteral of int

  let eof = Error "unexpected end of file"

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
    | [] -> eof
    | Lexer.Identifier name :: tail -> Ok (Name name, tail)
    | token :: _ -> unexpected_token token "identifier"

  let parse_expr = function
    | [] -> eof
    (* TODO: Unsafe code. Raises exception on integer parsing failure. *)
    | Lexer.Integer value :: tokens ->
      Ok (IntLiteral (int_of_string value), tokens)
    | token :: _ -> unexpected_token token "expression"

  (* let ( := ) t x = match x with | Ok (value, tail) -> Ok (t value, tail) |
     Error msg -> Error msg *)

  let parse_return_stmt = function
    | [] -> eof
    | Lexer.Return :: tokens -> begin
      match parse_expr tokens with
      | Ok (value, next_tokens) -> Ok (ReturnStmt value, next_tokens)
      | error -> error
    end
    | token :: _ -> unexpected_token token (Lexer.string_of_token Lexer.Return)

  let parse_block = function
    | [] -> eof
    | Lexer.BraceL :: init_tokens ->
      let rec aux acc next_tokens =
        match next_tokens with
        | [] -> eof
        | Lexer.BraceL :: Lexer.BraceR :: tail -> Ok (Block [], tail)
        | Lexer.BraceR :: tail -> Ok (Block (List.rev acc), tail)
        | Lexer.Return :: _ -> begin
          match parse_return_stmt next_tokens with
          | Ok (return_stmt, tail) -> aux (return_stmt :: acc) tail
          | error -> error
        end
        | token :: _ ->
          unexpected_token token (Lexer.string_of_token Lexer.BraceL)
      in
      aux [] init_tokens
    | token :: _ -> unexpected_token token (Lexer.string_of_token Lexer.BraceL)

  let parse_fn = function
    | [] -> eof
    | Lexer.Fn :: Lexer.Identifier name :: tokens -> begin
      match parse_block tokens with
      | Ok (body, next_tokens) -> Ok (Function { name; body }, next_tokens)
      | error -> error
    end
    | token :: _ -> unexpected_token token (Lexer.string_of_token Lexer.Fn)

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
          | Lexer.Fn -> parse_fn tokens
          | _ -> unexpected_token head "top-level construct"
        in
        match result with
        | Ok (name, next_tokens) -> aux next_tokens (name :: acc)
        | Error msg -> Error msg
      end
    in
    aux tokens []
end
