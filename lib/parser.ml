open Lexer

module Parser = struct
  type ty = Unknown | Foo | Never

  type ast =
    | Name of string
    | Function of { name : string; prototype : ast; body : ast }
    | BlockExpr of ast list
    | ReturnStmt of ast
    | IntLiteral of int
    | ExternFunction of { name : string; prototype : ast }
    | Parameter of { name : string; type_hint : ty option }
    | Prototype of {
        parameters : ast list;
        return_type_hint : ty option;
        is_variadic : bool;
        is_extern : bool;
        accepts_instance : bool;
        this_parameter : ast option;
      }

  (** Convert a ast node to a string. Not tail-recursive. *)
  let rec string_of_ast = function
    | Name value -> "(name: " ^ value ^ ")"
    | Function { name; _ } -> "(function: " ^ name ^ ")"
    (* TODO: Display statements. *)
    | BlockExpr _ -> "(block: <statements>)"
    | ReturnStmt value -> "(return: " ^ string_of_ast value ^ ")"
    | IntLiteral value -> "(int: " ^ Int.to_string value ^ ")"
    | _ -> "(unnamed)"

  let rec traverse (visitor : ast -> 'a list) = function
    | Function { name = _; prototype = _; body } ->
      (* TODO: Concat? *)
      ignore (visitor body);
      traverse visitor body
    | BlockExpr statements -> List.map visitor statements
    | ReturnStmt value -> traverse visitor value
    | ast -> [ visitor ast ]

  let eof = Error "unexpected end of file"

  let unexpected_token actual expected =
    Error
      begin
        "expected " ^ expected ^ ", got " ^ Lexer.string_of_token actual
      end

  (* let try_parse (f : Lexer.token list -> (ast * Lexer.token list, string)
     result) t = match f t with Ok v -> v | Error msg -> Error msg *)

  let parse_name = function
    | [] -> eof
    | Lexer.Identifier name :: tail -> Ok (Name name, tail)
    | token :: _ -> unexpected_token token "identifier"

  let parse_expr = function
    | [] -> eof
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

  let parse_type = function
    | [] -> eof
    | Lexer.Identifier name :: tokens -> Ok (Unknown, tokens)
    | token :: _ -> unexpected_token token "type"

  let parse_parameter = function
    | [] -> eof
    | tokens -> begin
      match parse_name tokens with
      | Ok (name, next_tokens) -> (
        match tokens with
        | Lexer.Colon :: tail -> Ok (name, next_tokens)
        | _ -> None)
    end
    | token :: _ -> unexpected_token token "parameter name"

  let parse_prototype = function
    | [] -> eof
    | Lexer.ParenthesesL :: tokens ->
      Ok
        ( Prototype
            {
              parameters = [];
              return_type_hint = None;
              is_variadic = false;
              is_extern = false;
              accepts_instance = false;
              this_parameter = None;
            },
          tokens )
    | token :: _ ->
      unexpected_token token (Lexer.string_of_token Lexer.ParenthesesL)

  let parse_block_expr = function
    | [] -> eof
    | Lexer.BraceL :: init_tokens ->
      let rec aux acc next_tokens =
        match next_tokens with
        | [] -> eof
        | Lexer.BraceL :: Lexer.BraceR :: tail -> Ok (BlockExpr [], tail)
        | Lexer.BraceR :: tail -> Ok (BlockExpr (List.rev acc), tail)
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
      match parse_block_expr tokens with
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
