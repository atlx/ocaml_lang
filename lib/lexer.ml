module Lexer = struct
  type token =
    | Illegal of char
    | Identifier of string
    | Integer of string
    | Plus
    | Minus
    | Whitespace
    | EOF

  (** Determine whether a character is within the (inclusive) range
      of [a-z] or [A-Z], or is the [_] char. *)
  let is_identifier ch =
    ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'

  let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false
  let is_digit ch = '0' <= ch && ch <= '9'

  (** Flatten a list of characters into a string. *)
  let string_of_chars chars =
    let rec aux acc = function
      | [] -> acc
      | head :: tail -> aux (String.concat "" [ acc; String.make 1 head ]) tail
    in
    aux "" (List.rev chars)

  let lex_while predicate chars =
    let rec aux acc chars =
      match chars with
      | [] -> (string_of_chars acc, [])
      | head :: tail -> begin
        match head with
        | _ when predicate head -> aux (head :: acc) tail
        | _ -> (string_of_chars acc, chars)
      end
    in
    aux [] chars

  (** Lex the next token. If the given char list is empty, the [EOF]
      token will be returned. *)
  let lex_token chars =
    match chars with
    | [] -> (EOF, [])
    | head :: tail -> begin
      match head with
      | '+' -> (Plus, tail)
      | '-' -> (Minus, tail)
      | _ when is_whitespace head -> (Whitespace, tail)
      | _ when is_identifier head ->
        let value, tail = lex_while is_identifier chars in
        (Identifier value, tail)
      | _ when is_digit head ->
        let value, tail = lex_while is_digit chars in
        (Integer value, tail)
      | _ -> (Illegal head, tail)
    end

  (* TODO: Not adding the [EOF] token at the end. *)

  (** Collect all possible tokens from a string. *)
  let lex str =
    let rec aux acc = function
      | [] -> acc
      | chars ->
        let lex_result = lex_token chars in
        aux (fst lex_result :: acc) (snd lex_result)
    and chars =
      (* Deconstruct the string into a list of characters. *)
      List.init (String.length str) (String.get str)
    in
    aux [] chars |> List.rev

  let string_of_token = function
    | Illegal value -> "illegal(" ^ String.make 1 value ^ ")"
    | Identifier value -> "identifier(" ^ value ^ ")"
    | Integer value -> "integer(" ^ value ^ ")"
    | Plus -> "plus"
    | Minus -> "minus"
    | Whitespace -> "whitespace"
    | EOF -> "eof"
end
