module Lexer = struct
  type token =
    | Illegal
    | Identifier of string
    | Integer of string
    | Plus
    | Minus
    | Whitespace
    | EOF

  (** Determine whether a character is within the (inclusive) range of a-z or A-Z. *)
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
      | head :: tail -> (
          match head with
          | _ when predicate head -> aux (head :: acc) tail
          | _ -> (string_of_chars acc, chars))
    in
    aux [] chars

  let lex_token chars =
    match chars with
    | [] -> (EOF, [])
    | head :: tail -> (
        match head with
        | '+' -> (Plus, tail)
        | '-' -> (Minus, tail)
        | _ when is_whitespace head -> (Whitespace, tail)
        | _ when is_identifier head ->
            let identifier = lex_while is_identifier chars in
            (Identifier (fst identifier), snd identifier)
        | _ when is_digit head ->
            let integer = lex_while is_digit chars in
            (Integer (fst integer), snd integer)
        | _ -> (Illegal, tail))

  (* TODO: Not adding the [EOF] token at the end. *)

  (** Collect all possible tokens from a string. *)
  let lex str =
    let rec aux acc = function
      | [] -> acc
      | chars ->
          let lex_result = lex_token chars in
          aux (fst lex_result :: acc) (snd lex_result)
    in
    (* Deconstruct the string into a list of characters. *)
    let chars = List.init (String.length str) (String.get str) in
    aux [] chars |> List.rev

  let string_of_token = function
    | Illegal -> "unknown"
    | Identifier value -> "identifier(" ^ value ^ ")"
    | Integer value -> "integer(" ^ value ^ ")"
    | Plus -> "plus"
    | Minus -> "minus"
    | Whitespace -> "whitespace"
    | EOF -> "eof"
end

let () =
  read_line () |> Lexer.lex
  |> List.map Lexer.string_of_token
  |> String.concat ", " |> print_endline
