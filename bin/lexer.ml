module Lexer = struct
  type token = Illegal | Identifier of string | Plus | EOF

  (** Determine whether a character is within the (inclusive) range of a-z or A-Z. *)
  let is_identifier_char ch =
    ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'

  (* let is_digit ch = '0' <= ch && ch <= '9' *)

  (** Flatten a list of characters into a string. *)
  let string_of_char_list char_list =
    let rec aux acc = function
      | [] -> acc
      | head :: tail -> aux (String.concat "" [ acc; String.make 1 head ]) tail
    in
    aux "" (List.rev char_list)

  (** Create an [Identifier] token from the given character list. *)
  let lex_identifier char_list =
    let rec aux acc = function
      | [] -> (string_of_char_list acc, [])
      | head :: tail -> (
          match head with
          | _ when is_identifier_char head -> aux (head :: acc) tail
          | _ -> (string_of_char_list acc, tail))
    in
    aux [] char_list

  (** Deconstruct a string into a list of characters. *)
  let explode str = List.init (String.length str) (String.get str)

  let lex_token char_list =
    match char_list with
    | [] -> (EOF, [])
    | head :: tail -> (
        match head with
        | '+' -> (Plus, tail)
        | _ when is_identifier_char head ->
            let result = lex_identifier char_list in
            (Identifier (fst result), snd result)
        | _ -> (Illegal, tail))

  (** Collect all possible tokens from a string. *)
  let lex str =
    let rec aux char_list tok acc =
      match tok with
      | EOF -> tok :: acc
      | _ ->
          if char_list = [] then acc
          else
            let lex_result = lex_token char_list in
            aux (snd lex_result) (fst lex_result) (tok :: acc)
    in
    let char_list = explode str in
    aux char_list (lex_token char_list |> fst) [] |> List.rev

  let string_of_token = function
    | Illegal -> "unknown"
    | Identifier value -> "identifier(" ^ value ^ ")"
    | Plus -> "plus"
    | EOF -> "eof"
end

let () =
  Lexer.lex "hello world +++"
  |> List.map Lexer.string_of_token
  |> String.concat ", " |> print_endline
(*[ 'a'; 'b'; 'c' ] |> Lexer.string_of_char_list |> print_endline*)