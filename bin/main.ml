type token = Unknown | Identifier of string | Plus | EOF

(** Determine whether a character is within the (inclusive) range of a-z or A-Z. *)
let is_identifier_char ch =
  ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'

(* let is_digit ch = '0' <= ch && ch <= '9' *)

(** Flatten a list of characters into a string. *)
let string_of_char_list char_list =
  let rec aux char_list accumulator =
    match char_list with
    | [] -> accumulator
    | head :: tail ->
        aux tail (String.concat "" [ accumulator; String.make 1 head ])
  in
  aux (List.rev char_list) ""

(** Create an [Identifier] token from the given character list. *)
let lex_identifier char_list =
  let rec aux char_list accumulator =
    match char_list with
    | [] -> string_of_char_list accumulator
    | head :: tail -> (
        match head with
        | _ when is_identifier_char head -> aux tail (head :: accumulator)
        | _ -> string_of_char_list accumulator)
  in
  aux char_list []

(** Deconstruct a string into a list of characters. *)
let explode str = List.init (String.length str) (String.get str)

(** Collect all possible tokens from a string. *)
let lex str =
  let lex_token char_list =
    match char_list with
    | [] -> EOF
    | head :: _ -> (
        match head with
        | '+' -> Plus
        | _ when is_identifier_char head ->
            Identifier (lex_identifier char_list)
        | _ -> Unknown)
  in
  (* FIXME: List of list of char. *)
  List.map lex_token [ explode str ]

let string_of_token = function
  | Unknown -> "unknown"
  | Identifier value -> "identifier(" ^ value ^ ")"
  | Plus -> "plus"
  | EOF -> "eof"

let () =
  lex "hello_world hello world +"
  |> List.map string_of_token |> String.concat ", " |> print_endline
