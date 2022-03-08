type token = Unknown | Identifier of string | Plus | EOF

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

let explode str = List.init (String.length str) (String.get str)

let lex (str : string) =
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
  String.concat ", "
    (List.map string_of_token (lex "hello_world hello world +"))
  |> print_endline
(* output: identifier(hello_world) *)
