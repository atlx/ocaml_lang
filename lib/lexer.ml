module Lexer = struct
  type token =
    | Illegal of char
    | Identifier of string
    | Integer of string
    | Plus
    | Minus
    | Whitespace
    | At
    | BraceL
    | BraceR
    | Bang
    | EOF

  type 'a state = 'a * char list

  (** Determine whether a character is within the (inclusive) range
      of [a-z] or [A-Z], or is the [_] char. *)
  let is_identifier = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false

  let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

  let is_digit = function '0' .. '9' -> true | _ -> false

  (** Flatten a list of characters into a string. *)
  let string_of_chars chars =
    let rec aux acc = function
      | [] -> acc
      | head :: tail -> aux (String.concat "" [ acc; String.make 1 head ]) tail
    in
    aux "" chars

  let lex_while predicate initial_state : string state =
    let rec aux acc state =
      match state with
      | [] -> (string_of_chars (List.rev acc), [])
      | head :: tail -> begin
        match head with
        | _ when predicate head -> aux (head :: acc) tail
        | _ -> (string_of_chars (List.rev acc), state)
      end
    in
    aux [] initial_state

  (* REVIEW: Why now make it return [Option] instead of [EOF]? *)

  (** Lex the next token. If the given char list is empty, the [EOF]
      token will be returned. *)
  let lex_token state : token state =
    match state with
    | [] -> (EOF, [])
    | head :: tail -> begin
      match head with
      | '+' -> (Plus, tail)
      | '-' -> (Minus, tail)
      | '@' -> (At, tail)
      | '{' -> (BraceL, tail)
      | '}' -> (BraceR, tail)
      | '!' -> (Bang, tail)
      | _ when is_whitespace head -> (Whitespace, tail)
      | _ when is_identifier head ->
        let value, tail =
          lex_while (fun ch -> is_identifier ch || is_digit ch) state
        in
        (Identifier value, tail)
      | _ when is_digit head ->
        let value, tail = lex_while is_digit state in
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
        (* REVISE: Possibly unsafe tuple access. *)
        aux (fst lex_result :: acc) (snd lex_result)
    and chars =
      (* Deconstruct the string into a list of characters. *)
      List.init (String.length str) (String.get str)
    in
    aux [] chars |> List.rev

  let string_of_token token =
    "'"
    ^ begin
        match token with
        | Illegal value -> "illegal(" ^ String.make 1 value ^ ")"
        | Identifier value -> "identifier(" ^ value ^ ")"
        | Integer value -> "integer(" ^ value ^ ")"
        | Plus -> "plus"
        | Minus -> "minus"
        | Whitespace -> "whitespace"
        | At -> "at_sign"
        | BraceL -> "left_brace"
        | BraceR -> "right_brace"
        | Bang -> "bang"
        | EOF -> "eof"
      end
    ^ "'"
end
