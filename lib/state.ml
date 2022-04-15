open Lexer

module State = struct
  type ast =
    | Name of string
    | Function of { name : string; body : ast }
    | Block of ast list
    | ReturnStmt of ast
    | IntLiteral of int

  type ('value, 'state) state = 'value * 'state
  type parser_state = ((ast, Lexer.token list) state, string) result

  type ('value_a, 'value_b, 'state) state_transformer =
    ('value_a, 'state) state -> ('value_b, 'state) state

  type parser_state_transformer = parser_state -> parser_state

  (* let map_state f (ast, (tok, str)) = (ast, f tok str) *)

  let map_state state (f : ('value_a, 'value_b, 'state) state_transformer) =
    f state

  let map_parser_state (state : parser_state) (f : parser_state_transformer) =
    f state

  let ( =>> ) a b = map_parser_state a b
  let parse_x (_state : parser_state) : parser_state = Ok (Name "x", [])

  (* let parse_top_level tokens = match tokens with | [] -> Error "unexpected
     end of file" | token :: tail -> map_parser_state Ok(Name("hi"), tail)
     parse_x *)

  (* let parse_top_level state = match state with | _, [] -> Error("unexpected
     end of file") | _ => Ok() *)

  (* let init_parse = Ok (Name "hi", []) =>> parse_x =>> pars_x *)

  (* let a = map_parser_state state f *)
end
