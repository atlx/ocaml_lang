module BindPractice = struct
  type state = { chars : char list }

  let parse_a state =
    match state with
    | { chars = [] } -> failwith "unexpected end of input"
    | { chars = head :: tail } -> begin
      match head with 'a' -> { chars = tail } | _ -> failwith "expected 'a'"
    end

  let bind fn next =
    let next_state = fn () in
    next next_state

  let parse_top_level chars =
    match chars with
    | [] -> failwith "unexpected end of input"
    | _ -> parse_a { chars } |> parse_a
end
