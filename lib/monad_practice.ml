(* From 'Ocaml programming: Correct + Efficient + Beautiful' *)
module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >== ) : 'a t -> ('a -> 'a t) -> 'a t
end

(* From 'Ocaml programming: Correct + Efficient + Beautiful' *)
module CheckResult : Monad = struct
  type 'a t = 'a option

  let return x = Some x
  let ( >== ) m f = match m with None -> None | Some x -> f x
end

module MonadPractice = struct
  let increment (x : int option) factor = Option.map (fun x -> x + factor) x
  let maybe_incremented = increment (Some 2)
  let maybe_incremented_2 = increment None

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
