open Parser

module Check = struct
  let rec short_on_error application = function
    | [] -> Ok ()
    | head :: tail -> (
      match application head with
      | Error e -> Error e
      | _ -> short_on_error application tail)

  let rec aggregate_diagnostics f values = List.map f values |> List.flatten

  let rec infer_type = function
    | Parser.ReturnStmt value -> infer_type value
    | _ -> Parser.Unknown

  let rec check = function
    | Parser.ReturnStmt _ -> [ Error "foo" ]
    | Parser.Block statements -> aggregate_diagnostics check statements
    | _ -> []

  let check_all ast = check ast
end
