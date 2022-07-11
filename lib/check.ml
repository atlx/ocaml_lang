open Parser

module Check = struct
  type ty = Unknown | Foo | Never

  let ( >>= ) = 1

  let rec short_on_error application = function
    | [] -> Ok ()
    | head :: tail -> (
      match application head with
      | Error e -> Error e
      | _ -> short_on_error application tail)

  let rec infer_type = function
    | Parser.ReturnStmt value -> infer_type value
    | _ -> Unknown

  let rec check = function
    | Parser.ReturnStmt _ -> Error [ "this is an error!" ]
    | Parser.Block statements -> short_on_error check statements
    | _ -> Ok ()

  let check_all ast = check ast
end
