open Parser

module Check = struct
  type ty = Faz | Foo | Never

  let rec infer_type = function
    | Parser.ReturnStmt value -> infer_type value
    | _ -> Faz

  let rec check = function Parser.ReturnStmt _ -> [ "foo" ] | _ -> []

  let check_all ast =
    let errors = check ast in
    if errors == [] then Ok () else Error errors
end
