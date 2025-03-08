let parse_module file_content =
  let open PyreAst.Parser in
  with_context (fun context ->
    match Concrete.parse_module ~context file_content with
    | Result.Error { Error.message; line; column; _ } ->
      let message =
        Printf.sprintf "Parsing error at line %d, column %d: %s" line column message
      in
      failwith message
    | Result.Ok ast -> ast)
;;
