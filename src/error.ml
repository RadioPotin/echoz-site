let error_msg code reason = Format.sprintf "<h1>Error: %d - %s</h1>" code reason

let page code reason =
  let content =
    match code with
    | 404 -> Format.sprintf {|%s<br />|} (error_msg code reason)
    | n -> error_msg n reason
  in
  content
