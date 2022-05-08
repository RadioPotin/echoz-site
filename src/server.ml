let my_error_template _debug_info suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in
  let content = Error.page code reason in
  Dream.html (Template.render_unsafe ~title:"Oops" ~content ())

let asset_loader _root path _request =
  match Content.read ("assets/" ^ path) with
  | None -> Dream.empty `Not_Found
  | Some asset -> Dream.respond asset

let page path =
  match Content.read (path ^ ".md") with
  | None -> None
  | Some page -> Some (Omd.of_string page |> Omd.to_html)

let title content =
  let open Soup in
  try
    let soup = content |> parse in
    soup $ "h1" |> R.leaf_text
  with
  | Failure _e -> "Zamal Vie"

let () =
  Dream.run ~interface:"0.0.0.0"
    ~error_handler:(Dream.error_template my_error_template)
  @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/assets/**" (Dream.static ~loader:asset_loader "")
       ; Dream.get "/" (fun _request ->
             match page "index" with
             | None -> Dream.empty `Not_Found
             | Some content ->
               let title = title content in
               Dream.html (Template.render_unsafe ~title ~content ()) )
       ; Dream.get "/:page" (fun request ->
             match page (Dream.param "page" request) with
             | None -> Dream.empty `Not_Found
             | Some content ->
               let title = title content in
               Dream.html (Template.render_unsafe ~title ~content ()) )
       ]
  @@ Dream.not_found
