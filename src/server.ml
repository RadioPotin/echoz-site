(*
  Function that returns the corresponding error as HTML response
 *)
let my_error_template _debug_info suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in
  let content = Error.page code reason in
  Dream.html (Template.render_unsafe ~title:"Oops" ~content ())

  (*
 Functions that load assets from the filetree
 *)
let asset_loader _root path _request =
  match Content.read ("assets/" ^ path) with
  | None -> Dream.empty `Not_Found
  | Some asset -> Dream.respond asset

let blog_asset_loader _root path _request =
  match Content.read ("/blog/assets/" ^ path) with
  | None -> Format.printf "%s@.@." path; Dream.empty `Not_Found
  | Some asset -> Dream.respond asset

  (*
  Functions that return the text content to the server found in [path]
 *)

  (* function [page path] returns the contents of static pages likes [index.md]*)
let page path =
  match Content.read (path ^ ".md") with
  | None -> None
  | Some page -> Some (Omd.of_string page |> Omd.to_html)

  (* function [blog path] returns the contents of static blog articles like [2022_05_25_burning_zamal_incoming.md]*)
let blog path =
  match Content.read ("blog/" ^ path ^ ".md") with
  | None -> None
  | Some blog_article -> begin
    match Blog.article_of_string blog_article path with
    | None -> None
    | Some article ->
        let header =
          Blog.specific_article_header article.title article.date
          article.category article.tags
in
      Some
        ( header ^ (Omd.of_string article.content |> Omd.to_html)
        , article.tags )
  end

  (*
  Function that extracts the page title from the first
  <h1></h1> found in the page [content]
 *)
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
       ; Dream.get "/blog/assets/**" (Dream.static ~loader:blog_asset_loader "")
       ; Dream.get "/" (fun _request ->
         match page "index" with
             | None -> Dream.empty `Not_Found
             | Some content ->
                 let title = title content in
                 Dream.html (Template.render_unsafe ~title ~content ()) )
       ; Dream.get "/blog/category" (fun _request ->
         let content = Blog.category_home in
         let title = title content in
         Dream.html (Template.render_unsafe ~title ~content ()) )
       ; Dream.get "/blog/category/:cat" (fun request ->
         let content = Blog.given_category (Dream.param "cat" request) in
         let title = title content in
         Dream.html (Template.render_unsafe ~title ~content ()) )
       ; Dream.get "/blog/:title" (fun request ->
         match blog (Dream.param "title" request) with
             | None -> Dream.empty `Not_Found
             | Some (content, tags) ->
                 let title = title content in
                 let keywords =
                   Dream.html_escape (String.concat ", " (tags @ Meta.keywords_l))
                 in
               Dream.html (Template.render_unsafe ~title ~content ~keywords ()) )
       ; Dream.get "/blog/tag/:tag" (fun request ->
         let content=
           Blog.given_tag (Dream.param "tag" request)
                 in
             let title = title content in
             Dream.html (Template.render_unsafe ~title ~content  ()) )
       ; Dream.get "/blog" (fun _request ->
         let content = Blog.home_page in
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
