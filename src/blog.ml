type article =
  { date : int * int * int
  ; title : string
  ; tags : string list
  ; category : string
  ; content : string
  ; url : string
  }

let footer = {|> Zamal is Life Zamal is Life|}

let compare_articles a1 a2 = compare (a2.date, a2.title) (a1.date, a1.title)

let error msg =
  Format.eprintf "error: %s@." msg;
  Format.pp_print_flush Format.err_formatter ();
  exit 1

(* The code below is used to extract a small preview from the content field of an article type with the help of Omd *)
open Omd

let handle_html html =
  let c = Markup.string html |> Markup.parse_html |> Markup.signals in
  let buff = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buff in
  Markup.iter
    (fun element ->
      match element with
      | `End_element -> ()
      | `Start_element (_, _) -> ()
      | `Text ls -> List.iter (fun s -> Format.fprintf fmt "%s " s) ls
      | _ -> () )
    c;
  Format.pp_print_flush fmt ();
  Buffer.contents buff

let rec handle_inline = function
  | Concat (_attr, attr_inline_list) ->
    List.fold_left (fun acc i -> acc ^ handle_inline i) "" attr_inline_list
  | Text (_attr, s) -> s
  | Emph (_attr, attr_inline) -> handle_inline attr_inline
  | Strong (_attr, attr_inline) -> handle_inline attr_inline
  | Code (_attr, s) -> s
  | Hard_break _attr -> " "
  | Soft_break _attr -> " "
  | Link (_attr, attr_link) -> handle_inline attr_link.label
  | Image (_attr, attr_link) -> handle_inline attr_link.label
  | Html (_attr, str) -> handle_html str

let rec handle_block = function
  | Paragraph (_attr, inline) -> handle_inline inline
  | List (_attr, _list_type, _list_spacing, attr_block_list_list) ->
    Format.asprintf "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
         (fun fmt block -> Format.fprintf fmt "%s" (handle_block block)) )
      (List.flatten attr_block_list_list)
  | Blockquote (_attr, attr_block_list) ->
    Format.asprintf "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
         (fun fmt block -> Format.fprintf fmt "%s" (handle_block block)) )
      attr_block_list
  | Thematic_break _attr -> ""
  | Heading (_attr, _i, attr_inline) -> handle_inline attr_inline
  | Code_block (_attr, _s1, _s2) -> ""
  | Html_block (_attr, str) -> handle_html str
  | Definition_list (_attr, _attr_def_elt_list) -> ""

let find_preview n doc =
  let str =
    String.concat " "
    @@ List.filter
         (fun x -> x <> "")
         (List.map (fun block -> handle_block block) doc)
  in
  try String.sub str 0 n with
  | Invalid_argument _s -> str

let preview article_content =
  let content = Omd.of_string article_content |> find_preview 300 in
  Format.sprintf "%s..." content
(* The code above is used to extract a small preview from the content field of an article type with the help of Omd *)

(** [normalize_url target] takes a target URL and rids it of unwanted
    characters, such as utf8, and spaces *)
let normalize_url target =
  String.map
    (function
      | ' ' -> '_'
      | c -> c |> Char.lowercase_ascii )
    (Ubase.from_utf8 target)

(** [normalize_tag tag] takes a tag and rids it of unwanted characters, such as
    utf8, speces, dashes and underscores *)
let normalize_tag tag =
  let string_map_partial f s =
    let b = Buffer.create (String.length s) in
    let maybeadd c =
      match f c with
      | None -> ()
      | Some c' -> Buffer.add_char b c'
    in
    String.iter maybeadd s;
    Buffer.contents b
  in
  string_map_partial
    (function
      | '-'
      | ' '
      | '_' ->
        None
      | c -> Some (Char.lowercase_ascii c) )
    (Ubase.from_utf8 tag)

(** [allowed_categories] is a list of allowed categories for any article posted
    on the blog *)
let allowed_categories =
  [ "Evenements"
  ]

(** [raw_articles] List of all raw text in all articles in /content/blog/
    subdirectory *)
let raw_articles =
  List.find_all
    (fun file ->
      (String.length file >= 5 && String.equal (String.sub file 0 5) "blog/")
      && Filename.check_suffix file ".md" )
    Content.file_list

(** [get_meta_value field] extract the second field of meta_data required at the
    beginning of the article *)
let get_meta_value field = List.hd (List.rev (String.split_on_char '=' field))

(** [extract_date date] convert date meta_data into a [(int * int * int)] type *)
let extract_date date =
  match String.split_on_char '-' date with
  | [ year; month; day ] ->
    (int_of_string year, int_of_string month, int_of_string day)
  | _ -> (0, 0, 0)

(** [article_footer category tags] returns a footer according to a specific type
    of article, . While quite generic at the moment, it will be possible to make
    it return more specific footers based on a given article's Category and Tags *)
let article_footer category _tags =
  match category with
  | _ -> footer

(** [article_of_string post url] convert a given raw_text article into an
    [article] type *)
let article_of_string post url =
  match String.split_on_char '\n' post with
  | title :: date :: category :: tags :: r ->
    let date = extract_date (get_meta_value date) in
    let title = get_meta_value title in
    let tags =
      let tags = get_meta_value tags in
      match String.split_on_char ',' tags with
      | [ "" ] -> [ "Unspecified tags." ]
      | tags -> tags
    in
    let category =
      let c = get_meta_value category in
      if not @@ List.mem c allowed_categories then (
        let err_msg =
          Format.asprintf {|Category (%s) invalid, try: %a@.|} c
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
               Format.pp_print_string )
            allowed_categories
        in
        Format.pp_print_flush Format.str_formatter ();
        error err_msg
      );
      c
    in
    let content =
      String.concat "\n" r ^ {|<br /><hr class="featurette-divider"/>|}
      ^ article_footer category tags
    in
    Some { date; title; tags; category; content; url }
  | _ -> None

(** [get_article_data raw_articles] Returns a list of [article] types *)
let get_article_data raw_articles =
  List.map
    (fun article ->
      match Content.read article with
      | None -> failwith "Couldn't read article data"
      | Some data -> (
        match
          article_of_string data
            (Filename.basename (Filename.chop_suffix article ".md"))
        with
        | None -> failwith "Invalid article data"
        | Some data -> data ) )
    raw_articles

(** [articles_data] A list of [article] types *)
let articles_data = get_article_data raw_articles

(** [categories_count] List of all categories with their corresponding count of
    written articles *)
let categories_count =
  let tbl = Hashtbl.create 512 in
  List.iter
    (fun article ->
      match Hashtbl.find_opt tbl article.category with
      | None -> Hashtbl.add tbl article.category 1
      | Some count -> Hashtbl.replace tbl article.category (count + 1) )
    articles_data;
  let l = Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [] in
  List.sort (fun (k1, v1) (k2, v2) -> compare (v2, k2) (v1, k1)) l

let pp_list_to_blog_links t normalize =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
    (fun fmt e ->
      Format.fprintf fmt {|<a href="/blog/%s/%s">%s</a>|} t (normalize e) e )

let links_to_home_pages =
  Format.sprintf
    {|
    <div class="row">
      <div class="col-lg-6" align="left">
        <p class="toplinks2">
          <a href="/blog">
            Home
          </a>
        </p>
        <p class="toplinks">
          <img class="blogimg" src="/blog/assets/img/icon_home.svg"/>
        </p>
      </div>
      <div class="col-lg-6" align="right">
        <p class="toplinks3">
          <img class="blogimg" src="/blog/assets/img/icon_category.svg"/>
        </p>
        <p class="toplinks">
          <a href="/blog/category">
           Les Quoi
        </p>
          </a>
        </p>
      </div>
    </div>
    <hr class="featurette-divider"/>|}

let pp_article_excerpt fmt article =
  let year, month, day = article.date in
  Format.fprintf fmt
    {|
    <div class="row">
      <h3><a href="/blog/%s">%s</a></h3>
    </div>
    <div class="row">
      <div class="col-lg-3">
        <img class="icon" src="/blog/assets/img/icon_clock.svg"/>
        Quand: %4d-%02d-%02d
      </div>
      <div class="col-lg-3">
        <img class="icon" src="/blog/assets/img/icon_category.svg"/>
        Quoi: <a href="/blog/category/%s">%s</a>
      </div>
      <div class="col-lg-5">
        <img class="icon" src="/blog/assets/img/icon_panneau_directions.svg"/>
        Encore Quoi: %a
      </div>
    </div>
    <br />
    %s <a href="/blog/%s">(Read more)</a>|}
    article.url article.title
    year month day
    (normalize_url article.category)
    article.category
    (pp_list_to_blog_links "tag" normalize_tag)
    article.tags (preview article.content) article.url

let pp_blog_posts fmt articles_data_list =
  Format.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () ->
         Format.fprintf fmt {|<hr class="featurette-divider" /><br />|} )
       pp_article_excerpt )
    articles_data_list

(** [specific_article_header title author (year, month, day) category tags]
    prints the header for a given blog post *)
let specific_article_header title (year, month, day) category tags =
  Format.asprintf
    {|<h1 id="page-title">%s</h1>
    <div class="row">
      <div class="col-lg-4">
      <img class="icon" src="/blog/assets/img/icon_clock.svg"/>
        Quand: %4d-%02d-%02d
      </div>
      <div class="col-lg-4">
      <img class="icon" src="/blog/assets/img/icon_category.svg"/>
        Quoi: <a href="/blog/category/%s">%s</a>
      </div>
      <div class="col-lg-4">
      <img class="icon" src="/blog/assets/img/icon_panneau_directions.svg"/>
        Encore Quoi: %a
      </div>
    </div>
    <br />
    <hr class="featurette-divider"/>
    <br />|}
    title
    year month day (normalize_url category) category
    (pp_list_to_blog_links "tag" normalize_tag)
    tags

(** [given_category category] Displays the list of articles corresponding to the
    request category *)
let given_category category =
  let articles_by_date = List.sort compare_articles articles_data in
  let articles_of_category =
    List.filter
      (fun article -> String.equal (normalize_url article.category) category)
      articles_by_date
  in
  let category = (List.hd articles_of_category).category in
  Format.asprintf {|<h1 id="page-title">Quoi: %s</h1>%s%a@.|} category
    links_to_home_pages pp_blog_posts articles_of_category

(** [given_tag tag] Displays the list of articles tagged with given [tag] *)
let given_tag tag =
  let articles_by_date = List.sort compare_articles articles_data in
  let articles_with_tag =
    List.filter
      (fun article ->
        List.exists
          (fun tag0 -> String.equal (normalize_tag tag0) tag)
          article.tags )
      articles_by_date
  in
  let tag =
    List.find
      (fun t -> String.equal (normalize_tag t) tag)
      (List.hd articles_with_tag).tags
  in
  Format.asprintf {|<h1 id="page-title">Encore Quoi: %s</h1>%s%a@.|} tag
    links_to_home_pages pp_blog_posts articles_with_tag

(** [category_home] This is the home page for all available categories on the
    Blog, along with number of articles of given category *)
let category_home =
  Format.asprintf {|<h1 id="page-title">Les Quoi du Blog</h1>%s%a@.|}
    links_to_home_pages
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "<br />")
       (fun fmt (category, count) ->
         Format.fprintf fmt
           {|<h3><a href="/blog/category/%s">%s</a> (%d %s)</h3>|}
           (normalize_url category) category count
           ( if count > 1 then
             "articles"
           else
             "article" ) ) )
    categories_count

(** [home_page] this is the home page for the blog, articles appear as excerpts
    from most recent to oldest *)
let home_page =
  let articles_by_date = List.sort compare_articles articles_data in
  Format.asprintf {|<h1 id="page-title">Blog</h1>%s%a@.|} links_to_home_pages
    pp_blog_posts articles_by_date
