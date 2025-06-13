open Lwt.Syntax

module type DB = Caqti_lwt.CONNECTION

module T = Caqti_type

type golink = { name : string; url : string }

let parse_golink (name, url) = { name; url }

let get_golink_db golink (module Db : DB) =
  let* link_or_err = Db.find_opt Queries.find_golink golink in
  Caqti_lwt.or_fail link_or_err

let create_golink_db golink (module Db : DB) =
  let* link_or_err = Db.exec Queries.create_golink golink in
  Caqti_lwt.or_fail link_or_err

let normalize_link link =
  let f p = String.lowercase_ascii p |> Str.split (Str.regexp "[-/ ]+") in
  let mapped = List.map f link in
  let unified = List.flatten mapped in
  String.concat " " unified

let render maybe_golink =
  match maybe_golink with
  | Some golink -> golink.name ^ " -> " ^ golink.url
  | None -> "Whoops..."

let get_link request =
  let path = (Dream.path request [@warning "-3"]) in
  let norm_link = normalize_link path in
  let* result = Dream.sql request (get_golink_db norm_link) in
  let golink = Option.map parse_golink result in
  Dream.html (render golink)

let create_post request =
  let* req = Dream.form request in
  match req with
  | `Ok [ ("name", name); ("url", url) ] ->
      let* () = Dream.sql request (create_golink_db (name, url)) in
      Dream.html "Ok"
  | _ -> Dream.empty `Bad_Request

let () =
  Dream.run @@ Dream.logger
  @@ Dream.sql_pool "sqlite3:db.sqlite"
  @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream.get "/" (fun r -> Dream.html (Templates.show_form r));
         Dream.post "/" create_post;
         Dream.get "/**" get_link;
       ]
