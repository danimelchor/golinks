open Lwt.Syntax

type golink = { name : string; url : string }

module type DB = Caqti_lwt.CONNECTION

module T = Caqti_type

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let query =
    (string ->? t2 string string)
    @@ "SELECT name, url FROM golinks WHERE name LIKE ?"
end

let parse_golink (name, url) = { name; url }

let get_link_db golink (module Db : DB) =
  let* link_or_err = Db.find_opt Q.query golink in
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
  let* result = Dream.sql request (get_link_db norm_link) in
  let golink = Option.map parse_golink result in
  Dream.html (render golink)

let () =
  Dream.run @@ Dream.logger
  @@ Dream.sql_pool "sqlite3:db.sqlite"
  @@ Dream.router [ Dream.get "/**" get_link ]
