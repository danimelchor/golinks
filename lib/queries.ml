open Caqti_request.Infix
open Caqti_type.Std

let find_golink =
  (string ->? t2 string string)
  @@ "SELECT name, url FROM golinks WHERE name LIKE ?"

let create_golink =
  (t2 string string ->. unit)
  @@ "INSERT INTO golinks (name, url) VALUES ($1, $2)"
