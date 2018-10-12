type program


type stmt =
  For of expr * expr * expr * stmt
  ForNode of string * string * stmt
  ForEdge of string * string * stmt
  While of expr * stmt