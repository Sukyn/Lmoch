type t =
    { id: int;
      name: string;
      kind: kind; }

and kind =
  | Stream
  | Node
  | Prim

let make : string -> kind -> t =
  let counter = ref 0 in
  fun name kind ->
    if !counter = max_int then failwith "Maximum number of identifiers reached";
    incr counter;
    { id = !counter;
      name = name;
      kind = kind; }

let compare = compare

let print fmt identifier =
  Format.fprintf fmt "%s__%i" identifier.name identifier.id
