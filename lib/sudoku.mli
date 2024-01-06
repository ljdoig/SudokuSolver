type t

val create: int option array array -> t option
val to_string: t -> string
val read: string -> t option
val write: t -> string -> unit
val print: t -> unit
