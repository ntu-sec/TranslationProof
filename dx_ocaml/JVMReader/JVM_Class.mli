type t;;
val read : in_channel -> t;;
val get_method : string -> string -> t -> JVM_Method.t;;
val get_methods_key : t -> (string * string) list;;
val get_class_name : t -> string;;