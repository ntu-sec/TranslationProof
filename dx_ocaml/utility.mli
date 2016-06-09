val pop_n : ('a list) -> int -> ('a list * 'a list);;
val list_to_StringMap : (string * 'a) list -> 'a Map_type.StringMap.t;;
val list_to_intMap : (int * 'a) list -> 'a Map_type.Int32Map.t;;
val readChar : in_channel -> char
val readByte : in_channel -> int
val read_n : (in_channel -> 'a) -> in_channel -> int -> 'a list
val int_iterator : int -> int list
val readShort : in_channel -> int
val readInt : in_channel -> int
val string_of_short_value : int -> string
val string_of_byte_value : int -> string
val string_of_byte_list : int list -> string
val signed_byte : int -> int
val signed_int : int -> int
val countArgs : string -> int -> int -> int
val countRet_from_desc : string -> int
val countArgs_from_desc : string -> int
val hex_string_to_int : string -> int
val int_to_hex_string : int -> string
val print_list : 'a list -> ('a -> unit) -> unit
val string_of_int_list : int list -> (int -> string) -> string
