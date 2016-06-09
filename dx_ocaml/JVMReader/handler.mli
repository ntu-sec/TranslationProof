type t;;
val startPC : t -> int;;
val endPC : t -> int;;
val handlerPC : t -> int;;
val catchType : t -> Constant.ClassConstant.t;;
val startPC32 : t -> int32;;
val endPC32 : t -> int32;;
val handlerPC32 : t -> int32;;
val create : int -> int -> int -> Constant.ClassConstant.t -> t;;

val read_handler : Map_type.constantMap -> in_channel -> (int * t list);;

val print_handlers : (t list) -> (out_channel) -> unit;;

val handlers_for : (t list) -> int -> (t list);;
