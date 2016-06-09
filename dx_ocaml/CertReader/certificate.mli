module Levels_relationship : sig
    type t;;
    val leq : int -> int -> t -> bool;;
    val leq32 : int32 -> int32 -> t -> bool;;
    val add : int32 -> int32 -> t -> t;;
    val initial_map : int -> t;;
    val to_byteString : t -> string;;
  end

module Level : sig
    type basic_level
    type ext_level
    val basic_level_id : basic_level -> int
    val ext_level_ids : ext_level -> int list
    val create_basic_level : int -> string -> basic_level
    val create_ext_level : basic_level -> ext_level -> ext_level
    val simple_ext_level : basic_level -> ext_level
    val get_basic_level : ext_level -> basic_level
    val bot_level : basic_level
    val string_of_basic_level : basic_level -> string
    val string_of_ext_level : ext_level -> string
    val string_of_level_list : ext_level list -> string
    val byteString_of_basic_level : basic_level -> string
    val leq_ext : ext_level -> ext_level -> Levels_relationship.t -> bool
    val lub_ext : basic_level -> ext_level -> Levels_relationship.t -> ext_level
    val lub : basic_level -> basic_level -> Levels_relationship.t -> basic_level
  end;;

module Level_pool : sig
    type t;;
    val length : t -> int;;
    val level : t -> int -> Level.basic_level;;
    val create_levels : int -> Level.basic_level list -> t;;
    val to_string : t -> int -> string;;
    val get_id_list : t -> int list;;
    val to_strings : t -> string list;;
    val empty : t;;
    val to_byteString :  t -> string;;
  end;;

module type Ins_cert_type = sig
    type t;;
    val to_string : t -> string;;
    val to_byteString : t -> string;;
    val empty : t;;
  end
type registersType_t = {rt:Level.ext_level Map_type.Int32Map.t; res:Level.ext_level option;
                        flag:int};;
module StackType : Ins_cert_type with type t = Level.ext_level list ;;
module RegistersType : Ins_cert_type with type t = registersType_t;;

module ST_op : sig
    val pop_n : StackType.t -> int -> (StackType.t * StackType.t);;
    val lub_st : StackType.t -> StackType.t -> Levels_relationship.t -> (bool * StackType.t);;
    val add : Level.ext_level -> StackType.t -> StackType.t;;
    val length : StackType.t -> int;;
    val top : StackType.t -> Level.ext_level;;
    val lift_st : StackType.t -> Level.basic_level -> Levels_relationship.t -> StackType.t;;
  end;;

module RT_op : sig
    val add : int -> Level.ext_level -> RegistersType.t -> RegistersType.t;;
    val get : int -> RegistersType.t -> Level.ext_level;;
    val create : Level.ext_level Map_type.Int32Map.t -> Level.ext_level option -> int ->
                 RegistersType.t;;
    val update_flag : int -> RegistersType.t -> RegistersType.t;;
    val add_missing_registers : Level.ext_level -> int -> RegistersType.t ->
                                RegistersType.t;;
  end;;

module type Ins_typing_type = sig
    type ins_cert_t
    type t
    val se : t -> Level.basic_level
    val typing : t -> ins_cert_t
    val create : Level.basic_level -> ins_cert_t -> t
    val to_string : t -> string
    val to_byteString : t -> string
    val empty : t
  end;;
module JVM_ins_typing : Ins_typing_type with type ins_cert_t = StackType.t;;
module DEX_ins_typing : Ins_typing_type with type ins_cert_t = RegistersType.t;;

module DEX_typing_op : sig
    val add_missing_registers : Level.ext_level -> int -> DEX_ins_typing.t ->
                                DEX_ins_typing.t
  end;;

module MethodPolicy : sig
    type t
    val ka : t -> Level.ext_level list;;
    val ka_at : int -> t -> Level.ext_level;;
    val kh : t -> Level.basic_level;;
    (* kr at least contains 1 sec_level for normal return value *)
    val kr : t -> Level.ext_level list;;
    val kr_at : int -> t -> Level.ext_level;;
    val kr_n : t -> int;;
    val create_policy : int -> int list list -> (* ka *)
                        int -> (* kh *)
                        int -> int list list -> (* kr *)
                        Level_pool.t -> t;;
    val empty : t;;
    val to_string : t -> string;;
    val to_byteString : t -> string;;
  end;;

module Region : sig type t
                    val find : int32 -> t -> int list
                    val add : int32 -> int list -> t -> t
                    val empty : t
                    val iter : (int32 -> int list -> unit) -> t -> unit
                    val fold : (int32 -> int list -> 'b -> 'b) -> t -> 'b -> 'b
                    val to_string : t -> string
                    val to_byteString : int list -> string
                end;;

module Junction : sig type t
                      val find : int32 -> t -> int option
                      val add : int32 -> int option -> t -> t
                      val empty : t
                      val fold : (int32 -> int option -> 'b -> 'b) -> t -> 'b -> 'b
                      val to_string : t -> string
                      val to_byteString : int option -> string
                  end;;

module type BytecodeMethod_cert_type = sig
    type ins_typing_t
    type t
    val typing_at : int -> t -> ins_typing_t
    val empty : t
    val to_string : t -> string
    val to_byteString : t -> string
  end
module JVM_BytecodeMethod_cert : BytecodeMethod_cert_type with
         type ins_typing_t = JVM_ins_typing.t and type t = JVM_ins_typing.t Map_type.Int32Map.t;;
module DEX_BytecodeMethod_cert : BytecodeMethod_cert_type with
         type ins_typing_t = DEX_ins_typing.t and type t = DEX_ins_typing.t Map_type.Int32Map.t;;

module type Method_cert_type = sig
    type bytecodeMethod_cert_t;;
    module Item : sig type t;; val policy : t -> MethodPolicy.t;;
                      val create : Level.basic_level -> MethodPolicy.t -> bytecodeMethod_cert_t -> t;;
                      val bytecodeMethod_cert : t -> bytecodeMethod_cert_t;;
                  end;;
    type t;;
    val class_name : t -> string;;
    val name : t -> string;;
    val descriptor : t -> string;;
    val policy : int -> t -> MethodPolicy.t;;
    val bytecodeMethod_cert : int -> t -> bytecodeMethod_cert_t;;
    val create : string -> string -> string -> Region.t -> Junction.t ->
                 Item.t Map_type.Int32Map.t -> t;;
    val empty : t;;
    val to_string : t -> string;;
    val to_byteString : t -> string;;
    val reg_map : t -> Region.t
    val jun_map : t -> Junction.t
    val reg_at : int -> t -> int list
    val jun_at : int -> t -> int option
    val read_wo_typing : in_channel -> Level_pool.t -> t;;
  end
module JVM_Method_cert : Method_cert_type with
         type bytecodeMethod_cert_t = JVM_BytecodeMethod_cert.t;;
module DEX_Method_cert : Method_cert_type with
         type bytecodeMethod_cert_t = DEX_BytecodeMethod_cert.t;;

module FieldPolicy : sig
    module Item : sig
        type t;;
        val class_name : t -> string;;
        val name : t -> string;;
        val sec_level : t -> Level.ext_level;;
      end;;

    type t
    val ft : string -> string -> t -> Level.ext_level;;
    val to_byteString : t -> string;;
    val read : in_channel -> Level_pool.t -> t;;
  end;;

module type Cert_type = sig
    type method_cert_t;;
    type t;;
    val lvl_pool : t -> Level_pool.t;;
    val levels_rel : t -> Levels_relationship.t;;
    val method_cert : string -> string -> string -> t -> method_cert_t;;
    val field_policy : t -> FieldPolicy.t
    val default_level : t -> Level.basic_level;;
    val default_top_level : t -> Level.basic_level;;
    val create_cert : Level_pool.t -> Levels_relationship.t ->
                      ((method_cert_t Map_type.StringMap.t) Map_type.StringMap.t) Map_type.StringMap.t ->
                      FieldPolicy.t -> Level.basic_level -> Level.basic_level -> t;;
    val to_byteString : t -> string;;
  end
module JVM_Cert : Cert_type with type method_cert_t = JVM_Method_cert.t;;
module DEX_Cert : Cert_type with type method_cert_t = DEX_Method_cert.t;;

val base_rt_rec : int -> MethodPolicy.t -> RegistersType.t
val translate_st_rt : StackType.t -> int -> MethodPolicy.t -> Level.ext_level option -> RegistersType.t
