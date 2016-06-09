module Block : sig
    type t
    val empty : t
    val retLabel : int
    val retLabel32 : int32

    val add_parent : int -> t -> t;;
    val add_succ : int -> t -> t;;
    val update_pSucc : int -> t -> t;;
    val update_handlers : Handler.t list -> t -> t;;
    val add_insn : (int*Instructions.DEX.DEX_Instruction.t) -> t -> t;;
    val add_insns : (int*Instructions.DEX.DEX_Instruction.t) list -> t -> t;;
    val update_se : Certificate.Level.basic_level -> t -> t;;
    val update_rt0 : Certificate.RegistersType.t -> t -> t;;
    val update_typing : Certificate.Level.basic_level -> Certificate.RegistersType.t -> t -> t;;
    val update_order : int -> t -> t;;
    val update_traced : bool -> t -> t;;
    val update_scopeAddress : (int * int) -> t -> t;;

    val parents : t -> int list;;
    val succs : t -> int list;;
    val insnList : t -> (int*Instructions.DEX.DEX_Instruction.t) list;;
    val lastInsn : t -> (int*Instructions.DEX.DEX_Instruction.t) option;;
    val psucc : t -> int option;;
    val catches : t -> Handler.t list;;
    val rt0 : t -> Certificate.RegistersType.t;;
    val se : t -> Certificate.Level.basic_level;;
    val order : t -> int option;;
    val traced : t -> bool;;
    val scopeAddress : t -> (int*int);;
    val to_string : t -> string
  end;;

type dBlockMap = Block.t Map_type.Int32Map.t;;
