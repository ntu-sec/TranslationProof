module type Type = sig type t val to_string : t -> string end

module type Instruction_type = sig
    type insnType
    type argsType
    type certType
    type t
    val label : t -> int;;
    val label32 : t -> int32;;
    val size : t -> int;;
    val code : t -> insnType;;
    val args : t -> argsType;;
    val typeInfo : t -> certType;;
    val create : int -> int -> insnType -> argsType -> certType -> t
    val to_string : t -> string
    val update_label : int -> t -> t;;
    val update_size : int -> t -> t;;
    val update_label_size : int -> int -> t -> t;;
    val update_typing : certType -> t -> t;;
  end;;

module JVM : sig
    type valType = Int | Void;;
    type insnType = Nop| Push | Pop | Load | Store | Swap | Goto | If | Return | Binop | IInc
                    | New | Getfield | Putfield | Dup
                    | Invoke of valType (* for now we collapse all the kinds of invoke *)
                    | Throw;;
    type binopKind = Add | Sub | Div | Mul | Mod | And | Or | Xor | Shl | Shr | Ushr;;
    type ifKind = Eq | Ne | Gt | Ge | Lt | Le |
                  Eqz | Nez | Gtz | Gez | Ltz | Lez;;
    val string_of_valType : valType -> string
    val string_of_binop : binopKind -> string
    val string_of_ifKind : ifKind -> string
    val string_of_insnType : insnType -> string
    val char_to_valType : string -> valType

    module PlainArgs : sig include Type
                           val create : unit -> t
                       end;;

    module ArgsArgs : sig include Type with type t = int list
                          val create : int list -> t
                          val arg_at : int -> t -> int
                      end;;

    module BinopArgs : sig include Type with type t = binopKind
                           val create : binopKind -> t
                           val op : t -> binopKind
                       end

    module IfArgs : sig
        include Type
        val kind : t -> ifKind
        val offset : t -> int
        val create : ifKind -> int -> t
      end

    module GotoArgs : sig include Type with type t = int
                          val create : int -> t
                      end

    module ReturnArgs : sig include Type with type t = valType
                            val create : valType -> t
                            val retType : t -> valType
                        end

    module ClassArgs : sig
        include Type
        val index : t -> int
        val name : t -> string
        val create : int -> string -> t
      end

    module FMIArgs : sig include Type
                         val index : t -> int
                         val class_name : t -> string
                         val name : t -> string
                         val fmi_type : t -> string
                         val create : int -> string -> string -> string -> t
                     end

    type insArgsType = PlainArgsType of PlainArgs.t | ArgsArgsType of ArgsArgs.t |
                       BinopArgsType of BinopArgs.t | IfArgsType of IfArgs.t | GotoArgsType of GotoArgs.t |
                       ReturnArgsType of ReturnArgs.t | ClassArgsType of ClassArgs.t |
                       FMIArgsType of FMIArgs.t;;

    module ArgsPrint : Type with type t = insArgsType;;
    module JVM_InsnType : Type with type t = insnType;;

    module JVM_Instruction : Instruction_type
           with type insnType = JVM_InsnType.t and
                type certType = Certificate.JVM_ins_typing.t and
                type argsType = ArgsPrint.t;;

    val getPlainArgs : JVM_Instruction.t -> PlainArgs.t;;
    val getArgsArgs : JVM_Instruction.t -> ArgsArgs.t;;
    val getBinopArgs : JVM_Instruction.t -> BinopArgs.t;;
    val getIfArgs : JVM_Instruction.t -> IfArgs.t;;
    val getGotoArgs : JVM_Instruction.t -> GotoArgs.t;;
    val getReturnArgs : JVM_Instruction.t -> ReturnArgs.t;;
    val getClassArgs : JVM_Instruction.t -> ClassArgs.t;;
    val getFMIArgs : JVM_Instruction.t -> FMIArgs.t;;

    val createNopInsn : int -> int -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createPushInsn : int -> int -> int -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createBinopInsn : int -> int -> string -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createLoadInsn : int -> int -> int -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createStoreInsn : int -> int -> int -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createPopInsn : int -> int -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createSwapInsn : int -> int -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createIfInsn : int -> int -> int -> string -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createGotoInsn : int -> int -> int -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createReturnInsn : int -> int -> string -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createNewInsn : int -> int -> int -> string -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createFieldInsn : int -> int -> string -> int -> string -> string -> string -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createInvokeInsn : int -> int -> string -> int -> string -> string -> string -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createThrowInsn : int -> int -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createIincInsn : int -> int -> int -> int -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
    val createDupInsn : int -> int -> Certificate.JVM_ins_typing.t -> JVM_Instruction.t;;
  end;;

module DEX : sig
    type valType = Int | Void;;
    type fieldOp = Get | Put ;;
    type insnType = Nop | Const | Move | Goto | If | Ifz | Return | Binop | BinopConst
                    | New | Field of fieldOp | Invoke | MoveResult | MoveException
                    | Throw;;
    type binopKind = Add | Sub | Div | Mul | Mod | And | Or | Xor | Shl | Shr | Ushr;;
    type insArgsType
    type ifKind
    val string_of_valType : valType -> string
    val string_of_binop : binopKind -> string
    val string_of_ifKind : ifKind -> string
    val string_of_insnType : insnType -> string

    module NopArgs : sig include Type
                         val create : unit -> t;;
                         val to_string : t -> string;;
                     end;;

    module ConstArgs : sig include Type
                           val create : int -> int -> t;;
                           val reg : t -> int;;
                           val value : t -> int;;
                           val to_string : t -> string
                       end;;

    module ReturnArgs : sig include Type
                            val create : valType -> int -> t
                            val retType : t -> valType
                            val retReg : t -> int
                        end;;

    module MoveArgs : sig include Type
                          val create : int -> int -> t
                          val target : t -> int
                          val source : t -> int
                      end;;

    module BinopArgs : sig include Type
                           val create : binopKind -> int -> int -> int -> t
                           val target : t -> int
                           val source1 : t -> int
                           val source2 : t -> int
                       end;;

    module BinopConstArgs : sig include Type
                                val create : binopKind -> int -> int -> int -> t
                                val target : t -> int
                                val source : t -> int
                                val const : t -> int
                            end;;

    val get_OppositeIf : ifKind -> ifKind;;

    module IfArgs : sig
        include Type
        val kind : t -> ifKind
        val offset : t -> int
        val create : ifKind -> int -> int -> int -> t
      end;;

    module IfzArgs : sig
        include Type
        val kind : t -> ifKind
        val offset : t -> int
        val create : ifKind -> int -> int -> t
        val register : t -> int
      end;;

    module GotoArgs : sig include Type with type t = int
                          val create : int -> t
                          val offset : t -> int
                          val to_string : t -> string
                      end;;

    module TargetArgs : sig include Type
                            val create : int -> t
                            val to_string : t -> string
                        end;;

    module ClassArgs : sig
        include Type
        val target : t -> int
        val name : t -> string
        val create : int -> string -> t
      end;;

    module FieldArgs : sig include Type
                           val op : t -> fieldOp
                           val valReg : t -> int
                           val objectReg : t -> int
                           val fieldName : t -> string
                           val create : fieldOp -> int -> int -> string -> t
                       end;;

    module MethodArgs : sig include Type
                            val argNum : t -> int
                            val params : t -> int list
                            val name : t -> string
                            val create : int -> int list -> string -> t
                        end;;

    module ArgsPrint : Type with type t = insArgsType;;
    module DEX_InsnType : Type with type t = insnType;;

    module DEX_Instruction : Instruction_type
           with type insnType = DEX_InsnType.t and
                type certType = Certificate.DEX_ins_typing.t and
                type argsType = ArgsPrint.t;;

    val createNopInsn : int -> int -> Certificate.Level.basic_level ->
                        Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createConstInsn : int -> int -> int -> int -> Certificate.Level.basic_level ->
                          Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createMoveInsn : int -> int -> int -> int -> Certificate.Level.basic_level ->
                         Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createReturnInsn : int -> int -> valType -> Certificate.Level.basic_level ->
                           Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createMoveExceptionInsn : int -> int -> int -> Certificate.Level.basic_level ->
                                  Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createMoveResultInsn : int -> int -> int -> Certificate.Level.basic_level ->
                               Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createThrowInsn : int -> int -> int -> Certificate.Level.basic_level ->
                          Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createBinopInsn : int -> int -> binopKind -> int -> int -> int -> Certificate.Level.basic_level ->
                          Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createBinopConstInsn : int -> int -> binopKind -> int -> int -> int -> Certificate.Level.basic_level ->
                               Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createIfInsn : int -> int -> ifKind -> int -> int -> int -> Certificate.Level.basic_level ->
                       Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createIfzInsn : int -> int -> ifKind -> int -> int -> Certificate.Level.basic_level ->
                        Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createGotoInsn : int -> int -> int -> Certificate.Level.basic_level ->
                         Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createNewInsn : int -> int -> int -> string -> Certificate.Level.basic_level ->
                        Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createGetfieldInsn : int -> int -> int -> int -> string -> Certificate.Level.basic_level ->
                             Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createPutfieldInsn : int -> int -> int -> int -> string -> Certificate.Level.basic_level ->
                             Certificate.RegistersType.t -> DEX_Instruction.t;;
    val createInvokeInsn : int -> int -> int -> int list -> string -> Certificate.Level.basic_level ->
                           Certificate.RegistersType.t -> DEX_Instruction.t;;

    val getConstArgs : DEX_Instruction.t -> ConstArgs.t;;
    val getMoveArgs : DEX_Instruction.t -> MoveArgs.t;;
    val getBinopArgs : DEX_Instruction.t -> BinopArgs.t;;
    val getBinopConstArgs : DEX_Instruction.t -> BinopConstArgs.t;;

    val getIfArgs : DEX_Instruction.t -> IfArgs.t;;
    val getIfzArgs : DEX_Instruction.t -> IfzArgs.t;;
    val getGotoArgs : DEX_Instruction.t -> GotoArgs.t;;

    val opposite_if_insn : DEX_Instruction.t -> int -> DEX_Instruction.t;;
    val opposite_ifz_insn : DEX_Instruction.t -> int -> DEX_Instruction.t;;

    val fixGotoTarget : DEX_Instruction.t -> int -> DEX_Instruction.t;;
    val fixIfTarget : DEX_Instruction.t -> int -> DEX_Instruction.t;;
    val fixIfzTarget : DEX_Instruction.t -> int -> DEX_Instruction.t;;
  end;;

val map_ifKind : JVM.ifKind -> DEX.ifKind;;
val map_binopKind : JVM.binopKind -> DEX.binopKind;;
