module CodeAttribute : sig	
	type t;;
  val insnList : t -> Instructions.JVM.JVM_Instruction.t list;;
  val insnMap : t -> Instructions.JVM.JVM_Instruction.t Map_type.Int32Map.t;;
  val maxLabel : t -> int;;
  val maxStack : t -> int;;
  val handlers : t -> Handler.t list;;
  val localN : t -> int;;
	
	val read : Map_type.constantMap -> in_channel -> t
end;;

type methodAttribute;;
val read_methodAttribute : Map_type.constantMap -> in_channel -> (string * methodAttribute);;

val get_CodeAttribute : methodAttribute -> CodeAttribute.t;;
