val parse : Instructions.JVM.JVM_Instruction.t list -> int -> Handler.t list ->
  (Compiler_meta.dBlockMap * Map_type.sbMap)
