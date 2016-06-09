val parse : Instructions.JVM.JVM_Instruction.t list -> int -> int -> Certificate.MethodPolicy.t ->
            Compiler_meta.dBlockMap -> Map_type.sbMap -> Handler.t list ->
            Compiler_meta.dBlockMap * Map_type.bpMap * Map_type.tsMap * Compiler_meta.Block.t
