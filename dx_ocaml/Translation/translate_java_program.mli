val parse : Instructions.JVM.JVM_Instruction.t list -> Compiler_meta.dBlockMap -> Map_type.bpMap ->
            Map_type.sbMap -> Map_type.tsMap -> Compiler_meta.Block.t -> int -> Certificate.MethodPolicy.t ->
            Map_type.valTypeMap ->
            (Compiler_meta.dBlockMap * int)
