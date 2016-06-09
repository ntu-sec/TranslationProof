val assign_type : Instructions.JVM.JVM_Instruction.t list -> Instructions.JVM.JVM_Instruction.t Map_type.Int32Map.t
                  -> Certificate.Level.basic_level Map_type.Int32Map.t -> Certificate.StackType.t Map_type.Int32Map.t ->
                  (Instructions.JVM.JVM_Instruction.t list * Instructions.JVM.JVM_Instruction.t Map_type.Int32Map.t)
