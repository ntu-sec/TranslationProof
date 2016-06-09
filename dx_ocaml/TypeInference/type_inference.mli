val infer_type : JVM_Class.t -> JVM_Method.t -> Certificate.JVM_Cert.t ->
                 int -> Certificate.Level.basic_level ->
                 (Instructions.JVM.JVM_Instruction.t list * Instructions.JVM.JVM_Instruction.t Map_type.Int32Map.t)
