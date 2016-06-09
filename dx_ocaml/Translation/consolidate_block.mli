val consolidateBlock : int -> Compiler_meta.dBlockMap -> Compiler_meta.Block.t -> 
  Handler.t list -> (int list) Map_type.Int32Map.t -> 
  ((Instructions.DEX.DEX_Instruction.t list) * (Instructions.DEX.DEX_Instruction.t Map_type.Int32Map.t) * 
	Compiler_meta.dBlockMap * (Handler.t list) * (int list) Map_type.Int32Map.t * int Map_type.Int32Map.t)
