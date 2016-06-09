open Instructions;;
open Instructions.JVM;;
open Map_type;;
open Certificate;;

let rec assign_type_rec (jInsnList : JVM_Instruction.t list) (jInsnMap : JVM_Instruction.t Int32Map.t) 
  (seMap : Level.basic_level Int32Map.t) (typeMap : StackType.t Int32Map.t) =
	match jInsnList with
	| [] -> ([], Int32Map.empty)
	| h :: t -> let (resList, resMap) = assign_type_rec (t) (jInsnMap) (seMap) (typeMap) in
	  let label32 = JVM_Instruction.label32 h in
	  let typing = JVM_ins_typing.create (Int32Map.find label32 seMap) (Int32Map.find label32 typeMap) in
	  let newIns = JVM_Instruction.update_typing typing h in
		let newResMap = Int32Map.add (label32) (newIns) (resMap) in
		(newIns :: resList, newResMap)

let assign_type (jInsnList : JVM_Instruction.t list) (jInsnMap : JVM_Instruction.t Int32Map.t) 
  (seMap : Level.basic_level Int32Map.t) (typeMap : StackType.t Int32Map.t) =
	assign_type_rec (jInsnList) (jInsnMap) (seMap) (typeMap);;