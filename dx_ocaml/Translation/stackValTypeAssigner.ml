open Flow_tracer;;
open Map_type;;
open Instructions;;
open Instructions.JVM;;

let rec assignValType_rec (orderList:int32 list) (jInsnMap:JVM_Instruction.t Int32Map.t)
                          (m:valTypeMap) =
  match orderList with
  | [] -> m
  | h :: t ->
     let currentStack = Int32Map.find h m in
     let ins = Int32Map.find h jInsnMap in
     let newM = ( match JVM_Instruction.code ins with
                  | JVM.Nop -> let label = JVM_Instruction.label ins in
                               let size = JVM_Instruction.size ins in
                               Int32Map.add (Int32.of_int (label + size)) (currentStack) (m)
                  | JVM.New
                  | JVM.Push -> let label = JVM_Instruction.label ins in
                                let size = JVM_Instruction.size ins in
                                Int32Map.add (Int32.of_int (label + size)) (true::currentStack) (m)
                  | JVM.Pop -> let label = JVM_Instruction.label ins in
                               let size = JVM_Instruction.size ins in
                               Int32Map.add (Int32.of_int (label + size)) (List.tl currentStack) (m)
                  | JVM.Load -> let label = JVM_Instruction.label ins in
                                let size = JVM_Instruction.size ins in
                                Int32Map.add (Int32.of_int (label + size)) (false::currentStack) (m)
                  | JVM.Store -> let label = JVM_Instruction.label ins in
                                 let size = JVM_Instruction.size ins in
                                 Int32Map.add (Int32.of_int (label + size)) (List.tl currentStack) (m)
                  | JVM.Swap -> let label = JVM_Instruction.label ins in
                                let size = JVM_Instruction.size ins in
                                let newStack = (match currentStack with | k1 :: k2 :: t -> k2::k1::t
                                                                   | _ -> raise (Invalid_argument "[stackValType] attempted to swap a stack with less than two elements"))
                                in
                                Int32Map.add (Int32.of_int (label + size)) (newStack) (m)
                  | JVM.Goto -> let args = JVM.getGotoArgs ins in
                                Int32Map.add (Int32.of_int args) (currentStack) (m)
                  | JVM.If -> let args = JVM.getIfArgs ins in
                              let label = JVM_Instruction.label ins in
                              let size = JVM_Instruction.size ins in
                              let offset = JVM.IfArgs.offset args in
                              let nVal = (match JVM.IfArgs.kind args with
                                          | Eq | Ne | Gt | Ge | Lt | Le -> 2
                                          | Eqz | Nez | Gtz | Gez | Ltz | Lez -> 1) in
                              let (_, newStack) = Utility.pop_n currentStack nVal in
                              let newM = Int32Map.add (Int32.of_int (label + size)) (newStack) (m) in
                              Int32Map.add (Int32.of_int (label + offset)) (newStack) (newM)
                  | JVM.Return -> m
                  | JVM.Binop ->
                     let label = JVM_Instruction.label ins in
                     let size = JVM_Instruction.size ins in
                     let newStack = (match currentStack with | k1 :: k2 :: t -> (k1||k2)::t
                                                        | _ -> raise (Invalid_argument "[stackValType] attempted to do a binop on a stack with less than two elements"))
                     in
                     Int32Map.add (Int32.of_int (label + size)) (newStack) (m)
                  | JVM.IInc -> let label = JVM_Instruction.label ins in
                                let size = JVM_Instruction.size ins in
                                Int32Map.add (Int32.of_int (label + size)) (currentStack) (m)
                  | JVM.Dup -> let label = JVM_Instruction.label ins in
                               let size = JVM_Instruction.size ins in
                               let newStack = (match currentStack with |k1 :: t -> k1 :: k1 :: t
                                                                  | _ -> raise (Invalid_argument "[stackValType] attempted to duplicate an empty stack"))
                               in
                               Int32Map.add (Int32.of_int (label + size)) (newStack) (m)
                  | JVM.Invoke m' -> let label = JVM_Instruction.label ins in
                                     let size = JVM_Instruction.size ins in
                                     let newStack =
                                       (let args = JVM.getFMIArgs ins in
                                        let method_desc = JVM.FMIArgs.fmi_type args in
                                        let nArgs = Utility.countArgs_from_desc method_desc in
                                        let nRet = Utility.countRet_from_desc method_desc in
                                        let (_, newStack) = Utility.pop_n currentStack (nArgs + 1) in
                                        let newStack = match nRet with
                                          | 0 -> newStack
                                          | 1 -> false :: newStack
                                          | _ -> raise (Invalid_argument "[stackValType] not a valid number of return values") in
                                        newStack)
                                     in
                                     Int32Map.add (Int32.of_int (label + size)) (newStack) (m)
                  | JVM.Getfield -> (let label = JVM_Instruction.label ins in
                                     let size = JVM_Instruction.size ins in
                                     Int32Map.add (Int32.of_int (label + size)) (false::(List.tl currentStack)) (m)
                                    )
                  | JVM.Putfield -> (let label = JVM_Instruction.label ins in
                                     let size = JVM_Instruction.size ins in
                                     let (_, newStack) = Utility.pop_n currentStack 2 in
                                     Int32Map.add (Int32.of_int (label + size)) (newStack) (m)
                                    )
                  | _ -> raise (Invalid_argument "[stackValType] not yet implemented")
                ) in
     assignValType_rec (t) (jInsnMap) (newM)
;;

let assignValType (jInsnList:JVM_Instruction.t list) (jInsnMap:JVM_Instruction.t Int32Map.t) =
  let orderList = Flow_tracer.trace (jInsnList) (jInsnMap) in
  let initialMap = Int32Map.add (0l) ([]) Int32Map.empty (* the assumption that a program will start with label 0 *) in
  assignValType_rec (orderList) (jInsnMap) (initialMap);;
