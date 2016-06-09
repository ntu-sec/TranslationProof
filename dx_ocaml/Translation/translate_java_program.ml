open Compiler_meta;;
open Instructions;;
open Instructions.JVM;;
open Instructions.DEX;;
open Map_type;;
open Certificate;;

let min_register = 0;;

(* basically just make a register ordered from start with n number of elements *)
let rec makeRegList (start: int) (count: int) =
  match count <= 0 with
    true -> []
  | false -> start :: makeRegList (start + 1) (count - 1)
;;

let parseInsn (insn: JVM_Instruction.t) (m: dBlockMap) (bp: bpMap) (sb: sbMap) (ts: tsMap)
              (ret: Block.t) (localN: int) (lvt: MethodPolicy.t) (constStackMap:valTypeMap) =
  let currentTS = Int32Map.find (JVM_Instruction.label32 insn) ts in
  let typeInfo = JVM_Instruction.typeInfo insn in
  let current_se = JVM_ins_typing.se typeInfo in
  let translated_rt = translate_st_rt (JVM_ins_typing.typing typeInfo) (localN) (lvt) (None) in
  let label32 = JVM_Instruction.label32 insn in
  let isStartBlock = Int32Map.find label32 sb in
  let blockIndex = Int32Map.find label32 bp in
  let cb = let cb_temp = Int32Map.find (blockIndex) m in
           match isStartBlock with
           | true -> Block.update_typing (current_se) (translated_rt) (cb_temp)
           | false -> cb_temp in
  match JVM_Instruction.code insn with
  | JVM.Nop -> let size = match JVM_Instruction.size insn with | 1 -> 1 | 3 -> 2
                                                          | _ -> raise (Invalid_argument "[translate_java_program] invalid value of size for Nop")
               in
               (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                           DEX.createNopInsn (-1) (size) (current_se) (translated_rt)) cb) (m), min_register)
  | Pop -> (m, min_register)
  | Swap ->
     let rt1 = RT_op.add currentTS (RT_op.get (currentTS - 2) translated_rt) translated_rt in
     let rt2 = RT_op.add (currentTS + 1)     (RT_op.get (currentTS - 1) rt1) rt1 in
     let rt3 = RT_op.add (currentTS - 2)     (RT_op.get (currentTS + 1) rt2) rt2 in
     let newList =  [
         (JVM_Instruction.label insn, DEX.createMoveInsn (-1) (1) (currentTS) (currentTS - 2) (current_se) (translated_rt));
         (JVM_Instruction.label insn, DEX.createMoveInsn (-1) (1) (currentTS + 1) (currentTS - 1) (current_se) (rt1));
         (JVM_Instruction.label insn, DEX.createMoveInsn (-1) (1) (currentTS - 2) (currentTS + 1) (current_se) (rt2));
         (JVM_Instruction.label insn, DEX.createMoveInsn (-1) (1) (currentTS - 1) (currentTS) (current_se) (rt3))]
     in
     (Int32Map.add (blockIndex) (Block.add_insns (newList) (cb)) (m), (currentTS + 1))
  | Dup ->
     let rt1 = RT_op.add (currentTS + 1) (RT_op.get (currentTS - 1) translated_rt) translated_rt in
     let rt2 = RT_op.add (currentTS - 1)     (RT_op.get (currentTS + 1) rt1) rt1 in
     let newList = [
         (JVM_Instruction.label insn, DEX.createMoveInsn (-1) (1) (currentTS + 1) (currentTS - 1) (current_se) (translated_rt));
         (JVM_Instruction.label insn, DEX.createMoveInsn (-1) (1) (currentTS - 1) (currentTS + 1) (current_se) (rt1));
         (JVM_Instruction.label insn, DEX.createMoveInsn (-1) (1) (currentTS - 2) (currentTS + 1) (current_se) (rt2))]
     in
     (Int32Map.add (blockIndex) (Block.add_insns (newList) (cb)) (m), (currentTS + 1))
  | JVM.Throw ->
     (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                 DEX.createThrowInsn (-1) (1) (currentTS -1) (current_se) (translated_rt)) cb) (m), (currentTS - 1))
  | JVM.Push -> let arg = JVM.getArgsArgs insn in
                let value = ArgsArgs.arg_at 0 arg in
                (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                            DEX.createConstInsn (-1) (1) (currentTS) (value) (current_se) (translated_rt)) cb) (m), currentTS)
  | JVM.Load -> let arg = JVM.getArgsArgs insn in
                let value = ArgsArgs.arg_at 0 arg in
                (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                            DEX.createMoveInsn (-1) (1) (currentTS) (value) (current_se) (translated_rt)) cb) (m), currentTS)
  | JVM.Store -> let arg = JVM.getArgsArgs insn in
                 let value = ArgsArgs.arg_at 0 arg in
                 (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                             DEX.createMoveInsn (-1) (1) (value) (currentTS - 1) (current_se) (translated_rt)) cb) (m), (currentTS-1))
  | JVM.IInc -> let arg = JVM.getArgsArgs insn in
                let targetReg = ArgsArgs.arg_at 0 arg in
                let value = ArgsArgs.arg_at 1 arg in
                (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                            DEX.createBinopConstInsn (-1) (1) (DEX.Add) (targetReg) (targetReg) (value) (current_se)
                                                                                     (translated_rt)) cb) (m), targetReg)
  | JVM.Binop -> (let arg = JVM.getBinopArgs insn in
                  let binopKind = map_binopKind (JVM.BinopArgs.op arg) in
                  let constStack = Int32Map.find label32 constStackMap in
                  let ([c1; c2], _) = Utility.pop_n constStack 2 in
                  match c1 || c2 with
                  | false ->
                     (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                                 DEX.createBinopInsn (-1) (1) (binopKind) (currentTS-2) (currentTS-2) (currentTS-1)
                                                                                     (current_se) (translated_rt)) cb) (m), (currentTS-1))
                  | true ->
                     let (r, c) = match c2 with
                       | true -> (currentTS-1, currentTS-2) (* at the moment we don't care of the constant value *)
                       | false -> (currentTS-2, currentTS-1) in
                     (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                                 DEX.createBinopConstInsn (-1) (1) (binopKind) (currentTS-2) (r) (c)
                                                                                          (current_se) (translated_rt)) cb) (m), (currentTS-1))) (* TODO: recheck whether this is the correct maximum register *)
  | JVM.If -> (let arg = JVM.getIfArgs insn in
               let target = ((JVM_Instruction.label insn) + (JVM.IfArgs.offset arg)) in
               let jvmIfKind = JVM.IfArgs.kind arg in
               let dexIfKind = map_ifKind (jvmIfKind) in
               match jvmIfKind with
               | Eq | Ne | Gt | Ge | Lt | Le -> (Int32Map.add (blockIndex)
                                                              (Block.add_insn (JVM_Instruction.label insn, DEX.createIfInsn (-1) (2) (dexIfKind)
                                                                                                                            (currentTS - 2) (currentTS - 1) (target) (current_se) (translated_rt)) cb) m, (currentTS-1))
               | Eqz | Nez | Gtz | Gez | Ltz | Lez -> (Int32Map.add (blockIndex)
                                                                    (Block.add_insn (JVM_Instruction.label insn, DEX.createIfzInsn (-1) (2) (dexIfKind)
                                                                                                                                   (currentTS - 1) (target) (current_se) (translated_rt)) cb) (m), (currentTS-1))
              )
  | JVM.Goto -> (match isStartBlock with
                 | true -> (Int32Map.add (blockIndex) (cb) (m), min_register)
                 | false -> (m, min_register))
  | JVM.Return -> (let arg = JVM.getReturnArgs insn in
                   match JVM.ReturnArgs.retType arg with
                     JVM.Void -> (m, min_register)
                   | JVM.Int -> (Int32Map.add (blockIndex) (Block.add_insn
                                                              (JVM_Instruction.label insn, DEX.createMoveInsn (-1) (1) (0)
                                                                                                              (currentTS - 1) (current_se) (RT_op.update_flag (1) (translated_rt))) cb) (m), (currentTS-1))
                  )
  | JVM.New -> let arg = JVM.getClassArgs insn in
               (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                           DEX.createNewInsn (-1) (1) (currentTS) (JVM.ClassArgs.name arg) (current_se)
                                                                             (translated_rt)) cb) m, currentTS)
  | JVM.Getfield -> let arg = JVM.getFMIArgs insn in
                    let fieldName = (JVM.FMIArgs.class_name arg) ^ "." ^ (JVM.FMIArgs.name arg) in
                    (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                                DEX.createGetfieldInsn (-1) (1) (currentTS -1) (currentTS - 1) (fieldName) (current_se) (translated_rt)) cb) m, (currentTS-1))
  | JVM.Putfield -> let arg = JVM.getFMIArgs insn in
                    let fieldName = (JVM.FMIArgs.class_name arg) ^ "." ^ (JVM.FMIArgs.name arg) in
                    (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                                DEX.createPutfieldInsn (-1) (1) (currentTS - 1) (currentTS - 2) (fieldName) (current_se) (translated_rt)) cb) m, (currentTS-1))
  | JVM.Invoke t -> let arg = JVM.getFMIArgs insn in
                    let methodName = (JVM.FMIArgs.class_name arg) ^ "." ^ (JVM.FMIArgs.name arg) in
                    let nArgs = (let close = String.index (JVM.FMIArgs.fmi_type arg) (')') in
                                 let str = String.sub (JVM.FMIArgs.fmi_type arg) (1) (close - 1) in
                                 match (String.length (str)) = 0 with
                                   false -> (Utility.countArgs (str) (String.length (str)) (0))
                                 | true -> (0)) in
                    let regList = makeRegList (currentTS - nArgs - 1) (nArgs + 1) in
                    (Int32Map.add (blockIndex) (Block.add_insn (JVM_Instruction.label insn,
                                                                DEX.createInvokeInsn (-1) (1) (nArgs) (regList) (methodName) (current_se)
                                                                                     (translated_rt)) cb) (m), (currentTS-1))
;;

let rec parseInsns (jProg: JVM_Instruction.t list) (m: dBlockMap) (bp: bpMap) (sb: sbMap)
                   (ts: tsMap) (ret: Block.t) (localN: int) (lvt: MethodPolicy.t) (constStackMap: valTypeMap) =
  match jProg with
    [] -> (m, min_register)
  | insn :: t ->
     let (newM, max_registers) = parseInsn (insn) (m) (bp) (sb) (ts) (ret) (localN) (lvt) (constStackMap) in
     let (newM, max_registers') = parseInsns (t) (newM) (bp) (sb) (ts) (ret) (localN) (lvt) (constStackMap) in
     (newM, match max_registers > max_registers' with | true -> max_registers | false -> max_registers');;

let parse (jProg: JVM_Instruction.t list) (m: dBlockMap) (bp: bpMap) (sb : sbMap) (ts: tsMap)
          (ret: Block.t) (localN: int) (lvt: MethodPolicy.t) (constStackMap: valTypeMap) =
  parseInsns (jProg) (m) (bp) (sb) (ts) (ret) (localN) (lvt) (constStackMap) ;;
