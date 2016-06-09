(* Module to do a simple task to indicate which instruction
  is a starting block, will return SBMap *)

open Compiler_meta;;
open Map_type;;
open Instructions.JVM;;
open Instructions;;
open Handler;;

let next_new_block (label:int) (size:int) (m:dBlockMap) (sb:sbMap) =
  (Int32Map.add (Int32.of_int (label+size)) (Block.empty) m,
   Int32Map.add (Int32.of_int (label+size)) (true) sb)
;;

let rec set_handlers (m:dBlockMap) (sb:sbMap) (handlers : Handler.t list) (maxLabel:int) =
  match handlers with
    [] -> (m, sb)
  | h :: t ->
     let newM  =
       Int32Map.add (Handler.startPC32 h) (Block.empty)
                    (Int32Map.add (Handler.endPC32 h) (Block.empty)
                                  (Int32Map.add (Handler.handlerPC32 h) (Block.empty)
                                                (* This last bit corresponds to the actual dx implementation, where there is additional
            block containing only move-exception in the address of handler pc + maxLabel *)
                                                (Int32Map.add (Int32.of_int (Handler.handlerPC h + maxLabel)) (Block.empty) (m))))
     in
     let newSB =
       Int32Map.add (Handler.startPC32 h) (true)
                    (Int32Map.add (Handler.endPC32 h) (true)
                                  (Int32Map.add (Handler.handlerPC32 h) (true)
                                                (Int32Map.add (Int32.of_int (Handler.handlerPC h + maxLabel)) (true) (sb))))
     in
     set_handlers (newM) (newSB) (t) (maxLabel)
;;

(* For now we assume that all instructions are live, therefore we don't care whether a handler
  might actually not be used at all and include it in the output anyway *)
let parseInsn (insn:JVM_Instruction.t) (maxLabel:int) (m:dBlockMap) (sb:sbMap)
              (catches : Handler.t list) =
  match JVM_Instruction.code insn with
    (* After a return instruction it must be a new block. Although
      in general case a "sane" java program will have a branching
      instruction to this block, there's nothing wrong with additional
      precaution *)
    Return ->
    let label = (JVM_Instruction.label insn) in
    let target_offset = (JVM_Instruction.size insn) in
    if (label + target_offset < maxLabel) then
      (
        (Int32Map.add (Int32.of_int (label+target_offset)) (Block.empty) m,
         Int32Map.add (Int32.of_int ((label)+(target_offset))) (true) sb)
      )
    else
      (m, sb)
  | New ->
     let filteredHandlers = (Handler.handlers_for (catches) (JVM_Instruction.label insn)) in
     let (newM, newSB) = set_handlers (m) (sb) (filteredHandlers) (maxLabel) in
     next_new_block (JVM_Instruction.label insn) (JVM_Instruction.size insn) (newM) (newSB)
  | Getfield
  | Putfield ->
     let filteredHandlers = (Handler.handlers_for (catches) (JVM_Instruction.label insn)) in
     let (newM, newSB) = set_handlers (m) (sb) (filteredHandlers) (maxLabel) in
     next_new_block (JVM_Instruction.label insn) (JVM_Instruction.size insn) (newM) (newSB)
  | (Invoke x) ->
     let filteredHandlers = (Handler.handlers_for (catches) (JVM_Instruction.label insn)) in
     let (newM, newSB) = set_handlers (m) (sb) (filteredHandlers) (maxLabel) in
     next_new_block (JVM_Instruction.label insn) (JVM_Instruction.size insn) (newM) (newSB)
  | Goto ->
     let target_offset = JVM.getGotoArgs insn in
     let label = (JVM_Instruction.label insn) in
     (Int32Map.add (Int32.of_int (label+target_offset)) (Block.empty) m,
      Int32Map.add (Int32.of_int ((label)+(target_offset))) (true) sb)
  | If ->
     let target_offset = JVM.IfArgs.offset (JVM.getIfArgs insn) in
     let label = (JVM_Instruction.label insn) in
     let size = (JVM_Instruction.size insn) in
     (Int32Map.add (Int32.of_int (label+size)) (Block.empty)
                   (Int32Map.add (Int32.of_int (label+target_offset)) (Block.empty) m),
      Int32Map.add (Int32.of_int (label+size)) (true)
                   (Int32Map.add (Int32.of_int (label+target_offset)) (true) sb))
  | Nop | Push | Pop | Load | Store | Binop | IInc | Swap | Dup  -> (
    match Int32Map.mem (Int32.of_int (JVM_Instruction.label insn + JVM_Instruction.size insn)) sb with
    | true -> (m, sb)
    | false -> (m,
                Int32Map.add (Int32.of_int (JVM_Instruction.label insn + JVM_Instruction.size insn)) (false) sb) )
  | Throw -> let filteredHandlers = (Handler.handlers_for (catches) (JVM_Instruction.label insn)) in
             set_handlers (m) (sb) (filteredHandlers) (maxLabel)
;;

let rec parseInsns (jProg: JVM_Instruction.t list) (maxLabel:int) (m:dBlockMap) (sb:sbMap)
                   (catches : Handler.t list) =
  match jProg with
    [] -> (m, sb)
  | insn :: t ->
     let (newM, newSB) = parseInsn (insn) (maxLabel) (m) (sb) (catches) in
     parseInsns (t) (maxLabel) (newM) (newSB) (catches);;

let parse (jProg: JVM_Instruction.t list) (maxLabel:int) (catches : Handler.t list) =
  let sb = Int32Map.add (0l) (true) Int32Map.empty in
  let m = Int32Map.add (0l) (Block.empty) Int32Map.empty in
  parseInsns (jProg) (maxLabel) (m) (sb) (catches);;
