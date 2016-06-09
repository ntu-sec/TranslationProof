open Instructions;;
open Instructions.JVM;;
open Map_type;;

(* BFS style tracing the control flow of the program, the intuition behind*)
(* this is that for the next step in the type inference we require that the*)
(* program point already has the SE and type *)

let rec trace_rec (working_set : int list) (m : JVM_Instruction.t Int32Map.t) (loopMap : bool Int32Map.t)
        : int32 list =
  match working_set with
  | [] -> []
  | label :: t ->
     let label32 = Int32.of_int (label) in
     let ins = Int32Map.find label32 m in
     match JVM_Instruction.code ins with
     | Nop
     | Push
     | Pop
     | Load
     | Store
     | Binop
     | IInc
     | New
     | Dup
     | Getfield
     | Putfield
     | Swap -> ( let size = JVM_Instruction.size ins in
                 let succ32 = Int32.of_int (label + size) in
                 match Int32Map.find (succ32) loopMap with
                 | false ->
                    let newLoopMap = Int32Map.add (succ32) (true) (loopMap) in
                    label32 :: trace_rec (t @ [label + size]) (m) (newLoopMap)
                 | true -> label32 :: trace_rec t m loopMap )
     | Goto -> (let args = JVM.getGotoArgs ins in
                let target32 = Int32.of_int (label + args) in
                match Int32Map.find (target32) loopMap with
                | false ->
                   let newLoopMap = Int32Map.add (target32) (true) (loopMap) in
                   label32 :: trace_rec (t @ [label + args]) (m) (newLoopMap)
                | true -> label32 :: trace_rec t m loopMap )
     | If -> (let args = JVM.getIfArgs ins in
              let size = JVM_Instruction.size ins in
              let succ32 = Int32.of_int (label + size) in
              let target = label + IfArgs.offset (args) in
              let target32 = Int32.of_int (target) in
              let (newList, newLoopMap) =
                match Int32Map.find (succ32) loopMap with
                | false -> ([label+size], Int32Map.add (succ32) (true) (loopMap))
                | true -> ([], loopMap) in
              match Int32Map.find (target32) loopMap with
              | false -> let newLoopMap = Int32Map.add (Int32.of_int target) (true) (newLoopMap) in
                         label32 :: trace_rec (t @ (target :: newList)) (m) (newLoopMap)
              | true -> label32 :: trace_rec (t @ newList) (m) (newLoopMap)
             )
     | Return -> label32 :: trace_rec (t) (m) (loopMap)
     | Invoke m' -> (let size = JVM_Instruction.size ins in
                     let succ32 = Int32.of_int (label + size) in
                     match Int32Map.find (succ32) loopMap with
                     | false ->
                        let newLoopMap = Int32Map.add (succ32) (true) (loopMap) in
                        label32 :: trace_rec (t @ [label + size]) (m) (newLoopMap)
                     | true -> label32 :: trace_rec t m loopMap )
     | Throw -> raise (Invalid_argument "Not dealing with exception at this moment")

let rec create_loopMap (jInsnList : JVM_Instruction.t list) =
  match jInsnList with
  | [] -> Int32Map.empty
  | h :: t -> Int32Map.add (JVM_Instruction.label32 h) (false) (create_loopMap t);;

let trace (jInsnList : JVM_Instruction.t list) (jInsnMap : JVM_Instruction.t Int32Map.t) =
  match jInsnList with
  | [] -> raise (Invalid_argument "empty method")
  | h :: t -> let loopMap = create_loopMap (jInsnList) in
              let label = JVM_Instruction.label h in (* technically speaking label must be 0 *)
              trace_rec ([label]) (jInsnMap) (loopMap);;
