open Compiler_meta;;
open Map_type;;
open Instructions;;
open Instructions.DEX;;
open Handler;;
open Certificate;;

let cb_compare ((aLabel, aBlock): int32 * Block.t) ((bLabel, bBlock): int32 * Block.t) =
  if (Block.order aBlock > Block.order bBlock) then 1
  else (-1);;

let add_nodup_list_map (key:int) (value:int) (map:(int list) Int32Map.t) =
  let key32 = Int32.of_int key in
  match Int32Map.mem key32 map with
  | true -> (let current_list = Int32Map.find (Int32.of_int key) map in
             match List.mem value current_list with
             | true -> map
             | false -> Int32Map.add (Int32.of_int key) (value::current_list) map)
  | false -> Int32Map.add key32 ([value]) map;;

let outputInsn (inp: int*DEX_Instruction.t) (address: int)
               (jvm_dex:(int list) Int32Map.t) (dex_jvm: int Int32Map.t) =
  let size = 2 in
  let (source, ins) = inp in
  let new_dex_jvm = Int32Map.add (Int32.of_int address) (source) dex_jvm in
  let new_jvm_dex = add_nodup_list_map (source) (address) (jvm_dex) in
  let newIns = DEX_Instruction.update_label_size (address) (size) (ins) in
  let size =
    match DEX_Instruction.code ins with
    (* At the moment we only care about integer values (excluding wide values) *)
    | Nop -> DEX_Instruction.size ins
    |       Const -> let args = DEX.getConstArgs ins in
                     let r = DEX.ConstArgs.reg args in
                     let c = DEX.ConstArgs.value args in
                     if (r < 16) && ((c > -8) && (c < 8)) then 1
                     else if (r < 256) && ((c > -32768) && (c < 32768)) then 2
                     else if (r < 256) && (c = (c land (15 lsl 4))) then 2
                     else 3
    | Move -> let args = DEX.getMoveArgs ins in
              let r1 = DEX.MoveArgs.target args in
              let r2 = DEX.MoveArgs.source args in
              if (r1 < 16) && (r2 < 16) then 1
              else if (r1 < 256) && (r2 < 65536) then 2
              else 3 (* We don't care for those which can't be represented, which should be non-existent *)
    | If -> 2
    | Ifz -> 2
    | Return -> 1
    | Goto -> 1 (* TODO : this needs an extra measure later on *)
    | Binop -> let args = DEX.getBinopArgs ins in
               let r1 = DEX.BinopArgs.source1 args in
               let r2 = DEX.BinopArgs.source2 args in
               if (r1 < 16) && (r2 < 16) then 1 else 2
    | BinopConst -> 2
    | Invoke -> 3
    | New | Field Get | Field Put -> 2
    | MoveResult -> 1
    | _ as c -> raise (Invalid_argument
                         ("[consolidate_block:outputInsn] DEX code not implemented yet " ^ DEX.string_of_insnType c))
  in
  (newIns, address + size, new_jvm_dex, new_dex_jvm);;

let rec outputInsnList (inp: (int*DEX_Instruction.t) list) (out: DEX_Instruction.t list)
                       (lastAddress: int) (address: int) (jvm_dex:(int list) Int32Map.t) (dex_jvm: int Int32Map.t) =
  match inp with
    [] -> (out, lastAddress, address, jvm_dex, dex_jvm)
  | h :: t ->
     let (newIns, newAddress, new_jvm_dex, new_dex_jvm) =
       outputInsn (h) (address) (jvm_dex) (dex_jvm) in
     outputInsnList (t) (out @ [newIns]) (address) (newAddress) (new_jvm_dex) (new_dex_jvm);;

let outputBlock (lbl: int32) (inp: Block.t) (sa: saMap) (address: int)
                (jvm_dex:(int list) Int32Map.t) (dex_jvm: int Int32Map.t)=
  let newSA = (Int32Map.add (lbl) (Int32.of_int address) sa) in
  let (out, lastAddress, address, new_jvm_dex, new_dex_jvm) = outputInsnList (Block.insnList inp)
                                                                             ([]) (address) (address) (jvm_dex) (dex_jvm) in
  (out, lastAddress, address, newSA, new_jvm_dex, new_dex_jvm)
;;

(* Assume that this function will not be called by a block that doesn't have a primary successor *)
let get_succ_rt (b: Block.t) (m: dBlockMap) =
  match Block.psucc b with
  | None -> raise (Invalid_argument "error:Attempting to create a goto instruction on a block that doesn't have a primary successor")
  | Some label -> let succ_b = Int32Map.find (Int32.of_int label) (m) in
                  (Block.rt0 succ_b);;

let rec replace_last_insn (l:DEX_Instruction.t list) (x:DEX_Instruction.t) =
  match l with
  | [] -> l
  | h :: [] -> x :: []
  | h :: t -> replace_last_insn (t) (x);;

let rec get_last_insn (l:DEX_Instruction.t list) =
  match l with
  | [] -> None
  | h :: [] -> Some h
  | h :: t -> get_last_insn (t);;

let rec constructResult (lst: (int32 * Block.t) list) (res: DEX_Instruction.t list) (sa: saMap)
                        (address: int) (m: dBlockMap) (jvm_dex:(int list) Int32Map.t) (dex_jvm: int Int32Map.t) =
  match lst with
    [] -> (res, sa, m, jvm_dex, dex_jvm)
  | (lbl, content) :: []-> (
    let (insnListResult, lastAddress, newAddress, newSA, new_jvm_dex, new_dex_jvm) =
      outputBlock (lbl) (content) (sa) (address) (jvm_dex) (dex_jvm) in
    match Block.lastInsn content with
      None ->
      (* TODO: have to check this case where it is the last block in the *)
      (* list yet it is empty.                                           *)
      let content = Block.update_scopeAddress (address, lastAddress) (content) in
      let m = Int32Map.add (lbl) (content) (m) in
      (res @ insnListResult, newSA, m, new_jvm_dex, new_dex_jvm)
    | Some (source, ins) -> (
      let lastAddress = newAddress in
      ( match DEX_Instruction.code ins with
          DEX.Return ->
          (
            let content = Block.update_scopeAddress (address, lastAddress) content in
            let m = Int32Map.add (lbl) (content) (m) in
            (res @insnListResult, newSA, m, new_jvm_dex, new_dex_jvm)
          )
        | _ ->
           (
             let lastAddress = newAddress in
             let psucc = match Block.psucc content with
               | Some x -> x
               | None -> raise (Invalid_argument "error:last non-return block will always have a primary successor")
             in (*there will always be a primary successor for non return last block *)
             let (newLastIns, newAddress, new_jvm_dex', new_dex_jvm') = outputInsn (source, DEX.createGotoInsn (-1) (1) (psucc)
                                                                                                               (Block.se content) (get_succ_rt (content) (m))) (newAddress) (new_jvm_dex) (new_dex_jvm) in
             let content = Block.update_scopeAddress (address, lastAddress) (content) in
             let m = Int32Map.add (lbl) (content) (m) in
             (res @ insnListResult @ [newLastIns], newSA, m, new_jvm_dex', new_dex_jvm') )
      )
    )
  )
  | (lbl, content) :: t -> (
    let (insnListResult, lastAddress, newAddress, newSA, new_jvm_dex, new_dex_jvm) =
      outputBlock (lbl) (content) (sa) (address) (jvm_dex) (dex_jvm) in
    let (nextLbl, nextBlock) = (List.hd t) in
    let psucc = Block.psucc content in
    match Block.lastInsn content with
      None ->
      (* hard coded for now, the behaviour is too weird if the block     *)
      (* doesn't have primary successor yet it is empty                  *)
      if (psucc <> Some (Int32.to_int (nextLbl))) && (psucc <> None) then
        raise (Invalid_argument "Empty block with no primary successor")
      else
        (* Not possible to have content yet the last instruction is None *)
        (constructResult (t) (res) (newSA) (newAddress) (m) (new_jvm_dex) (new_dex_jvm))
    | Some (source, ins) ->
       (
         if (psucc <> Some (Int32.to_int (nextLbl))) && (psucc <> None) then
           let psuccLabel = match psucc with
             | None -> raise (Invalid_argument "error:psucc is None after a condition psucc <> None")
             | Some x -> x in
           ( match DEX_Instruction.code ins with
               DEX.If ->
               (* If in fact the branch is the next block to output,    *)
               (* then just reverse the branch                          *)
               ( let contentSucc = Block.succs content in
                 let secondBranch = List.nth contentSucc 1 in
                 if secondBranch = Int32.to_int (nextLbl) then
                   (
                     let Some ifInsn = get_last_insn (insnListResult) in
                     let newInsnListResult = replace_last_insn (insnListResult) (DEX.opposite_if_insn (ifInsn) (List.nth contentSucc 0)) in
                     let content = Block.update_scopeAddress (address, lastAddress) (content) in
                     let newM = Int32Map.add (lbl) (content) (m) in
                     constructResult (t) (res @ newInsnListResult) (newSA) (newAddress) (m) (new_jvm_dex) (new_dex_jvm))
                     (* for every other case we just add a goto instruction *)
                 else
                   (
                     let lastAddress = newAddress in
                     let (newLastIns, newAddress, new_jvm_dex', new_dex_jvm') = outputInsn (source, DEX.createGotoInsn (-1) (1) (psuccLabel)
                                                                                                                       (Block.se nextBlock) (get_succ_rt (content) (m))) (newAddress) (new_jvm_dex) (new_dex_jvm) in
                     (* need to fix this region to be included in the region of the source instruction *)
                     let content = Block.update_scopeAddress (address, lastAddress) content in
                     let m = Int32Map.add (lbl) (content) (m) in
                     constructResult (t) (res @ insnListResult @ [newLastIns]) (newSA) (newAddress) (m) (new_jvm_dex') (new_dex_jvm')
                   )
               )
             | DEX.Ifz ->
                (* If in fact the branch is the next block to output,    *)
                (* then just reverse the branch                          *)
                ( let contentSucc = Block.succs content in
                  let secondBranch = List.nth contentSucc 1 in
                  if secondBranch = Int32.to_int (nextLbl) then
                    ( let Some ifzInsn = get_last_insn (insnListResult) in
                      let newInsnListResult = replace_last_insn (insnListResult) (DEX.opposite_ifz_insn (ifzInsn) (List.nth contentSucc 0)) in
                      let content = Block.update_scopeAddress (address, lastAddress) (content) in
                      let newM = Int32Map.add (lbl) (content) (m) in
                      constructResult (t) (res @ newInsnListResult) (newSA) (newAddress) (m) (new_jvm_dex) (new_dex_jvm))
                      (* for every other case we just add a goto instruction *)
                  else
                    (
                      let lastAddress = newAddress in
                      let (newLastIns, newAddress, new_jvm_dex', new_dex_jvm') = outputInsn (source, DEX.createGotoInsn (-1) (1) (psuccLabel)
                                                                                                                        (Block.se nextBlock) (get_succ_rt (content) (m))) (newAddress) (new_jvm_dex) (new_dex_jvm) in
                      let content = Block.update_scopeAddress (address, lastAddress) content in
                      let m = Int32Map.add (lbl) (content) (m) in
                      constructResult (t) (res @ insnListResult @ [newLastIns]) (newSA) (newAddress) (m) (new_jvm_dex') (new_dex_jvm')
                    )
                )
             (* If it's not a branching instruction but the next instruction to output is not the successor *)
             | _ -> (
               let lastAddress = newAddress in
               let (newLastIns, newAddress, new_jvm_dex', new_dex_jvm') =
                 outputInsn (source, DEX.createGotoInsn (-1) (1) (psuccLabel)
                                                        (Block.se content) (get_succ_rt (content) (m))) (newAddress) (new_jvm_dex) (new_dex_jvm) in
               let content = Block.update_scopeAddress (address, lastAddress) content in
               let m = Int32Map.add (lbl) (content) (m) in
               constructResult (t) (res @ insnListResult @ [newLastIns]) (newSA) (newAddress) (m) (new_jvm_dex') (new_dex_jvm'))
           )
         else (* default cases *)
           (
             let content = Block.update_scopeAddress (address, newAddress) content in
             let m = Int32Map.add (lbl) (content) (m) in
             constructResult (t) (res @ insnListResult) (newSA) (newAddress) (m) (new_jvm_dex) (new_dex_jvm) )
       )
  )
;;

(* leave this for now although there is inherent danger, still the         *)
(* addressing of the instructions are only cosmetic, the real value still  *)
(* lies in the block addressing                                            *)
let rec fixTarget (inp: DEX_Instruction.t list) (out: DEX_Instruction.t list) (sa: saMap) (m: dBlockMap) =
  match inp with
    [] -> (out, m)
  | h :: t ->
     match DEX_Instruction.code h with
       DEX.Goto ->
       let arg = DEX.getGotoArgs h in
       let newTarget = Int32Map.find (Int32.of_int (DEX.GotoArgs.offset arg)) sa in
       let newIns = DEX.fixGotoTarget (h) (Int32.to_int newTarget) in
       fixTarget (t) (out @ [newIns]) (sa) (m)
     | DEX.If ->
        let arg = DEX.getIfArgs h in
        let newTarget = Int32Map.find (Int32.of_int (DEX.IfArgs.offset arg)) sa in
        let newIns = DEX.fixIfTarget (h) (Int32.to_int newTarget) in
        fixTarget (t) (out @ [newIns]) (sa) (m)
     | DEX.Ifz ->
        let arg = DEX.getIfzArgs h in
        let newTarget = Int32Map.find (Int32.of_int (DEX.IfzArgs.offset arg)) sa in
        let newIns = DEX.fixIfzTarget (h) (Int32.to_int newTarget) in
        fixTarget (t) (out @ [newIns]) (sa) (m)
     | _ -> fixTarget (t) (out @ [h]) (sa) (m)
;;

(* assume that the call to this method always pass through length equality *)
(* check first                                                             *)
let rec sameHandler_rec (catchList1 : Handler.t list) (catchList2 : Handler.t list) =
  match catchList1 with
    [] -> true
  | h1 :: t1 ->
     (
       let (h2 :: t2) = catchList2 in
       match (Handler.handlerPC h1 = Handler.handlerPC h2) with
         true -> sameHandler_rec (t1) (t2)
       | false -> false
     )
;;

let sameHandler (catchList1 : Handler.t list) (catchList2 : Handler.t list) =
  let equalLength = ((List.length catchList1) = (List.length catchList2)) in
  match equalLength with
    true -> (sameHandler_rec (catchList1) (catchList2))
  | false -> false
;;

let rec constructEntries_rec (catches : Handler.t list) ((start_pc, end_pc) : (int * int)) (result : Handler.t list) =
  match catches with
    [] -> (result)
  | h :: t ->
     let result = Handler.create (start_pc) (end_pc) (Handler.handlerPC h) (Handler.catchType h)
                  :: result in
     constructEntries_rec (t) (start_pc, end_pc) (result)
;;

(* we assume that the scope will not be ridiculously long, that is, more   *)
(* than 65535 bytes long                                                   *)
let rec collectHandlers_rec (lst: (int32 * Block.t) list) (resultHandler : Handler.t list)
                            (currentHandlers : Handler.t list) ((currentStart, currentEnd) : (int * int)) =
  match lst with
    [] ->
    (
      match currentHandlers with
        [] -> (resultHandler)
      | h :: t -> let newHandlers = constructEntries_rec (currentHandlers) (currentStart, currentEnd) ([]) in
                  (resultHandler @ newHandlers)
    )
  | (lbl, content) :: t ->
     (
       match Block.catches content with
         [] -> collectHandlers_rec (t) (resultHandler) (currentHandlers) (currentStart, currentEnd)
       | handlerList -> let (startPC, endPC) = Block.scopeAddress content in
                        (
                          match sameHandler (Block.catches content) (currentHandlers) with
                            true -> collectHandlers_rec (t) (resultHandler) (currentHandlers) (currentStart, endPC)
                          | false ->
                             let newHandlers = constructEntries_rec (currentHandlers) (currentStart, currentEnd) ([]) in
                             collectHandlers_rec (t) (resultHandler @ newHandlers) (Block.catches content) (endPC, endPC)
                        )
     )
;;

let collectHandlers (m: dBlockMap) =
  let allBindings = Int32Map.bindings m in
  let sortedList = List.sort (cb_compare) (allBindings) in
  collectHandlers_rec (sortedList) ([]) ([]) (-1, -1)
;;

let consolidateBlock (localN:int) (m: dBlockMap) (retBlock: Block.t) (catches: Handler.t list)
                     (initial_jvm_dex : (int list) Int32Map.t)=
  let allBindings = Int32Map.bindings m in
  let sortedList = List.sort (cb_compare) ((Block.retLabel32, retBlock) :: allBindings) in
  let (insnList, sa, m, jvm_dex, dex_jvm) =
    constructResult (sortedList) ([]) (Int32Map.empty) (localN + 1) (m) (initial_jvm_dex) (Int32Map.empty) in
  let (insnList, m) = fixTarget (insnList) ([]) (sa) (m) in
  let (handlerList) = collectHandlers (m) in
  let insnMap = List.fold_right (fun h m -> (Int32Map.add (Int32.of_int (DEX_Instruction.label h))
                                                          h m)) insnList Int32Map.empty in
  (insnList, insnMap, m, handlerList, jvm_dex, dex_jvm)
;;

  (* TODO : make sure that handlers are correct. Feels like there will be a break somewhere with the new structure *)
