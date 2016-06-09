open Compiler_meta;;
open Map_type;;
open Instructions;;
open Instructions.JVM;;
open Instructions.DEX;;
open Handler;;
open Certificate;;

(* In the original dx tool, 7 corresponds to the SPECIAL_LABEL_COUNT (like *)
(* return block)                                                           *)
let getMinimumUnreservedLabel (maxLabel: int) = (maxLabel * 2) + 7;;

let getAvailableLabel (m: dBlockMap) (maxLabel: int) =
	let x = getMinimumUnreservedLabel (maxLabel) in
	let (maxKey, value) = Int32Map.max_binding (m) in
	let y = Int32.to_int maxKey in
	match x > y with
		true -> x
	| false -> y + 1
;;

let createInitBlock (insn: JVM_Instruction.t) (localN: int) (m: dBlockMap) 
: dBlockMap * bpMap * tsMap =
	let initialTS = Int32Map.add (0l) (localN) (Int32Map.empty) in
	match JVM_Instruction.args insn with
	(* to be precise there are instructions which are not possible to be the *)
	(* initial instruction. But we ignore it at this moment                  *)
		ArgsArgsType x ->
			let succOffset = (JVM_Instruction.label insn) + (JVM_Instruction.size insn) in
			(m,
				Int32Map.add (Int32.of_int succOffset) (0l) (Int32Map.add (0l) (0l) (Int32Map.empty)),
				Int32Map.add (Int32.of_int succOffset) (localN + 1) (initialTS))
	| GotoArgsType target_offset ->
			let succOffset = (JVM_Instruction.label insn) + target_offset in
			let startBlock = Block.add_succ (succOffset) (Block.update_pSucc (succOffset) (Block.empty)) in
			let targetBlock = Block.add_parent (0) (Block.empty) in
			let succOffset32 = Int32.of_int succOffset in
			(Int32Map.add (succOffset32) (targetBlock) (Int32Map.add (0l) (startBlock) (m)),
				Int32Map.add (succOffset32) (succOffset32) (Int32Map.add (0l) (0l) (Int32Map.empty)),
				Int32Map.add (succOffset32) (localN) (initialTS))
	| ClassArgsType x ->
			let succOffset = (JVM_Instruction.label insn) + (JVM_Instruction.size insn) in
			let startBlock = Block.add_succ (succOffset) (Block.update_pSucc (succOffset) (Block.empty)) in
			let targetBlock = Block.add_parent (0) (Block.empty) in
			let succOffset32 = Int32.of_int succOffset in
			(Int32Map.add (succOffset32) (targetBlock) (Int32Map.add (0l) (startBlock) (m)),
				Int32Map.add (succOffset32) (succOffset32) (Int32Map.add (0l) (0l) (Int32Map.empty)),
				Int32Map.add (succOffset32) (localN + 1) (initialTS))
	| _ -> raise (Invalid_argument ("Bogus Initial Instruction " ^ JVM_Instruction.to_string insn));;

let rec createRetBlock (insns: JVM_Instruction.t list) (localN: int) (lvt: MethodPolicy.t) =
	match insns with
		[] -> ( raise (Invalid_argument "No Return Instruction") )
	| h :: t -> match JVM_Instruction.args h with
				JVM.ReturnArgsType x ->
					let (dexRetType, flag) = match x with
							JVM.Void -> (DEX.Void, 0)
						| JVM.Int -> (DEX.Int, 1)
					in
					let typeInfo = JVM_Instruction.typeInfo h in
					let se = JVM_ins_typing.se typeInfo in
					let translated_rt = translate_st_rt (JVM_ins_typing.typing typeInfo) (localN) (lvt) (None) in
					let translated_rt = RT_op.update_flag (flag) (translated_rt) in
					  Block.add_insn (JVM_Instruction.label h, DEX.createReturnInsn (-1) (1) (dexRetType) (se) (translated_rt)) 
						(Block.update_typing (se) (translated_rt) Block.empty)
			| _ -> createRetBlock (t) (localN) (lvt);;

let visitOneStepInsn (insn:JVM_Instruction.t) (m: dBlockMap) (bp: bpMap) (ts: tsMap) (sb: sbMap)
		(isStartBlock: bool) (tsValue: int) =
	let il = JVM_Instruction.label insn in
	let is = JVM_Instruction.size insn in
	let isNextStart = (Int32Map.find (Int32.of_int (il + is)) sb) in
	let blockIndex = match isStartBlock with
			true -> Int32.of_int il
		| false -> Int32Map.find (Int32.of_int (il)) bp
	in
	let cb = Int32Map.find (blockIndex) m in
	match isNextStart with
		true ->
	(* Next instruction is start of another block *)
			(let succb = Int32Map.find (Int32.of_int (il + is)) m in
				(
					Int32Map.add (Int32.of_int (il + is))
					  (Block.add_parent (Int32.to_int blockIndex) succb)
						(Int32Map.add (blockIndex)
						(Block.add_succ (il + is) (Block.update_pSucc (il+is) cb)) m),
					Int32Map.add (Int32.of_int (il + is)) (Int32.of_int (il + is)) (bp),
					Int32Map.add (Int32.of_int (il + is)) (tsValue) (ts)
				))
	| false -> ((m, Int32Map.add (Int32.of_int (il + is)) (blockIndex) bp,
					Int32Map.add (Int32.of_int (il + is)) (tsValue) (ts)))
;;

let visitGotoInsn (insn: JVM_Instruction.t) (m: dBlockMap) (bp: bpMap) (ts: tsMap) (sb: sbMap)
		(isStartBlock: bool) (tsValue: int) =
	let il = JVM_Instruction.label insn in
	let target_offset = (JVM.getGotoArgs insn) in
	let succ_idx32 = Int32.of_int (il + target_offset) in
	let blockIndex = match isStartBlock with
			true -> Int32.of_int il
		| false -> Int32Map.find (Int32.of_int (il)) bp
	in
	let cb = Int32Map.find (blockIndex) m in
	let succb = Int32Map.find (succ_idx32) m in
	(
		Int32Map.add (succ_idx32)
		  (Block.add_parent (Int32.to_int blockIndex) succb)
			(Int32Map.add (blockIndex)
			  (Block.add_succ (il + target_offset) (Block.update_pSucc (il+target_offset) cb))
			m),
		Int32Map.add (succ_idx32) (succ_idx32) bp,
		Int32Map.add (succ_idx32) (tsValue) (ts)
	)
;;

let visitIfeqInsn (insn: JVM_Instruction.t) (m: dBlockMap) (bp: bpMap) (ts: tsMap) (sb: sbMap)
		(isStartBlock: bool) (tsValue: int) =
	let il = JVM_Instruction.label insn in
	let is = JVM_Instruction.size insn in
	let target_offset = JVM.IfArgs.offset (JVM.getIfArgs insn) in
	let succ1_idx32 = Int32.of_int (il + is) in
	let succ2_idx32 = Int32.of_int (il + target_offset) in
	let newBPMap = Int32Map.add (succ2_idx32) (succ2_idx32)
			(Int32Map.add (succ1_idx32) (succ1_idx32) bp) in
	let newTSMap = Int32Map.add (succ1_idx32) (tsValue) (Int32Map.add (succ2_idx32) (tsValue) (ts)) in
	let blockIndex = match isStartBlock with
			true -> Int32.of_int il
		| false -> Int32Map.find (Int32.of_int (il)) bp
	in
	let cb = Int32Map.find (blockIndex) m in
	let succt = Int32Map.find (succ2_idx32) m in
	let succf = Int32Map.find (succ1_idx32) m in
	(
		Int32Map.add (succ2_idx32)
		  (Block.add_parent (Int32.to_int blockIndex) (succt))
			(Int32Map.add (succ1_idx32)
			  (Block.add_parent (Int32.to_int blockIndex) (succf))
					(Int32Map.add (blockIndex)
					   (Block.add_succ (il+is) (Block.add_succ (il+target_offset) (Block.update_pSucc (il+is) cb)))
		m)),
		newBPMap,
		newTSMap
	)
;;

(* Helper for addHandler to add all the handler block covering the current *)
(* pc as successor blocks                                                  *)
let rec addHandlers_rec (startBlock_pc: int32) (current: Block.t) (m: dBlockMap) (bp: bpMap)
		(ts: tsMap) (tsValue: int) (catches : Handler.t list) (maxLabel: int) =
	match catches with
		[] -> (m, bp, ts)
	| h :: t ->
			let handler_idx32 = Handler.handlerPC32 h in
			let intermediate_idx = (Handler.handlerPC h + maxLabel) in
			let intermediate_idx32 = Int32.of_int (intermediate_idx) in
			let intermediateBlock = Int32Map.find (intermediate_idx32) (m) in
			let handlerBlock = Int32Map.find (handler_idx32) (m) in
			let newHandler = Block.add_parent (intermediate_idx) (handlerBlock) in
			let newCurrent = Block.add_succ (intermediate_idx) (current) in
			let newIntermediate = 
				Block.add_succ (Handler.handlerPC h) (Block.add_parent (Int32.to_int startBlock_pc)
				(Block.update_pSucc (Handler.handlerPC h) 
				(Block.add_insn (Handler.handlerPC h, DEX.createMoveExceptionInsn (-1) (1) (tsValue -1) (Level.bot_level) (RegistersType.empty)) 
				(intermediateBlock))))
			in
			(* TODO : type derivation for exceptions is definitely not correct *)
			let newM = Int32Map.add (handler_idx32) (newHandler)
					(Int32Map.add (startBlock_pc) (newCurrent)
							(Int32Map.add (intermediate_idx32) (newIntermediate) m))
			in
			let newBPMap = Int32Map.add (handler_idx32) (handler_idx32)
					(Int32Map.add (intermediate_idx32) (intermediate_idx32) (bp))
			in
			let newTSMap = Int32Map.add (handler_idx32) (tsValue)
					(Int32Map.add (intermediate_idx32) (tsValue + 1) (ts))
			in
			addHandlers_rec (startBlock_pc) (newCurrent) (newM) (newBPMap) (newTSMap) (tsValue) (t) (maxLabel)

let addHandlers (startBlock_pc: int32) (pc: int) (m: dBlockMap) (bp: bpMap) (ts: tsMap)
		(tsValue: int) (catches : Handler.t list) (maxLabel: int) =
	let filteredHandlers = Handler.handlers_for (catches) (pc) in
	let current = Int32Map.find (startBlock_pc) m in
	let (m, bp, ts) = addHandlers_rec (startBlock_pc) (current) (m) (bp)(ts) (tsValue) (filteredHandlers) (maxLabel) in
	(m, bp, ts, filteredHandlers)

let withIntermediateBlock (m: dBlockMap) (bp: bpMap) (ts: tsMap) (parent_lbl: int) (succ_lbl: int) (tsValue: int) (maxLabel: int) 
(ins: (int*DEX_Instruction.t) option) (applicableHandlers : Handler.t list) =
	let newLabel = getAvailableLabel (m) (maxLabel) in
	let blockWithIns = match ins with 
	| Some (source, insn) -> Block.add_insn (source, insn) (Block.empty)
	| None -> Block.empty in
	let newBlock = Block.add_parent (parent_lbl) (Block.add_succ (succ_lbl) 
	  (Block.update_pSucc (succ_lbl) (Block.update_handlers (applicableHandlers) (blockWithIns)))) in
	let currBlock = Int32Map.find (Int32.of_int parent_lbl) (m) in
	let succBlock = Int32Map.find (Int32.of_int succ_lbl) (m) in
	let currBlock = Block.add_succ (newLabel) (Block.update_pSucc newLabel 
	  (Block.update_handlers applicableHandlers currBlock)) in
	let succBlock = Block.add_parent (newLabel) (succBlock) in
	(
		Int32Map.add (Int32.of_int (newLabel)) (newBlock)
			(Int32Map.add (Int32.of_int (parent_lbl)) (currBlock)
					(Int32Map.add (Int32.of_int (succ_lbl)) (succBlock) (m))),
		Int32Map.add (Int32.of_int (newLabel)) (Int32.of_int (newLabel))
			(Int32Map.add (Int32.of_int (succ_lbl)) (Int32.of_int (succ_lbl)) (bp)),
		Int32Map.add (Int32.of_int (newLabel)) (tsValue - 1)
			(Int32Map.add (Int32.of_int (succ_lbl)) (tsValue) (ts))
	)
;;

let visitThrowingInsn (insn:JVM_Instruction.t) (m: dBlockMap) (bp: bpMap) (ts: tsMap)
		(isStartBlock: bool) (tsValue: int) (catches: Handler.t list) (localN: int) (maxLabel: int)
		(ins: (int * DEX_Instruction.t) option) =
	let il = JVM_Instruction.label insn in
	let is = JVM_Instruction.size insn in
	let blockIndex = match isStartBlock with
			true -> Int32.of_int il
		| false -> Int32Map.find (Int32.of_int (il)) bp
	in
	let (m, bp, ts, applicableHandlers) = addHandlers (blockIndex) (il) (m) (bp) (ts) (localN +1) 
	  (catches) (maxLabel) in
	withIntermediateBlock (m) (bp) (ts) (Int32.to_int blockIndex) (il + is) (tsValue) (maxLabel) 
	  (ins) (applicableHandlers)
;;

let visitReturnInsn (insn: JVM_Instruction.t) (maxLabel: int) (m: dBlockMap) (bp: bpMap) (ts: tsMap) (sb: sbMap) 
(isStartBlock: bool) (retBlock: Block.t) =
	let il = JVM_Instruction.label insn in
	let is = JVM_Instruction.size insn in
	let blockIndex = match isStartBlock with
			true -> Int32.of_int il
		| false -> Int32Map.find (Int32.of_int (il)) bp
	in
	let cb = Int32Map.find (blockIndex) m in
	let newBPMap =
		if ((il + is) < maxLabel) then
			(let next_idx32 = Int32.of_int (il + is) in
				Int32Map.add (next_idx32) (next_idx32) bp)
		else
			(bp)
	in
	((
			Int32Map.add (blockIndex) (Block.add_succ (Block.retLabel) (Block.update_pSucc (Block.retLabel) (cb))) m, 
			newBPMap,
			ts
		), 
		Block.add_parent (Int32.to_int blockIndex) (retBlock))
;;

let rec countArgs (str: string) (len: int) (count: int) =
	match str.[0] with
		'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' ->
			(match (len - 1) <= 0 with
					true -> (count + 1)
				| false -> countArgs (String.sub (str) (1) (len - 1)) (len - 1) (count + 1)
			)
	| '[' -> countArgs (String.sub (str) (1) (len - 1)) (len - 1) (count)
	| 'L' -> (let nextIndex = (String.index (str) (';')) + 1 in
				match nextIndex >= len with
					true -> (count + 1)
				| false -> countArgs (String.sub (str) (nextIndex) (len - nextIndex)) (len - nextIndex) (count + 1)
			)
	| _ as chr -> raise (Invalid_argument ("Invalid character for a method descriptor : " ^ Char.escaped (chr)))
;;

let parseInsn (insn: JVM_Instruction.t) (maxLabel: int) (localN: int) (m: dBlockMap) (bp: bpMap)
		(ts: tsMap) (sb: sbMap) (ret: Block.t) (catches: Handler.t list) 
		(oNextIns: JVM_Instruction.t option) (lvt: Certificate.MethodPolicy.t) =
	let currentTS = Int32Map.find (JVM_Instruction.label32 insn) (ts) in
	let isStartBlock = Int32Map.find (JVM_Instruction.label32 insn) (sb) in
	match JVM_Instruction.code insn with
	| JVM.Nop -> (visitOneStepInsn (insn) (m) (bp) (ts) (sb) (isStartBlock) (currentTS), ret)
	| Dup | Swap | Pop -> 
		          ( let newTS = match JVM_Instruction.code insn with
									Pop -> currentTS - 1
								| Swap -> currentTS
								| Dup -> currentTS + 1
								| _ as c -> raise (Invalid_argument ("Bogus opcode for Plain instruction" ^ JVM.string_of_insnType (c)))
							in
							(visitOneStepInsn (insn) (m) (bp) (ts) (sb) (isStartBlock) (newTS), ret))
	| JVM.Return ->
			(	let isStartBlock = (Int32Map.find (JVM_Instruction.label32 insn) sb) in
				visitReturnInsn (insn) (maxLabel) (m) (bp) (ts) (sb) (isStartBlock) (ret))
	| JVM.Throw -> (let blockIndex = (match isStartBlock with
										true -> JVM_Instruction.label32 insn
									| false -> Int32Map.find (Int32.of_int (JVM_Instruction.label insn)) bp)
							in
							let (m, bp, ts, applicableHandlers) = (addHandlers (blockIndex) (JVM_Instruction.label insn) 
							  (m) (bp) (ts) (localN + 1) (catches) (maxLabel)) in
							let currentBlock = Int32Map.find (blockIndex) (m) in
							let m = Int32Map.add (blockIndex) (Block.update_handlers (applicableHandlers) (currentBlock)) (m) in
							((m, bp, ts), ret) )
	| Store | Load | Push | IInc -> 
				(let newTS = match JVM_Instruction.code insn with
						Push
					| Load -> currentTS + 1
					| Store -> currentTS - 1
					| IInc -> currentTS
					| _ as c -> raise (Invalid_argument ("Bogus opcode for Args instruction " ^ JVM.string_of_insnType (c)))
				in
				(visitOneStepInsn (insn) (m) (bp) (ts) (sb) (isStartBlock) (newTS), ret)
			)
	| JVM.Binop -> (visitOneStepInsn (insn) (m) (bp) (ts) (sb) (isStartBlock) (currentTS - 1), ret) 
	| JVM.Goto -> (visitGotoInsn (insn) (m) (bp) (ts) (sb) (isStartBlock) (currentTS), ret) 
	| JVM.If -> (let ifArg = JVM.getIfArgs insn in
				let tsValue = match JVM.IfArgs.kind ifArg with
						JVM.Eq | JVM.Ne
					| JVM.Gt | JVM.Ge
					| JVM.Lt | JVM.Le -> (currentTS - 2)
					| JVM.Eqz | JVM.Nez
					| JVM.Gtz | JVM.Gez
					| JVM.Ltz | JVM.Lez -> (currentTS - 1)
				in
				(visitIfeqInsn (insn) (m) (bp) (ts) (sb) (isStartBlock) (tsValue), ret) )
	| JVM.New -> (visitThrowingInsn (insn) (m) (bp) (ts) (isStartBlock) (currentTS + 1) (catches) 
	    (localN) (maxLabel) (None), ret)
	| JVM.Getfield -> (visitThrowingInsn (insn) (m) (bp) (ts) (isStartBlock) (currentTS) (catches) 
	    (localN) (maxLabel) (None), ret)
	| JVM.Putfield -> (visitThrowingInsn (insn) (m) (bp) (ts) (isStartBlock) (currentTS - 2) (catches) 
	    (localN) (maxLabel) (None), ret)
	| JVM.Invoke t -> let arg = JVM.getFMIArgs insn in
		        let (tsValue) = (let closeIndex = String.index (FMIArgs.fmi_type arg) (')') in
								let str = String.sub (FMIArgs.fmi_type arg) (1) (closeIndex - 1) in
								let isVoid = (match t with JVM.Void -> 1 | JVM.Int -> 0) in
								match (String.length (str)) = 0 with
									false -> (currentTS - (countArgs (str) (String.length (str)) (0)) - isVoid)
								| true -> (currentTS - isVoid)) in
						let intermediateIns = 
							( let retSig = (FMIArgs.fmi_type arg).[(String.index (FMIArgs.fmi_type arg) (')')) + 1] in
								let noReturn = (retSig = 'V') in
								match noReturn with
									true -> None
								| false -> let Some nextInsn = oNextIns in 
									  let typeInfo = JVM_Instruction.typeInfo nextInsn in
	                  let current_se = JVM_ins_typing.se typeInfo in
										let st = JVM_ins_typing.typing typeInfo in
										let ([res], st') = ST_op.pop_n st 1 in
	                  let translated_rt = translate_st_rt (st') (localN) (lvt) (Some res) in
	                Some (JVM_Instruction.label insn, DEX.createMoveResultInsn (-1) (1) 
								        (tsValue - 1) (current_se) (translated_rt))
								(* TODO : need to fix this one for method instruction *)
							)
						in
				(visitThrowingInsn (insn) (m) (bp) (ts) (isStartBlock) (tsValue) (catches) (localN) (maxLabel)
						(intermediateIns), ret)
;;

let rec parseInsns (jProg: JVM_Instruction.t list) (maxLabel: int) (localN: int) (m: dBlockMap)
		(bp: bpMap) (ts: tsMap) (sb: sbMap) (ret: Block.t) (catches: Handler.t list) 
		(lvt: Certificate.MethodPolicy.t) =
	match jProg with
		[] -> (m, bp, ts, ret)
	| insn :: t -> 
		  let oNextInsn = match t with
			  | [] -> None
			  | nextIns :: t' -> Some nextIns in
			let ((newM, newBP, newTS), ret) = 
				parseInsn (insn) (maxLabel) (localN) (m) (bp) (ts) (sb) (ret) (catches) (oNextInsn) (lvt)
			in 
			parseInsns (t) (maxLabel) (localN) (newM) (newBP) (newTS) (sb) (ret) (catches) (lvt);;

let parse (jProg: JVM_Instruction.t list) (maxLabel: int) (localN: int) (lvt: Certificate.MethodPolicy.t) 
    (m: dBlockMap) (sb: sbMap) (catches: Handler.t list) =
	let (m, bp, ts) = createInitBlock (List.hd jProg) (localN) (m) in
	let ret = createRetBlock (jProg) (localN) (lvt) in
	parseInsns (List.tl jProg) (maxLabel: int) (localN) (m) (bp) (ts) (sb) (ret) (catches) (lvt);;

