open Compiler_meta;;
open Map_type;;
open Instructions;;
open Instructions.JVM;;
open Instructions.JVM.JVM_Instruction;;
open Certificate;;
open Constant;;
open Utility;;

let get_className_from_ConstantClass (idx:int) (pool:constantMap) =
  match Int32Map.find (Int32.of_int idx) (pool) with
    Class cst -> ClassConstant.name cst
    | _ -> raise (Invalid_argument ("Index " ^ string_of_int(idx) ^ " is not a class constant"))
;;
    
let get_info_from_ConstantFmi (idx:int) (pool:constantMap) =
  match Int32Map.find (Int32.of_int idx) (pool) with
    FMI cst -> (ClassConstant.name (FMIConstant.classConstant cst), 
                NATConstant.name (FMIConstant.natConstant cst),
                NATConstant.desc  (FMIConstant.natConstant cst))
    | _ -> raise (Invalid_argument ("Index " ^ string_of_int(idx) ^ " is not a field, method, or interface method constant"))
;;

let consolidate_int (b1:int) (b2:int) = 
  let value = ((b1 * 256) + b2) in
    signed_int(value);;

let rec construct_code_rec (input:int list) (off:int) (pool:constantMap) (maxLabel:int)=
  match input with
    [] -> (maxLabel, [], Int32Map.empty)
    | h :: t ->
			let current_typing = JVM_ins_typing.empty in  
			let (remaining_list, current_insn, current_size) = 
      (
        match (int_to_hex_string h) with
				(* Nop and CheckCast *)
				| "00" -> (t, JVM.createNopInsn (off) (1) (current_typing), 1)
				| "C0" -> (List.tl (List.tl t), JVM.createNopInsn (off) (3) (current_typing), 3)
        (* Push *)
        | "02" 
        | "03" 
        | "04" 
        | "05" 
        | "06" 
        | "07" 
        | "08" as opcode ->
          let value = match opcode with 
            | "02" -> -1 | "03" -> 0 | "04" -> 1 | "05" -> 2 
            | "06" -> 3 |"07" -> 4 | "08" -> 5 | _ -> raise (Invalid_argument "Bogus opcode") 
          in 
					  (t, JVM.createPushInsn (off) (1) (value) (current_typing) , 1) 
        | "10" ->
          let value = List.hd t in
					  (List.tl t, JVM.createPushInsn (off) (2) (value) (current_typing), 2)
        | "11" ->
          let value = consolidate_int (List.hd t) (List.hd (List.tl t)) in
					  (List.tl (List.tl t), JVM.createPushInsn (off) (3) (value) (current_typing), 3)
				| "12" ->
					let index = List.hd t in
					let const = getConstantValue index (fun idx -> Int32Map.find (Int32.of_int idx) pool) in
						(List.tl t, JVM.createPushInsn (off) (2) (ValueConstant.value const) (current_typing), 2)	
				| "13" ->
					let index = consolidate_int (List.hd t) (List.hd (List.tl t)) in
					let const = getConstantValue index (fun idx -> Int32Map.find (Int32.of_int idx) pool) in
						(List.tl (List.tl t), JVM.createPushInsn (off) (3) (ValueConstant.value const) (current_typing), 3)	
        (* Binop *)
        | "60"
        | "64"
        | "6C"
        | "68"
        | "70"
        | "78"
        | "7A"
        | "7C"
        | "7E" 
        | "80"
        | "82" as opcode ->
          (t, JVM.createBinopInsn (off) (1) (opcode) (current_typing), 1)
        (* Load *)
        | "15" | "19" ->
					(List.tl t, JVM.createLoadInsn (off) (2) (List.hd t) (current_typing), 2)
        | "1A" | "2A"
        | "1B" | "2B"
        | "1C" | "2C"
        | "1D" | "2D" as opcode ->
          let value = match opcode with 
            | "1A" | "2A" -> 0 
            | "1B" | "2B" -> 1 
            | "1C" | "2C" -> 2 
            | "1D" | "2D" -> 3 | 
            _ -> raise (Invalid_argument "Bogus opcode") 
          in
					(t, JVM.createLoadInsn (off) (1) (value) (current_typing), 1)
        (* Store *) 
        | "36" | "3A" ->
					(List.tl t, JVM.createStoreInsn (off) (2) (List.hd t) (current_typing), 2)
        | "3B" | "4B"
        | "3C" | "4C"
        | "3D" | "4D"
        | "3E" | "4E" as opcode ->
          let value = match opcode with 
            | "3B" | "4B" -> 0 
            | "3C" | "4C" -> 1 
            | "3D" | "4D" -> 2 
            | "3E" | "4E" -> 3 
            | _ -> raise (Invalid_argument "Bogus opcode") 
          in
					(t, JVM.createStoreInsn (off) (1) (value) (current_typing), 1)
        (* Pop *)
        | "57" ->
          (t, JVM.createPopInsn (off) (1) (current_typing), 1)
        (* Swap *)
        | "5F" ->
          (t, JVM.createSwapInsn (off) (1) (current_typing), 1)
        (* If z *)
        | "99" 
        | "9A" 
        | "9B" 
        | "9C" 
        | "9D" 
        | "9E"
        (* If *)
        | "9F" 
        | "A0" 
        | "A1" 
        | "A2" 
        | "A3" 
        | "A4" as opcode ->
          (
            let offset = consolidate_int (List.hd t) (List.hd (List.tl t)) in
            (List.tl (List.tl t), JVM.createIfInsn (off) (3) (offset) (opcode) (current_typing), 3)
          )
        (* Goto *)
        | "A7" ->
          let offset = consolidate_int (List.hd t) (List.hd (List.tl t)) in
            (List.tl (List.tl t), JVM.createGotoInsn (off) (3) (offset) (current_typing), 3)
        (* Return *)
        | "AC" 
        | "B1" as opcode ->
          (t, JVM.createReturnInsn (off) (1) (opcode) (current_typing), 1)
        (* New *)  
        | "BB" -> 
          let class_index = consolidate_int (List.hd t) (List.hd (List.tl t)) in
          let className = get_className_from_ConstantClass (class_index) (pool) in
              (List.tl (List.tl t), JVM.createNewInsn (off) (3) (class_index) (className) (current_typing), 3)
        (* Get / Put field *)
        | "B4" 
        | "B5" as opcode ->
          let field_index = consolidate_int (List.hd t) (List.hd (List.tl t)) in
          let (className, natName, desc) = get_info_from_ConstantFmi (field_index) (pool) in
            (List.tl (List.tl t), JVM.createFieldInsn (off) (3) (opcode) (field_index) (className) (natName) (desc) (current_typing), 3)
        (* Invoke *)
        | "B6" | "B7" ->
          let method_index = consolidate_int (List.hd t) (List.hd (List.tl t)) in
          let (className, natName, desc) = get_info_from_ConstantFmi (method_index) (pool) in
					let ins_result = JVM.createInvokeInsn (off) (3) (desc) (method_index) (className) (natName) (desc) (current_typing) in
            (List.tl (List.tl t), ins_result, 3)
        (* Throw *)    
        | "BF" ->
              (t, JVM.createThrowInsn (off) (1) (current_typing), 1)
        (* additional from Barthe *)
        | "84" -> (* Iinc *)
          (List.tl (List.tl t), JVM.createIincInsn (off) (3) (List.hd t) (signed_byte (List.hd (List.tl t))) (current_typing), 3)
        | "59" -> (* Dup *) 
          (t, JVM.createDupInsn (off) (1) (current_typing), 1)
        (* Unknown Code *)  
        | _ -> raise (Invalid_argument ("[parse_byte] Not Implemented : " ^ (int_to_hex_string h) ^ " - " ^ string_of_int h))
      ) in
			let (maxLabel, res, resMap) = construct_code_rec (remaining_list) (off + current_size) (pool) (off) (*bm_cert*) in
			(maxLabel, current_insn :: res, Int32Map.add (Int32.of_int off) (current_insn) resMap);;
      
let construct_code (input:int list) (pool:constantMap) =
  construct_code_rec (input) (0) (pool) (0)
	
