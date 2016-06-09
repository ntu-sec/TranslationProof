(* String Converter *)
let rec string_of_list (input: 'a list) (str_converter:'a -> string) (str: string) =
	match input with
		[] -> str
	| h :: [] -> (if (String.length str > 0) then str ^ ", " ^ (str_converter (h))
				else (str_converter (h)))
	| h :: t ->
			let newStr = (if (String.length str > 0) then str ^ ", " ^ (str_converter (h))
					else (str_converter (h))) in
			string_of_list (t) (str_converter) (newStr)
;;

(* note : consider about maintaining the last insn *)
module Block = struct
	type t = {parents : int list; succs : int list; psucc : int option;
	  insnList: (int * Instructions.DEX.DEX_Instruction.t) list; (* attached with the source label *) 
		catches: Handler.t list;
		block_se:Certificate.Level.basic_level; rt0: Certificate.RegistersType.t;
		order : int option; traced:bool; scopeAddress: (int * int)};;
  let retLabel = (-2);;
  let retLabel32 = (-2l);;
  let empty = { parents =[]; succs =[]; psucc = None; insnList =[]; catches=[];
	  block_se = Certificate.Level.bot_level ; rt0 = Certificate.RegistersType.empty;
		order=None; traced=false; scopeAddress=(-1,-1)};;

  let add_parent = fun parent t -> match List.mem parent t.parents with
	| true -> t
	| false -> {t with parents = parent::t.parents};;
  let add_succ = fun succ t -> match List.mem succ t.succs with
	| true -> t
	| false -> {t with succs = succ :: t.succs};;
  let update_pSucc = fun pSucc t -> {t with psucc = Some pSucc};;
  let add_insn = fun insn t -> {t with insnList = t.insnList @ [insn]};;
  let add_insns = fun insns t -> {t with insnList = t.insnList @ insns};;
  let update_se = fun se t -> {t with block_se = se};;
  let update_rt0 = fun rt t -> {t with rt0 = rt};;
  let update_typing = fun se rt t -> {t with block_se = se; rt0=rt};;
  let update_handlers = fun handlers t -> {t with catches=handlers};;
  let update_order = fun input t -> {t with order = Some input};;
	let update_traced = fun traced t -> {t with traced = traced};;
  let update_scopeAddress = fun sa t -> {t with scopeAddress = sa};;

  let parents = fun t -> t.parents;;
  let succs = fun t -> t.succs;;
  let insnList = fun t -> t.insnList;;
  let rec lastInsn_rec = fun insnList -> match insnList with
	| [] -> None
	| h::[] -> Some h
	| h::t -> lastInsn_rec (t);;
  let lastInsn = fun t -> lastInsn_rec (t.insnList);;
  let psucc = fun t -> t.psucc;;
  let catches = fun t -> t.catches;;
  let rt0 = fun t -> t.rt0;;
  let se = fun t -> t.block_se;;
  let order = fun t -> t.order;;
  let traced = fun t -> t.traced;;
  let scopeAddress = fun t -> t.scopeAddress;;
  let to_string = fun input ->
		let psucc = match input.psucc with None -> "" | Some x -> string_of_int (x) in
		"{P:[" ^ (string_of_list (input.parents) (string_of_int) ("")) ^ "]; S:["
	^ (string_of_list (input.succs) (string_of_int) ("")) ^ "]; PS:" ^
	psucc ^ (*"; Insn:[" ^ (string_of_list (input.insnList)
			(fun ins -> Instructions.DEX.DEX_Instruction.to_string ins) ("")) ^ "]}"*)
			"; se:" ^(Certificate.Level.string_of_basic_level input.block_se) ^ "; type:" ^
			(Certificate.RegistersType.to_string input.rt0)
end;;

type dBlockMap = Block.t Map_type.Int32Map.t;;
