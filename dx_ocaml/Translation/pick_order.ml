open Compiler_meta;;
open Map_type;;

(* Check if all the bindings have been ordered *)
let rec allBindingsOrdered (blockList:(int32 * Block.t) list) =
  match blockList with
    [] -> (true)
    | h :: t -> let (key, content) = h in 
        match Block.order content with
          (None) -> (false)
          | _ -> ( allBindingsOrdered (t) )

(* will return 3 values, the ancestor if there is one, or a flag indication to continue tracing ancestor *)
let rec findAncestor (label:int32) (b:Block.t) (parents:int list) (m:dBlockMap) =
  match parents with
    [] -> (label, b, false)
    | h :: t ->
      let parentLabel = Int32.of_int (List.hd parents) in
       try 
      let parent = Int32Map.find (parentLabel) m in
      if (Block.traced parent = true) then
        (label, b, false)
      else if (Block.psucc parent == Some (Int32.to_int label)) && (Block.order parent == None) then
        (parentLabel, parent, true)
      else
        findAncestor (label) (b) (t) (m)
       with Not_found -> raise (Invalid_argument ("in findAncestor caused by " ^ (Int32.to_string parentLabel))) 
;;
     
let rec traceAncestor (label:int32) (b:Block.t) (m:dBlockMap) =
  let (lbl, blk, cont) = findAncestor (label) (b) (Block.parents b) (m) in
  let newM = Int32Map.add (lbl) (Block.update_traced true blk) m in
  match cont with
    true -> traceAncestor (lbl) (blk) (newM)
    | false -> (lbl, blk);;
        
(* Same as findAncestor, except that this one will pick next available successor *)
let rec findAvailableSuccessor (label:int32) (b:Block.t) (succs:int list) (m:dBlockMap) 
(retBlock:Block.t) =
  match succs with
    [] -> (label, b, false)
    | h :: t ->
      let succLabel = Int32.of_int (List.hd succs) in
      let succ = 
        if (succLabel = Block.retLabel32) then (retBlock )
        else (Int32Map.find (succLabel) m )
      in
        if (Block.order succ = None) then
          (succLabel, succ, true)
        else
          findAvailableSuccessor (label) (b) (t) (m) (retBlock)
;;

let findSuccessor (label:int32) (b:Block.t) (succs:int list) (m:dBlockMap) (retBlock:Block.t) =
  let psucc = (Block.psucc b) in
	match psucc with
	| Some psuccLabel -> let psuccBlock = 
		    if (psuccLabel = (Block.retLabel)) then (retBlock)
			  else (Int32Map.find (Int32.of_int psuccLabel) m)
			in
			  if (Block.order psuccBlock = None) then ((Int32.of_int psuccLabel), psuccBlock, true)
				else findAvailableSuccessor (label) (b) (succs) (m) (retBlock)
	| None -> findAvailableSuccessor (label) (b) (succs) (m) (retBlock);;

let rec traceSuccessor (label:int32) (b:Block.t) (m:dBlockMap) (ord:int) 
(retBlock:Block.t) =
  let (lbl, blk, found) = findSuccessor (label) (b) (Block.succs b) (m) (retBlock) in
  let newM = 
    if (label = Block.retLabel32) then (m)
    else (Int32Map.add (label) (Block.update_order (ord) (Block.update_traced false b)) 
		  m )
  in
  let newRet = 
    if (label = Block.retLabel32) then 
      Block.update_order (ord) (Block.update_traced false retBlock)
    else 
      (retBlock)
  in
    if (found) then traceSuccessor (lbl) (blk) (newM) (ord + 1) (newRet)
    else (newM, ord + 1, newRet);;

(* Iterate through the bindings *)
let rec pickUnorderedBlock (blockList:(int32 * Block.t) list) (m:dBlockMap) (ord:int) 
(retBlock:Block.t) =
  match blockList with
    [] -> (m, retBlock, ord)
    | h :: t -> 
      let (label, content) = h in
      let content = (Int32Map.find (label) m) in
        if (Block.order content = None) then
          let (ancestorLabel, ancestorBlock) = traceAncestor (label) (content) (m) in
          let (newM, newOrd, newRet) = traceSuccessor (ancestorLabel) (ancestorBlock) (m) (ord) (retBlock) in
            pickUnorderedBlock (t) (newM) (newOrd) (newRet)
        else
          pickUnorderedBlock (t) (m) (ord) (retBlock);;
   
let rec pickOrder_rec (m:dBlockMap) (retBlock:Block.t) (order:int) =
  let allBindings = Int32Map.bindings (m) in
  let (newM, retBlock, newOrder) = pickUnorderedBlock (allBindings) (m) (order) (retBlock) in
  let break = allBindingsOrdered (Int32Map.bindings (newM)) in
  match break with
    true -> (newM, retBlock, newOrder)
    | false -> pickOrder_rec (newM) (retBlock) (newOrder);;

let pickOrder (m:dBlockMap) (retBlock:Block.t) = 
	let (newM, newRet, _) = pickOrder_rec (m) (retBlock) (0) in (newM, newRet)
