open Utility;;
open Map_type;;
open Constant;;

type t = {startPC : int; endPC : int; handlerPC : int; catchType : ClassConstant.t};;
let startPC = fun t -> t.startPC;;
let endPC = fun t -> t.endPC;;
let handlerPC = fun t -> t.handlerPC;;
let catchType = fun t -> t.catchType;; 
let startPC32 = fun t -> Int32.of_int t.startPC;;
let endPC32 = fun t -> Int32.of_int t.endPC;;
let handlerPC32 = fun t -> Int32.of_int t.handlerPC;;
let create = fun startPC endPC handlerPC catchType -> 
	{startPC=startPC; endPC=endPC; handlerPC=handlerPC; catchType=catchType};;

(* Reading handlers from java bytecodes *)    
let rec read_handler_rec (i:int) (n:int) (handler_list : t list) (pool : constantMap) (ic:in_channel)=
  match (i >= n) with
	| true -> handler_list
	| false ->
  	let start_pc = readShort(ic) in
    let end_pc = readShort(ic) in
    let handler_pc = readShort(ic) in
    let catch_type_id = readShort(ic) in
    let catch_type_const = Int32Map.find (Int32.of_int catch_type_id) (pool) in
    let catch_type = (match catch_type_const with
      Class x -> x
      | _ -> raise (Invalid_argument ("Index " ^ string_of_int(catch_type_id) ^ " is not a class constant"))) in
    let newList = ({startPC = start_pc; endPC = end_pc; handlerPC = handler_pc; catchType = catch_type} :: handler_list)
    in read_handler_rec (i+1) (n) (newList) (pool) (ic) 
;;

let read_handler (pool : constantMap) (ic:in_channel) =
  let n = readShort(ic) in
  let handlers = read_handler_rec (1) (n) ([]) (pool) (ic) in
    (n, handlers)
;;    
(* End of reading handlers from java bytecodes *)

let to_string (item : t) =
  "S=" ^ (string_of_int (item.startPC)) ^ "; E=" ^ (string_of_int (item.endPC)) ^ "; H=" ^ (string_of_int (item.handlerPC)) ^ "; T=" ^  (
  match ClassConstant.idx item.catchType with 
    -1 -> "Object"
    | _ -> ClassConstant.name item.catchType)
;;

let rec print_handlers (handlers : t list) (oc : out_channel) =
  match handlers with
    [] -> Printf.fprintf oc "empty handlers\n"
    | h :: [] -> Printf.fprintf oc "%s\n" (to_string (h))
    | h :: t -> (Printf.fprintf oc "%s\n" (to_string (h)); print_handlers (t) (oc))
;;

let handlers_for (allHandlers : t list ) (pc : int) =
  let handler_filter (item : t) = ((item.startPC <= pc) && (item.endPC > pc)) in
    List.filter (handler_filter) (allHandlers)
;;    
