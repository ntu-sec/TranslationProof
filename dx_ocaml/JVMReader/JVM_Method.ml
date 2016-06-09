open Utility;;
open Map_type;;
open Instructions.JVM;;
open Constant;;
open Attributes;;

type t = {access_flag:int; name:string; desc:string; attributes_count:int; 
  attributesMap : methodAttribute StringMap.t};; 

type methodMap = (t StringMap.t) StringMap.t;;

let read (pool : constantMap) (ic : in_channel) =
	let access_flag = readShort (ic) in
	let name_id = readShort (ic) in
	let desc_id = readShort (ic) in
	let name = UTF8Constant.utf_string (getConstantUtf (name_id) (fun id -> Int32Map.find (Int32.of_int id) pool)) in
	let desc = UTF8Constant.utf_string (getConstantUtf (desc_id) (fun id -> Int32Map.find (Int32.of_int id) pool)) in
	let attributes_count = readShort (ic) in
	let attributes = read_n (read_methodAttribute (pool)) (ic) (attributes_count) in
    {access_flag=access_flag; name=name; desc=desc; attributes_count = attributes_count; 
		attributesMap=list_to_StringMap (attributes)};;

let rec reads_rec (n : int) (pool : constantMap) (ic : in_channel) =
	match n > 0 with
	| true -> ( 
			let m = read (pool) (ic) in
			let methodPool = (reads_rec (n-1) (pool) (ic)) in
			match StringMap.mem (m.name) (methodPool) with
			| true -> let subMap = StringMap.find (m.name) (methodPool) in
			  StringMap.add (m.name) (StringMap.add (m.desc) (m) subMap) (methodPool)
			| false -> StringMap.add (m.name) (StringMap.add (m.desc) (m) StringMap.empty) (methodPool)
		) 
	| false -> StringMap.empty;;

let reads (pool : constantMap) (ic : in_channel) =
	let n_methods = readShort (ic) in
	let result = reads_rec (n_methods) (pool) (ic) in
	  (n_methods, result);; 

let name = fun t -> t.name;;
let desc = fun t -> t.desc;;

let insnList = fun t -> let c = get_CodeAttribute(StringMap.find ("Code") t.attributesMap) in
  CodeAttribute.insnList c;;

let insnMap = fun t -> let c = get_CodeAttribute(StringMap.find ("Code") t.attributesMap) in
  CodeAttribute.insnMap c;;

let maxLabel = fun t -> let c = get_CodeAttribute(StringMap.find ("Code") t.attributesMap) in
  CodeAttribute.maxLabel c;;

let maxStack = fun t -> let c = get_CodeAttribute(StringMap.find ("Code") t.attributesMap) in
  CodeAttribute.maxStack c;;

let handlers = fun t -> let c = get_CodeAttribute(StringMap.find ("Code") t.attributesMap) in
  CodeAttribute.handlers c;;

let localN = fun t -> let c = get_CodeAttribute(StringMap.find ("Code") t.attributesMap) in
  CodeAttribute.localN c;;

let isStatic = fun t -> let flag = t.access_flag in
  match flag land 8 with
  | 0 -> false
  | 8 -> true;;
