open Map_type;;
open Utility;;
open Constant;;
open Instructions.JVM;;

module LineNumberTableAttribute = struct
  module Item = struct
		type t = {start_pc:int; line_number:int};;
		let read (ic:in_channel) = 
			let start_pc = readShort (ic) in
			let line_number = readShort (ic) in
			  {start_pc=start_pc; line_number=line_number};;
    end;;
	  type t = {count:int; line_number_table:Item.t list};;
		let read (ic:in_channel) = 
		  let count = readShort (ic) in
		  let line_number_table = read_n (Item.read) (ic) (count) in
			  {count=count; line_number_table=line_number_table};;
	end;;
  
module LocalVariableTableAttribute = struct
	module Item = struct
	  type t = {start_pc:int; length:int; name_index:int;
		  descriptor_index:int; index:int};;
    let read (ic:in_channel) = 
			let start_pc = readShort (ic) in
			let length = readShort (ic) in
			let name_index = readShort (ic) in
			let descriptor_index = readShort (ic) in
			let index = readShort (ic) in
			  {start_pc=start_pc; length=length; name_index=name_index;
				descriptor_index=descriptor_index; index=index};;
    end;;
	type t = {count:int; local_variable_table:Item.t list};;
  let read (ic:in_channel) =
		let count = readShort (ic) in
		let local_variable_table = read_n (Item.read) (ic) (count) in
		  {count=count; local_variable_table=local_variable_table};;
end;;

module LocalVariableTypeTableAttribute = struct
	module Item = struct
		type t = {start_pc:int; length:int; name_index:int;
		  signature_index:int; index:int};;
    let read (ic:in_channel) = 
			let start_pc = readShort (ic) in
			let length = readShort (ic) in
			let name_index = readShort (ic) in
			let signature_index = readShort (ic) in
			let index = readShort (ic) in
			  {start_pc=start_pc; length=length; name_index=name_index;
				  signature_index=signature_index; index=index};;
  end;;
	type t = {count:int; local_variable_type_table:Item.t list};;
  let read (ic:in_channel) =
		let count = readShort(ic) in
		let local_variable_type_table = read_n (Item.read) (ic) (count) in
		  {count=count; local_variable_type_table=local_variable_type_table};;
end;; 

module StackMapTableAttribute = struct
	module Item = struct
		module VerificationTypeInfo = struct
			type t =
				| Top_variable_info of int
				| Integer_variable_info of int
				| Float_variable_info of int
				| Null_variable_info of int
				| UninitializedThis_variable_info of int
				| Object_variable_info of (int * int)
				| Uninitialized_variable_info of (int * int)
				| Long_variable_info of int
				| Double_variable_info of int;;

      let read (ic:in_channel) =
			  let tag = readByte(ic) in
				match tag with
				| 0 -> Top_variable_info 0
				| 1 -> Integer_variable_info 1
				| 2 -> Float_variable_info 2
				| 3 -> Double_variable_info 3
				| 4 -> Long_variable_info 4
				| 5 -> Null_variable_info 5
				| 6 -> UninitializedThis_variable_info 6
				| 7 -> Object_variable_info (7, readShort (ic))
				| 8 -> Uninitialized_variable_info (8, readShort (ic))
				| _ as d -> raise (Invalid_argument ((string_of_int d) ^ " is not a valid tag for verification type info"));;
		end;;

    module AppendFrame = struct
		  type t = {offset_delta:int; locals:VerificationTypeInfo.t list};;
      let read (tag:int) (ic:in_channel) =
				let offset_delta = readShort (ic) in
				let locals = read_n (VerificationTypeInfo.read) (ic) (tag-251) in
				  {offset_delta=offset_delta; locals=locals};;
    end;;
      
		module FullFrame = struct
			type t = {offset_delta:int; localN:int; locals:VerificationTypeInfo.t list;
			  stack_itemsN:int; stack:VerificationTypeInfo.t list};;
      let read (ic:in_channel) = 
				let offset_delta = readShort (ic) in
				let localN = readShort (ic) in
				let locals = read_n (VerificationTypeInfo.read) (ic) (localN) in
				let stack_itemsN = readShort (ic) in
				let stack = read_n (VerificationTypeInfo.read) (ic) (stack_itemsN) in
				  {offset_delta=offset_delta; localN=localN; locals=locals;
					  stack_itemsN=stack_itemsN; stack=stack};;
    end;;
      
		type t = 
			| Same_frame of int
			| Same_locals_1_stack_item_frame of (int * VerificationTypeInfo.t)
			| Same_locals_1_stack_item_frame_extended of (int * int * VerificationTypeInfo.t)
			| Chop_frame of (int * int)
			| Same_frame_extended of (int * int)
			| Append_frame of (int * AppendFrame.t)
			| Full_frame of (int * FullFrame.t);;
				
		let read (ic:in_channel) =
			let tag = readByte (ic) in
			if tag >= 0 && tag < 64 then
				Same_frame (tag)
			else if tag >= 64 && tag < 128 then
				let stack = VerificationTypeInfo.read (ic) in
				Same_locals_1_stack_item_frame (tag, stack)
			else if tag = 247 then
				let offset_delta = readShort (ic) in
				let stack = VerificationTypeInfo.read (ic) in
				Same_locals_1_stack_item_frame_extended (tag, offset_delta, stack)
			else if tag > 247 && tag < 251 then
				let offset_delta = readShort (ic) in
				Chop_frame (tag, offset_delta)
			else if tag = 251 then
				let offset_delta = readShort (ic) in
				Same_frame_extended (tag, offset_delta)
			else if tag > 251 && tag < 255 then
				Append_frame (tag, AppendFrame.read (tag) (ic))
			else if tag = 255 then
				Full_frame (tag, FullFrame.read (ic))
			else raise (Invalid_argument ("Not a valid tag : " ^ string_of_int (tag)));;
	end;;
	type t = {count:int; entries:Item.t list};;
  let read (ic:in_channel) = 
		let count = readShort (ic) in
		let entries = read_n (Item.read) (ic) (count) in
		  {count=count; entries=entries};;
end;;

module Annotation = struct
  type element_value_pair = {_name_index:int; _element_value:element_value}
	and element_value = {tag:char; v:value}
	and  value =
				| ConstValue of int
				| EnumConstValue of (int * int)
				| ClassInfo of int
				| Annotation of t
				| Array of element_value
	and		
	t = {type_index:int; name_index:int; element_count:int; 
		  element_value_pairs:element_value_pair list};;

  let rec read_element_value (ic:in_channel) =
	  let tag = readChar (ic) in
		let v = match tag with
		| 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' | 's' ->
			ConstValue (readShort (ic))
		| 'e' -> let type_name_idx = readShort(ic) in
		     let const_name_idx = readShort(ic) in EnumConstValue (type_name_idx, const_name_idx)
		| 'c' -> ClassInfo (readShort (ic))
		| '@' -> Annotation (read (ic))
		| '[' -> Array (read_element_value (ic)) 
		| _ -> raise (Invalid_argument "Not a valid tag for element_value") in
		  {tag=tag; v=v}
   and read_element_value_pair (ic:in_channel) =
		let _name_index = readShort (ic) in
		let _element_value = read_element_value (ic:in_channel) in
		  {_name_index=_name_index; _element_value=_element_value}
  and read (ic:in_channel) =
		let type_index = readShort (ic) in
		let name_index = readShort (ic) in
		let element_count = readShort (ic) in
		let element_value_pairs = read_n (read_element_value_pair) (ic) (element_count) in
		  {type_index = type_index; name_index = name_index; element_count = element_count;
			  element_value_pairs = element_value_pairs};;
end;;

module TypeAnnotation = struct
	module LocalvarTarget = struct
		module Table = struct
			type t = {start_pc:int; length:int; index:int};;
			let read (ic:in_channel) =
			  let start_pc = readShort (ic) in
				let length = readShort (ic) in
				let index = readShort (ic) in
				  {start_pc=start_pc; length=length; index=index};;
    end;;

		type t = {count:int; table:Table.t list};;
    let read (ic:in_channel) =
			let count = readShort (ic) in
			let table = read_n (Table.read) (ic) (count) in
			  {count=count; table=table};;
  end;;

	type target_info = 
		| Type_parameter_target of int
		| Supertype_index of int 
		| Type_parameter_bound_target of (int * int)
		| Empty_target of unit
		| Formal_parameter_target of int
		| Throws_target of int
		| Localvar_target of LocalvarTarget.t
		| Catch_target of int
		| Offset_target of int
		| Type_argument_target of (int * int);;
  let read_target_info (target_type:int) (ic:in_channel) =
		match target_type with
		| 0 
		| 1 -> Type_parameter_target (readByte (ic))
		| 16 -> Supertype_index (readShort (ic))
		| 17
		| 18 -> let type_parameter_index = readByte (ic) in
		    let bound_index = readByte (ic) in
				  Type_parameter_bound_target (type_parameter_index, bound_index)
		| 19 | 20 | 21 -> Empty_target ()
		| 22 -> Formal_parameter_target (readByte(ic))
		| 23 -> Throws_target (readShort(ic))
		| 64 | 65 -> Localvar_target (LocalvarTarget.read (ic))
		| 66 -> Catch_target (readShort (ic))
		| 67 | 68 | 69 | 70 -> Offset_target (readShort (ic))
		| 71 | 72 | 73 | 74 | 75 -> let offset = readShort (ic) in
		    let type_argument_index = readByte (ic) in
				 Type_argument_target (offset, type_argument_index)
		| _ -> raise (Invalid_argument "Invalid value for target_info");;
		
  module Path = struct
		type t = {type_path_kind:int; type_argument_index:int};;
    let read (ic:in_channel) = 
			let type_path_kind = readByte(ic) in
			let type_argument_index = readByte (ic) in
			  {type_path_kind = type_path_kind; type_argument_index = type_argument_index};;
  end;;
  type type_path = {length:int; paths:Path.t list};;
  let read_target_path (ic:in_channel) =
		let length = readByte (ic) in
		let paths = read_n (Path.read) (ic) (length) in
		  {length=length; paths=paths};;

	type t = {target_type:int; target_info_item:target_info; target_path:type_path;
	  type_index:int; element_value_count:int; 
		element_value_pairs:Annotation.element_value_pair list};;
	let read (ic:in_channel) =
		let target_type = readShort(ic) in
		let target_info_item = read_target_info (target_type) (ic) in
		let target_path = read_target_path (ic) in
		let type_index = readShort (ic) in
		let element_value_count = readShort (ic) in
		let element_value_pairs = read_n (Annotation.read_element_value_pair) (ic) (element_value_count) in
		  {target_type=target_type; target_info_item=target_info_item; target_path=target_path;
			  type_index=type_index; element_value_count=element_value_count;
				element_value_pairs=element_value_pairs};;  
end;;

module RTAAttribute = struct
	type t = {count:int; annotations:TypeAnnotation.t list};;
  let read (ic:in_channel) =
		let count = readShort(ic) in
		let annotations = read_n (TypeAnnotation.read) (ic) (count) in
		  {count=count; annotations=annotations};;
end;;

type codeAttribute = 
	| LineNumberTable of LineNumberTableAttribute.t
	| LocalVariableTable of LocalVariableTableAttribute.t
	| LocalVariableTypeTable of LocalVariableTypeTableAttribute.t
	| StackMapTable of StackMapTableAttribute.t
	| RuntimeTypeAnnotations of RTAAttribute.t;;
	
let read_codeAttribute (pool:constantMap) (ic:in_channel) = 
	let str_id = readShort (ic) in
	let _ = readInt(ic) in (* length : seems useless *)
 	let str = UTF8Constant.utf_string (getConstantUtf (str_id) (fun id -> Int32Map.find (Int32.of_int id) pool)) in
 	match str with
 	| "LineNumberTable" -> (str, LineNumberTable (LineNumberTableAttribute.read (ic)))
 	| "LocalVariableTable" -> (str, LocalVariableTable (LocalVariableTableAttribute.read (ic)))
  | "LocalVariableTypeTable" -> (str, LocalVariableTypeTable (LocalVariableTypeTableAttribute.read (ic)))
  | "StackMapTable" -> (str, StackMapTable (StackMapTableAttribute.read (ic)))
	| "RuntimeVisibleTypeAnnotations" 
	| "RuntimeInvisibleTypeAnnotations" -> ("RTA", RuntimeTypeAnnotations (RTAAttribute.read (ic)))
  | _ -> raise (Invalid_argument "Invalid attribute name")
;;

let read_code (pool : constantMap) (ic:in_channel) =
	let n = readInt(ic) in
	let arr = Array.create (n) (0) in
	let accept_byte arr i chr = arr.(i) <- (Char.code chr) in
	for i = 0 to (n - 1) do
		Scanf.fscanf ic "%c" (accept_byte arr i);
	done;
	(Parse_byte.construct_code (Array.to_list arr) (pool))
;;

module CodeAttribute = struct	
	type t = {localN:int; maxStack:int; maxLabel:int; 
  handlers_count:int; handlers:Handler.t list; 
	codesList:JVM_Instruction.t list; codesMap:JVM_Instruction.t Int32Map.t;
	attributes_count:int; attributesMap : codeAttribute StringMap.t};;
  
  let insnList = fun t -> t.codesList;;
  let insnMap = fun t -> t.codesMap;;
  let maxLabel = fun t -> t.maxLabel;;
  let maxStack = fun t -> t.maxStack;;
  let handlers = fun t -> t.handlers;;
  let localN = fun t -> t.localN;;
	
	let read (pool:constantMap) (ic:in_channel) =
		let maxStack = readShort (ic) in
		let localN = readShort (ic) in
	  let (maxLabel, jInsnList, jInsnMap) = read_code (pool) (ic) in
	  let (handlers_count, handlers) = Handler.read_handler (pool) (ic) in
	  let attributes_count = readShort (ic) in
		let attributesList = read_n (read_codeAttribute (pool)) (ic) (attributes_count) in
		let attributesMap = list_to_StringMap (attributesList) in
		  {localN=localN; maxStack=maxStack; maxLabel=maxLabel;
			handlers_count=handlers_count; handlers=handlers;
			codesList=jInsnList; codesMap=jInsnMap;
			attributes_count=attributes_count; attributesMap=attributesMap};;
end;;

module ExceptionsAttribute = struct
	type t = {count:int; exception_index_table:int list};;
	
	let read (ic:in_channel) = 
		let count = readShort (ic) in
		let exception_index_table = read_n (readShort) (ic) (count) in
		  {count=count; exception_index_table=exception_index_table};;
end;;

module ParamsAttribute = struct
	module Item = struct 
		type t = {name_index:int; access_flag:int};; 
    let read (ic:in_channel) = 
			let name_index = readShort (ic) in
			let access_flag = readShort (ic) in
			  {name_index=name_index; access_flag=access_flag};;
  end;; 
	type t = {count:int; parameters:Item.t list};;

  let read (ic:in_channel) =
		let count = readShort (ic) in
		let params = read_n (Item.read) (ic) (count) in
		  {count=count; parameters=params};;
end;;

module RPAAttribute = struct
	module ParameterAnnotation = struct
		type t = {count:int; annotations:Annotation.t list};;
		let read (ic:in_channel) = 
			let count = readShort (ic) in
			let annotations = read_n (Annotation.read) (ic) count in
			  {count=count; annotations=annotations};;
  end;;
	type t = {count:int; parameter_annotations:ParameterAnnotation.t list};;
  let read (ic:in_channel) = 
		let count = readByte(ic) in
		let parameter_annotations = read_n (ParameterAnnotation.read) (ic) (count) in
		  {count=count; parameter_annotations=parameter_annotations};;
end;;

module SyntheticAttribute = struct
	type t = unit;;
  let read (ic:in_channel) = ();;
end;;

module DeprecatedAttribute = struct
	type t = unit;;
  let read (ic:in_channel) = ();;
end;;

module SignatureAttribute = struct
	type t = {signature_index:int};;
  let read (ic:in_channel) = let signature_index = readShort(ic) in
	  {signature_index=signature_index};;
end;;

module RuntimeAnnotationsAttribute = struct
	type t = {count:int; annotations:Annotation.t list};;
  let read (ic:in_channel) =
		let count = readShort(ic) in
		let annotations = read_n (Annotation.read) (ic) (count) in
		  {count=count; annotations=annotations};;
end;;

module AnnotationDefaultAttribute = struct
	type t = Annotation.element_value;;
  let read = Annotation.read_element_value;;
end;;

type methodAttribute = 
	| Code of CodeAttribute.t
	| MethodParameters of ParamsAttribute.t
	| AnnotationDefault of AnnotationDefaultAttribute.t
	| Exceptions of ExceptionsAttribute.t
	| RuntimeParameterAnnotations of RPAAttribute.t
	| Synthetic of SyntheticAttribute.t
	| Deprecated of DeprecatedAttribute.t
	| Signature of SignatureAttribute.t
	| RuntimeAnnotations of RuntimeAnnotationsAttribute.t
	| RuntimeTypeAnnotations of RTAAttribute.t;;

let read_methodAttribute (pool:constantMap) (ic:in_channel) =
	let str_id = readShort(ic) in
	let _ = readInt(ic) in (* length : seems useless *)
	let str = UTF8Constant.utf_string (getConstantUtf (str_id) (fun id -> Int32Map.find (Int32.of_int id) pool)) in
	match str with
	| "Code" -> ("Code", Code (CodeAttribute.read (pool) (ic)))
	| "Exceptions" -> ("Exceptions", Exceptions (ExceptionsAttribute.read (ic)))
	| "MethodParameters" -> ("MethodParameters", MethodParameters (ParamsAttribute.read (ic)))
	| "RuntimeVisibleParameterAnnotations" 
	| "RuntimeInvisibleParameterAnnotations" -> ("RPA", RuntimeParameterAnnotations (RPAAttribute.read (ic)))
	| "AnnotationDefault" -> ("AnnotationDefault", AnnotationDefault (AnnotationDefaultAttribute.read (ic)))
	| "Synthetic" -> ("Synthetic", Synthetic (SyntheticAttribute.read (ic)))
	| "Deprecated" -> ("Deprecated", Deprecated (DeprecatedAttribute.read (ic)))
	| "Signature" -> ("Signature", Signature (SignatureAttribute.read (ic)))
	| "RuntimeVisibleAnnotations" 
	| "RuntimeInvisibleAnnotations" -> ("RuntimeAnnotations", RuntimeAnnotations (RuntimeAnnotationsAttribute.read (ic)))
	| "RuntimeVisibleTypeAnnotations"
	| "RuntimeInvisibleTypeAnnotations" -> ("RTA", RuntimeTypeAnnotations (RTAAttribute.read (ic)))
  | _ -> raise (Invalid_argument "Invalid attribute name")
;;

let get_CodeAttribute (attr:methodAttribute) =
	match attr with
	| Code c -> c
	| _ -> raise (Invalid_argument "attempting to get a code attribute from non CodeAttribute");;
