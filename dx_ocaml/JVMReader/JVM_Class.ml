open Utility;;
open Map_type;;
open Constant;;

type interfaceMap = int Int32Map.t;;
type fieldMap = int Int32Map.t;;
type attributeMap = int Int32Map.t;;

type t = {magic : int; major_version : int; minor_version : int;
          constant_count: int; constant_pool : constantMap; access_flag : int;
          this_class : int; super_class : int;
          interfaces_count : int; interfaces : interfaceMap;
          fields_count : int; fields : fieldMap;
          methods_count : int; methods : JVM_Method.methodMap;
          attributes_count : int; attributes : attributeMap};;

let constant_pool = fun t -> t.constant_pool;;
let methods = fun t -> t.methods;;

let get_method = fun name desc t ->
  StringMap.find (desc) (StringMap.find (name) t.methods);;

let get_class_name = fun t ->
  let str = ClassConstant.name (getConstantClass (t.this_class)
                                                 (fun id -> Int32Map.find (Int32.of_int id) t.constant_pool)) in
  str
;;

let get_methods_key = fun t ->
  StringMap.fold (fun k v l -> StringMap.fold (fun k' v' l' ->
                                               (k, k')::l') v l) t.methods [];;

let read (ic:in_channel) =
  let magic = readInt (ic) in
  let major_version = readShort (ic) in
  let minor_version = readShort (ic) in
  let (constant_count, pool) = Constant_parsing.read_and_parse_constant (ic) in
  let access_flag = readShort (ic) in
  let this_class = readShort (ic) in
  let super_class = readShort (ic) in
  let interfaces_count = readShort (ic) in (* TODO : for now it will always be 0 *)
  let interfaces = Int32Map.empty in
  let fields_count = readShort (ic) in (* TODO : for now it will always be 0 *)
  let fields = Int32Map.empty in
  let (methods_count, methods) = JVM_Method.reads (pool) (ic) in
  let attributes_count = readShort (ic) in (* TODO : for now it will always be 0 *)
  let attributes = Int32Map.empty in
  {magic = magic; major_version = major_version; minor_version = minor_version;
   constant_count=constant_count; constant_pool=pool; access_flag=access_flag;
   this_class = this_class; super_class = super_class;
   interfaces_count = interfaces_count; interfaces = interfaces;
   fields_count = fields_count; fields = fields;
   methods_count = methods_count; methods = methods;
   attributes_count = attributes_count; attributes = attributes};;
