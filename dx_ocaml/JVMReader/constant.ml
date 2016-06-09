module ClassConstant = struct
    type t = { idx: int; name: string };;
    let idx = fun t -> t.idx;;
    let name = fun t -> t.name;;
    let to_string = fun t -> "Class " ^ string_of_int (t.idx) ^ ":" ^ t.name;;
    let create = fun idx -> {idx = idx; name = ""};;
    let update_ref = fun name t -> {t with name=name};;
    let empty = {idx = -1; name = ""};;
  end;;

module NATConstant = struct
    type t = { name_idx: int; desc_idx: int; name: string; desc: string };;
    let name_idx = fun t -> t.name_idx;;
    let desc_idx = fun t -> t.desc_idx;;
    let name = fun t -> t.name;;
    let desc = fun t -> t.desc;;
    let to_string = fun t ->  "NameAndType "^t.name ^ ":" ^ t.desc;;
    let create = fun name_idx desc_idx ->
      { name_idx = name_idx; desc_idx = desc_idx; name = ""; desc = ""};;
    let update_ref = fun name desc t -> {t with name=name; desc=desc};;
    let empty = { name_idx = -1; desc_idx = -1; name = ""; desc = ""};;
  end;;

module FMIConstant = struct
    type refType = FieldRef | MethodRef | InterfaceMethodRef;;
    type t = {fmi_type:refType; class_idx: int; nat_idx: int; classConstant: ClassConstant.t;
              natConstant: NATConstant.t };;
    let fmi_type = fun t -> t.fmi_type;;
    let class_idx = fun t -> t.class_idx;;
    let nat_idx = fun t -> t.nat_idx;;
    let classConstant = fun t -> t.classConstant;;
    let natConstant = fun t -> t.natConstant;;
    let to_string = fun t -> let nat = t.natConstant in
                             let cl = t.classConstant in
                             let fmi_name = match t.fmi_type with FieldRef -> "FieldRef"
                                                                | MethodRef -> "MethodRef" | InterfaceMethodRef -> "InterfaceMethodRef" in
                             fmi_name ^ " " ^ (ClassConstant.name cl) ^ " " ^
                               (NATConstant.name nat) ^ ":" ^ (NATConstant.desc nat);;
    let create = fun fmi_type class_idx nat_idx -> {fmi_type = fmi_type;  class_idx = class_idx;
                                                    nat_idx = nat_idx; classConstant = ClassConstant.empty; natConstant = NATConstant.empty };;
    let update_ref = fun cc nc t -> {t with classConstant = cc; natConstant = nc};;
  end;;

module StringConstant = struct
    type t = { idx: int; str: string };;
    let idx = fun t -> t.idx;;
    let str = fun t -> t.str;;
    let to_string = fun t -> "String : "^t.str;;
    let create = fun idx -> {idx= idx; str=""};;
    let update_ref = fun str t -> {t with str = str};;
  end

module ValueConstant = struct
    type t = int;;
    let to_string = string_of_int;;
    let create = fun t -> t;;
    let value = fun t -> t;;
  end

module UTF8Constant = struct
    type t = { length: int; bytes: int list; utf_string: string };;
    let length = fun t -> t.length;;
    let bytes = fun t -> t.bytes;;
    let utf_string = fun t -> t.utf_string;;
    let to_string = fun t -> "UTF8 length:" ^ string_of_int(t.length) ^ " string=" ^ t.utf_string;;
    let create = fun length bytes utf_string ->
      { length = length; bytes = bytes; utf_string = utf_string };;
  end

type t =
  Class of ClassConstant.t
  | NAT of NATConstant.t
  | FMI of FMIConstant.t
  | String of StringConstant.t
  | Value of ValueConstant.t
  | UTF8 of UTF8Constant.t;;

let to_string = fun t -> match t with
                         | Class x -> ClassConstant.to_string x
                         | NAT x -> NATConstant.to_string x
                         | FMI x -> FMIConstant.to_string x
                         | String x -> StringConstant.to_string x
                         | Value x -> ValueConstant.to_string x
                         | UTF8 x -> UTF8Constant.to_string x;;

let newConstantClass = fun classIdx -> Class (ClassConstant.create classIdx);;
let newConstantNat = fun name_idx desc_idx -> NAT (NATConstant.create name_idx desc_idx);;
let newConstantFieldRef = fun class_idx nat_idx -> FMI (FMIConstant.create FMIConstant.FieldRef class_idx nat_idx);;
let newConstantMethodRef = fun class_idx nat_idx -> FMI (FMIConstant.create FMIConstant.MethodRef class_idx nat_idx);;
let newConstantInterfaceMethodRef = fun class_idx nat_idx -> FMI (FMIConstant.create FMIConstant.InterfaceMethodRef class_idx nat_idx);;
let newConstantString = fun str_id -> String (StringConstant.create str_id);;
let newConstantValue = fun value -> Value (ValueConstant.create value);;
let newConstantUtf = fun length bytes str -> UTF8 (UTF8Constant.create length bytes str);;

let getConstantUtf = fun id m -> let c = m (id) in
                                 match c with
                                 | UTF8 arg -> arg
                                 | _ -> raise (Invalid_argument "attempting to get a UTF argument from non-UTF constant");;

let getConstantClass = fun id m -> let c = m (id) in
                                   match c with
                                   | Class arg -> arg
                                   | _ -> raise (Invalid_argument "attempting to get a Class argument from non-Class constant");;

let getConstantValue = fun id m -> let c = m (id) in
                                   match c with
                                   | Value arg -> arg
                                   | _ -> raise (Invalid_argument "attempting to get a Value argument from non-Value constant");
