open Compiler_meta;;
open Constant;;
open Utility;;
open Map_type;;

type utf_type = OneByte | TwoByte | ThreeByte (*| Supplementary *);;

(* no checking for surrogate pairs for now, as such the supplementary is
  not checked. Also to be more precise we should check for invalid
  format, but for now assume that we have correct Java strings.
  Further restriction, we only care about the 8 bit characters*)
let checkUTFcase (byte:int) =
  match ((byte) land (hex_string_to_int "80") ) with
    0 -> (OneByte)
  | _ -> (let pattern = (hex_string_to_int "C0") in
          match ((byte) land (pattern)) with
            pattern -> (TwoByte));;

let rec read_utf8 (i:int) (length:int) (bytes:int list) (lst_len:int) (ic:in_channel) =
  let firstByte = readByte(ic) in
  match checkUTFcase (firstByte) with
    OneByte -> (let chr = ((firstByte) land (hex_string_to_int "7F")) in
                let newLst = (chr :: bytes) in
                match ((i + 1) >= length) with
                  true -> (newLst, lst_len + 1)
                | false -> read_utf8 (i+1) (length) (newLst) (lst_len + 1) (ic))
  | TwoByte -> (let secondByte = readByte(ic) in
                let firstChr = ((firstByte) land (hex_string_to_int "1F")) in
                let secondChr = ((secondByte) land (hex_string_to_int "3F")) in
                let newLst = (((firstChr lsl 6)+ secondChr) :: bytes) in
                match ((i + 1) >= length) with
                  true -> (newLst, lst_len + 2)
                | false -> read_utf8 (i+2) (length) (newLst) (lst_len + 1) (ic))
  | ThreeByte -> (let secondByte = readByte(ic) in
                  let thirdByte = readByte(ic) in
                  let firstChr = ((firstByte) land (hex_string_to_int "F")) in
                  let secondChr = ((secondByte) land (hex_string_to_int "3F")) in
                  let thirdChr = ((thirdByte) land (hex_string_to_int "3F")) in
                  let newLst = (((firstChr lsl 12) + (secondChr lsl 6) + thirdChr) :: bytes) in
                  match ((i + 3) >= length) with
                    true -> (newLst, lst_len + 1)
                  | false -> read_utf8 (i+3) (length) (newLst) (lst_len + 1) (ic))
;;

let rec convert_utf8_rec (bytes:int list) (str:string) =
  match bytes with
    [] -> (str)
  | h :: t -> (let newStr = (Char.escaped (Char.chr h)) ^ str in
               convert_utf8_rec (t) (newStr))
;;

let rec read_and_parse_constant_rec (i:int) (n:int) (pool:constantMap) (ic:in_channel)=
  match (i < n) with
  | true ->
     let inputTag = readByte(ic) in
     let newConst = (match inputTag with
                       1 -> let inLength = readShort(ic) in
                            let (inBytes, lst_len) = read_utf8 (0) (inLength) ([]) (0) (ic) in
                            let resString = convert_utf8_rec (inBytes) ("") in
                            newConstantUtf inLength inBytes resString
                     | 3 -> let inValue = readInt(ic) in newConstantValue inValue
                     | 7 -> let inValue = readShort(ic) in newConstantClass inValue
                     | 8 -> let inValue = readShort(ic) in newConstantString inValue
                     | 9 -> let value1 = readShort(ic) in
                            let value2 = readShort(ic) in newConstantFieldRef value1 value2
                     | 10 -> let value1 = readShort(ic) in
                             let value2 = readShort(ic) in newConstantMethodRef value1 value2
                     | 11 -> let value1 = readShort(ic) in
                             let value2 = readShort(ic) in newConstantInterfaceMethodRef value1 value2
                     | 12 -> let value1 = readShort(ic) in
                             let value2 = readShort(ic) in newConstantNat value1 value2
                     | _ as tag -> raise (Invalid_argument ("Tag " ^ string_of_int(tag) ^ " not yet implemented")))
     in
     let newPool = Int32Map.add (Int32.of_int(i)) (newConst) (pool) in
     read_and_parse_constant_rec (i+1) (n) (newPool) (ic)
  | false -> (pool)
;;

let get_utf8_from_pool (pool : constantMap) (key:int) =
  match (Int32Map.find (Int32.of_int key) pool) with
    UTF8 y -> (UTF8Constant.utf_string y)
  | _ -> (raise (Invalid_argument "name index have to point to UTF-8 constant"))
;;

let rec fix_pointing_utf8_rec (pool : constantMap) (bindings:(int32 * Constant.t) list) =
  match bindings with
    [] -> pool
  | h :: t -> (let (key, value) = h in
               match value with
                 Class x -> (let utf_value = get_utf8_from_pool (pool) (ClassConstant.idx x) in
                             let newConst = Class (ClassConstant.update_ref utf_value x) in
                             let newPool = (Int32Map.add (key) (newConst) (pool)) in
                             fix_pointing_utf8_rec (newPool) (t) )
               | String x -> ( let utf_value = get_utf8_from_pool (pool) (StringConstant.idx x) in
                               let newConst = String (StringConstant.update_ref utf_value x) in
                               let newPool = (Int32Map.add (key) (newConst) (pool)) in
                               fix_pointing_utf8_rec (newPool) (t) )
               | NAT x -> (let utf_value1 = get_utf8_from_pool (pool) (NATConstant.name_idx x) in
                           let utf_value2 = get_utf8_from_pool (pool) (NATConstant.desc_idx x) in
                           let newConst = NAT (NATConstant.update_ref utf_value1 utf_value2 x) in
                           let newPool = (Int32Map.add (key) (newConst) (pool)) in
                           fix_pointing_utf8_rec (newPool) (t) )
               | FMI _ (* will be dealt with later as we need finished class and NameAndType constant *)
               | UTF8 _
               | Value _ -> fix_pointing_utf8_rec (pool) (t)
              )
;;

let fix_pointing_utf8 (pool : constantMap) =
  fix_pointing_utf8_rec (pool) (Int32Map.bindings pool);;

let rec fix_fmi_rec (pool : constantMap) (bindings:(int32 * Constant.t) list) =
  match bindings with
    [] -> pool
  | h :: t -> (let (key, value) = h in
               match value with
                 FMI x -> (let class_value = (match (Int32Map.find (Int32.of_int (FMIConstant.class_idx x)) pool) with
                                                Class y -> y
                                              | _ -> (raise (Invalid_argument "name index have to point to a class constant"))) in
                           let nat_value = (match (Int32Map.find (Int32.of_int (FMIConstant.nat_idx x)) pool) with
                                              NAT y -> y
                                            | _ -> (raise (Invalid_argument "descriptor index have to point to a NAT constant"))) in
                           let newConst = FMI (FMIConstant.update_ref class_value nat_value x) in
                           let newPool = (Int32Map.add (key) (newConst) (pool)) in
                           fix_fmi_rec (newPool) (t)
                          )
               | Class _
               | String _
               | NAT _
               | UTF8 _
               | Value _ -> fix_fmi_rec (pool) (t)
              )
;;

let fix_fmi (pool : constantMap) =
  fix_fmi_rec (pool) (Int32Map.bindings pool);;

let read_and_parse_constant (ic:in_channel) =
  let n = readShort (ic) in
  let newPool = read_and_parse_constant_rec (1) (n) (Int32Map.empty) (ic) in
  let fixedPool1 = fix_pointing_utf8 (newPool) in
  let fixedPool2 = fix_fmi (fixedPool1) in
  (n, fixedPool2)
