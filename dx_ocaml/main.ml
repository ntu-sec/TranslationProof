open Compiler_meta;;
open Map_type;;
open Instructions.JVM;;
open Instructions.DEX;;
open Constant;;
open Utility;;
open Cert_reader;;
open Certificate;;

let transform_reg_jun (dex_list:DEX_Instruction.t list) (jvm_method_cert:JVM_Method_cert.t)
                      (jvm_dex:(int list) Int32Map.t) (dex_jvm: int Int32Map.t)
                      (jInsnMap:JVM_Instruction.t Int32Map.t) (nArgs:int) =
  let (dex_regMap, dex_junMap) = List.fold_right (fun h (m1,m2) ->
                                                  let label32 = DEX_Instruction.label32 h in
                                                  (Region.add (label32) ([]) m1, Junction.add (label32) (None) m2)) dex_list
                                                 (Region.empty, Junction.empty) in
  (* translate junction map *)
  let jvm_junMap = JVM_Method_cert.jun_map jvm_method_cert in
  (*let jvm_junMap = JVM_BytecodeMethod_cert.jun_map (JVM_Method_cert.bytecodeMethod_cert jvm_method_cert) in*)
  let dex_junMap = Junction.fold (fun key v junMap -> match v with
                                                      | None -> junMap
                                                      | Some i -> let jList = Int32Map.find (Int32.of_int i) jvm_dex in
                                                                  let source_ins = Int32Map.find (key) jInsnMap in
                                                                  match JVM_Instruction.code source_ins with
                                                                  | Instructions.JVM.If -> let iList = Int32Map.find (key) jvm_dex in
                                                                                           (*Printf.printf "JUN: (%d):" (List.hd iList);
                              List.iter (fun h -> Printf.printf " %d" h) (jList);
                              Printf.printf "\n"; *)
                                                                                           Junction.add (Int32.of_int (List.hd iList))
                                                                                                        (Some (List.hd (List.rev jList))) junMap
                                                                  (* We need to get the last program point as it will be the first in the
                                list *)
                                                                  | _ -> raise (Invalid_argument ("Junction for non-if instruction "
                                                                                                  ^ (JVM_Instruction.to_string source_ins))) ) jvm_junMap dex_junMap in
  (* translate region map *)
  let jvm_regMap = JVM_Method_cert.reg_map jvm_method_cert in
  let dex_regMap = Region.fold (fun key v regMap ->
                                let jList = Int32Map.find key jvm_dex in
                                let newRegionList = List.fold_right (fun h l ->
                                                                     let translatedJ = Int32Map.find (Int32.of_int h) jvm_dex in translatedJ @ l) v [] in
                                match jList with
                                | [] -> regMap
                                | h :: t -> Region.add (Int32.of_int h) newRegionList regMap) jvm_regMap dex_regMap in

  (* Add the initial moving parameters *)
  let iterator = Utility.int_iterator (nArgs) in
  let new_dex_regMap = List.fold_right (fun i m -> Region.add (Int32.of_int i) ([]) m) iterator dex_regMap in
  let new_dex_junMap = List.fold_right (fun i m -> Junction.add (Int32.of_int i) (None) m) iterator dex_junMap in
  (new_dex_regMap, new_dex_junMap);;

let rec open_input_channels_fn (n:int) =
  match n > 0 with
  | true ->
     let (cert_fn, class_fn) = Scanf.scanf " %s" (fun str -> (str^".cert", str^".class")) in
     let input_channels_fn = open_input_channels_fn (n-1) in
     (cert_fn, class_fn)::(input_channels_fn)
  | false -> []

let get_channels_fn () =
  let file_n = Scanf.scanf "%d" (fun n -> n) in
  let input_channels_fn = open_input_channels_fn (file_n) in
  let out_fn = Scanf.scanf " %s" (fun str -> str) in
  (input_channels_fn, out_fn);;

let rec empty_jvm_dex (l : JVM_Instruction.t list) : (int list) Int32Map.t =
  match l with
  | [] -> Int32Map.empty
  | h :: t -> Int32Map.add (JVM_Instruction.label32 h) ([]) (empty_jvm_dex (t));;

let rec transformArgs (str: string) (len: int) =
  match str.[0] with
    'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' as chr ->
                                               (match (len - 1) <= 0 with
                                                  true -> (String.make 1 chr)
                                                | false -> (String.make 1 chr) ^ transformArgs (String.sub (str) (1) (len - 1)) (len - 1)
                                               )
    | '[' -> transformArgs (String.sub (str) (1) (len - 1)) (len - 1)
    | 'L' -> (let nextIndex = (String.index (str) (';')) + 1 in
              match nextIndex >= len with
                true -> "L"
              | false -> "L" ^ transformArgs (String.sub (str) (nextIndex) (len - nextIndex)) (len - nextIndex)
             )
    | _ as chr -> raise (Invalid_argument ("Invalid character for a method descriptor : " ^ Char.escaped (chr)))
;;

let transformRet (desc:string) =
  let close = String.index (desc) (')') in
  let length = String.length (desc) in
  let str = String.sub (desc) (close+1) (length - close - 1) in
  match str with
  | "V" | "B" | "C" | "D" | "F" | "I" | "J" | "S" | "Z" as str -> str
  | _ -> "L" (* for now we don't care about invalid characters yet *)
;;

let transform_desc (desc:string) : string =
  let close = String.index (desc) (')') in
  let str = String.sub (desc) (1) (close - 1) in
  let argsStr = match (String.length (str)) = 0 with
      false -> (transformArgs (str) (String.length (str)))
    | true -> "" in
  transformRet (desc) ^ argsStr;;


let common_program_translate (jInsnList:JVM_Instruction.t list) (maxLabel:int)
                             (handlers:Handler.t list) (localN:int) (lvt:MethodPolicy.t) (nArgs:int) (constStack:valTypeMap) =
  let (m, sb) = Start_block.parse (jInsnList) (maxLabel) (handlers) in
  let (m, bp, ts, ret) = Trace_parent_child.parse (jInsnList) (maxLabel) (localN) (lvt)
                                                  (m) (sb) (handlers) in
  let (m, max_registers) = Translate_java_program.parse (jInsnList) (m) (bp) (sb) (ts) (ret) (localN)
                                                        (lvt) (constStack) in
  let (m, ret) = Pick_order.pickOrder (m) (ret) in
  let (dex_list, dex_map, m, dexHandlers, jvm_dex, dex_jvm) =
    Consolidate_block.consolidateBlock (nArgs) (m) (ret) (handlers) (empty_jvm_dex jInsnList) in
  (dex_list, dex_map, m, dexHandlers, jvm_dex, dex_jvm, max_registers)
;;

let translate_cert (object_level:int) (c:JVM_Class.t) (m:JVM_Method.t)
                   (jvm_method_cert : JVM_Method_cert.t) (cert:JVM_Cert.t) (constStack:valTypeMap) =

  (* From this point onward, assume that we focus only on one method *)
  let lvt = JVM_Method_cert.policy (object_level) (jvm_method_cert)  in

  let jvm_method_desc = (JVM_Method_cert.descriptor jvm_method_cert) in
  let nArgs = Utility.countArgs_from_desc (jvm_method_desc) in

  (* JVM Simple Type Inference *)
  Printf.printf "Starting type inference for object level %d\n" (object_level);
  let (jInsnList, jInsnMap) = Type_inference.infer_type (c) (m) (cert) (object_level)
                                                        (JVM_Cert.default_level cert)in
  Printf.printf "Finished type inference\n";
  List.iter (fun h -> Printf.printf "%s\n" (JVM_Instruction.to_string h)) jInsnList;

  Printf.printf "Method policy :\n %s\n" (MethodPolicy.to_string lvt);

  (* do a program translation *)
  let maxLabel = JVM_Method.maxLabel m in
  let handlers = JVM_Method.handlers m in
  let maxStack = JVM_Method.maxStack m in
  let localN = JVM_Method.localN m in
  let isStatic = JVM_Method.isStatic m in
  let (dex_list, dex_map, m, dexHandlers, jvm_dex, dex_jvm, max_registers) =
    common_program_translate (jInsnList) (maxLabel) (handlers) (localN) (lvt) (nArgs)
                             (constStack) in

  let (new_dex_regMap, new_dex_junMap) = transform_reg_jun (dex_list) (jvm_method_cert)
                                                           (jvm_dex) (dex_jvm) (jInsnMap) (nArgs) in

  (* Constructing DEX certificate *)
  (* Type Map *)

  let defaultTopValue = Level.simple_ext_level (JVM_Cert.default_top_level cert) in
  let allRegisters = max_registers + nArgs + match isStatic with
                                             | false -> 1 | true -> 0 in

  let dexTypeMap = List.fold_right (fun h m ->
                                    Int32Map.add (Int32.of_int (DEX_Instruction.label h))
                                                 (DEX_typing_op.add_missing_registers (defaultTopValue) (allRegisters) (DEX_Instruction.typeInfo h)) m) dex_list Int32Map.empty in

  let starting_point = match (localN + maxStack) > (max_registers + 1) with
    | true -> (localN + maxStack) | false -> (max_registers + 1) in
  let iterator = Utility.int_iterator (nArgs) in
  let base_rt = RT_op.add_missing_registers (defaultTopValue) (allRegisters)
                                            (Certificate.base_rt_rec (localN) (lvt)) in
  let se = JVM_Cert.default_level cert in
  let new_dexTypeMap = List.fold_right (fun i m ->
                                        let current_rt = Certificate.RT_op.add (i + starting_point)
                                                                               (MethodPolicy.ka_at i lvt) (base_rt) in
                                        let base_cert = Certificate.DEX_ins_typing.create (se) (current_rt) in
                                        Int32Map.add (Int32.of_int i) (base_cert) m) iterator dexTypeMap in

  let lvl_pool = JVM_Cert.lvl_pool cert in
  let new_methodCert_item = DEX_Method_cert.Item.create (Level_pool.level lvl_pool object_level)
                                                        (lvt) (new_dexTypeMap) in
  Printf.printf "DEX Certificate\n%s" (DEX_BytecodeMethod_cert.to_string (DEX_Method_cert.Item.bytecodeMethod_cert new_methodCert_item));
  (new_dex_regMap, new_dex_junMap, new_methodCert_item)
;;

(* leaves room for optimization where we can just do the region and junction once,
  because they don't depend on the security level / policy *)
let translate_certs (c:JVM_Class.t) (m:JVM_Method.t)
                    (jvm_method_cert : JVM_Method_cert.t) (cert:JVM_Cert.t) (constStack:valTypeMap) =
  let lvl_pool = JVM_Cert.lvl_pool cert in
  let keys = Level_pool.get_id_list lvl_pool in
  List.fold_right (fun lvl (regMap, junMap, typeMap) ->
                   let (new_dex_regMap, new_dex_junMap, new_dexTypeMap) = translate_cert
                                                                            (lvl) (c) (m) (jvm_method_cert) (cert) (constStack) in
                   (new_dex_regMap, new_dex_junMap, (lvl, new_dexTypeMap)::typeMap)) keys
                  (Region.empty, Junction.empty, []);;

let translate_method (current_class:string) (current_name:string) (current_desc:string) (c:JVM_Class.t)
                     (cert:JVM_Cert.t) (dex_method_certs:((DEX_Method_cert.t StringMap.t) StringMap.t) StringMap.t) =
  Printf.printf "Start processing : %s %s %s\n" current_class current_name current_desc;
  let m = JVM_Class.get_method current_name current_desc c in

  (* rough flow for now : do a translation over and over again. This obviously open to
        optimization where we just need the final relation between program points *)
  let jvm_method_cert = JVM_Cert.method_cert current_class current_name current_desc cert in
  let jInsnList = JVM_Method.insnList m in
  let jInsnMap = JVM_Method.insnMap m in
  let constStack = StackValTypeAssigner.assignValType (jInsnList) (jInsnMap) in
  let (dex_regMap, dex_junMap, typesList) =
    translate_certs (c) (m) (jvm_method_cert) (cert) constStack in

  (* Finalizing the certificate *)
  let dex_method_desc = transform_desc (current_desc) in

  let class_name = "L" ^ (JVM_Class.get_class_name c) ^ ";" in
  let dex_method_str = (*"L"^class_name ^";"^*)current_name in
  let dex_typeMap = List.fold_right (fun (lvl, h) m -> Int32Map.add (Int32.of_int lvl) h m) typesList Int32Map.empty in
  let dex_method_cert = DEX_Method_cert.create (class_name) (dex_method_str) (dex_method_desc) (dex_regMap)
                                               (dex_junMap) (dex_typeMap) in

  let dex_method_certs =
    match StringMap.mem (class_name) (dex_method_certs) with
    | true -> ( let cn_map = StringMap.find (class_name) (dex_method_certs) in
                let new_cnMap = match StringMap.mem (dex_method_str) (cn_map) with
                  | true -> let subMap = StringMap.find (dex_method_str) (cn_map) in
                            StringMap.add (dex_method_str) (StringMap.add (current_desc) (dex_method_cert) subMap) cn_map
                  | false -> StringMap.add (dex_method_str) (StringMap.add (current_desc) (dex_method_cert)
                                                                           StringMap.empty) cn_map in
                StringMap.add (class_name) (new_cnMap) (dex_method_certs)
              )
    | false -> (StringMap.add (class_name) (StringMap.add (dex_method_str)
                                                          (StringMap.add (current_desc) (dex_method_cert) StringMap.empty) StringMap.empty)
                              dex_method_certs)
  in
  Printf.printf "Finished processing : %s %s\n" current_name current_desc;
  (dex_method_certs)
;;

let translate_class (class_fn:string) (cert_fn:string)
                    (dex_method_certs:((DEX_Method_cert.t StringMap.t) StringMap.t) StringMap.t) =

  (* reading JVM bytecode *)
  let class_channel = open_in_bin class_fn in
  let c = JVM_Class.read (class_channel) in
  close_in class_channel;

  (* reading certificate *)
  let cert_channel = open_in_bin cert_fn in
  let cert = Cert_reader.read_certificate (JVM_Class.get_class_name c) (cert_channel) in
  close_in cert_channel;

  let method_keys = JVM_Class.get_methods_key c in

  let dex_method_certs = List.fold_right (fun (current_name,current_desc) m ->
                                          translate_method (JVM_Class.get_class_name c) (current_name) (current_desc) (c) (cert) (m))
                                         method_keys dex_method_certs in

  let class_name = JVM_Class.get_class_name c in
  let class_name = "L" ^ class_name ^ ";" in
  Default_methods.DEX.add_current_class_methods (dex_method_certs) (class_name)
                                                (JVM_Cert.lvl_pool cert)
;;

(* assume for now size of instructions won't be more than 2^31 *)
let main () =
  let (input_channels_fn, oc_fn) = get_channels_fn () in

  let dex_method_certs = StringMap.empty in

  let dex_method_certs = List.fold_right (fun (cert_fn, class_fn) m ->
                                          translate_class (class_fn) (cert_fn) (m))
                                         input_channels_fn dex_method_certs in

  (* assume that there is always at least one certificate to glean the information from *)
  let (cert_hd, _)  = List.hd input_channels_fn in
  let cert_ch = open_in_bin cert_hd in
  let cert = Cert_reader.read_certificate ("") (cert_ch) in
  close_in cert_ch;

  let dex_method_certs = Default_methods.DEX.add_default_methods (dex_method_certs) (JVM_Cert.lvl_pool cert) in

  let dex_cert = DEX_Cert.create_cert (JVM_Cert.lvl_pool cert) (JVM_Cert.levels_rel cert)
                                      (dex_method_certs) (JVM_Cert.field_policy cert) (JVM_Cert.default_level cert)
                                      (JVM_Cert.default_top_level cert) in

  let oc = open_out_bin oc_fn in
  Printf.fprintf oc "%s" (DEX_Cert.to_byteString dex_cert);

  close_out oc;

  Printf.printf "Finished translating JVM certificates\n";

  exit 0;;

let _ = Printexc.print main ();;
