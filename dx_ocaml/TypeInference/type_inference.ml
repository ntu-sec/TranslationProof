open Instructions;;
open Instructions.JVM;;
open Map_type;;
open Certificate;;

let fix_result_one (label:int32) (st:StackType.t) (cert : JVM_Cert.t) (typeMap : StackType.t Int32Map.t) =
  match Int32Map.mem label typeMap with (* if ST for the succ already exists *)
  | false ->
     let newTypeMap = Int32Map.add (label) (st) typeMap in
     (true, newTypeMap)
  | true ->
     let succSt = Int32Map.find (label) (typeMap) in
     let (changeSt, newSt) = ST_op.lub_st (succSt) (st) (JVM_Cert.levels_rel cert) in
     let newTypeMap = Int32Map.add (label) (newSt) typeMap in
     (changeSt, newTypeMap);;

let debug_ch = open_out "debug.out";;

let rec fix_se_region (regList : int32 list) (se:Level.basic_level) (seMap:Level.basic_level Int32Map.t) (cert:JVM_Cert.t) =
  match regList with
  | [] -> (false, seMap)
  | label32 :: t -> let (b, newSeMap) = fix_se_region (t) (se) (seMap) (cert) in
                    match Int32Map.mem (label32) (seMap) with
                    | true -> (
                      let targetSe = Int32Map.find (label32) (seMap) in
                      let changeSe = Level.leq_ext (Level.simple_ext_level se) (Level.simple_ext_level targetSe)
                                                   (JVM_Cert.levels_rel cert) in
                      match changeSe with
                      | true -> (b, newSeMap)
                      | false -> (true, Int32Map.add (label32) (se) (newSeMap)) )
                    | false -> (true, Int32Map.add (label32) (se) (newSeMap))
;;

let rec param_leq (k_st:Level.ext_level list) (target_lvt:MethodPolicy.t) (n:int)
                  (lvl_rel:Levels_relationship.t) =
  match k_st with
  | [] -> true
  | h :: t -> let b = Level.lub_ext (Level.get_basic_level h)
                                    (MethodPolicy.ka_at n target_lvt) (lvl_rel) in
              param_leq (t) (target_lvt) (n+1) (lvl_rel);;


let rec infer_type_rec (orderedLabel : int32 list) (jInsnMap : JVM_Instruction.t Int32Map.t)
                       (cert : JVM_Cert.t) (seMap : Level.basic_level Int32Map.t) (typeMap : StackType.t Int32Map.t)
                       (c : JVM_Class.t) (current_method_cert:JVM_Method_cert.t) (current_policy:MethodPolicy.t) =
  match orderedLabel with
  | [] -> (false, seMap, typeMap)
  | label32 :: t ->
     let ins = Int32Map.find label32 jInsnMap in
     let cSe = Int32Map.find label32 seMap in
     let cSt = Int32Map.find label32 typeMap in
     match JVM_Instruction.code ins with
     | Nop -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let (b, newTypeMap) = fix_result_one (succ32) (cSt) (cert) (typeMap) in
       let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap) (c)
                                                       (current_method_cert) (current_policy) in
       (b || b', resSeMap, resTypeMap)
     )
     | Push -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let newSt = ST_op.add (Level.simple_ext_level cSe) cSt in
       let (b, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
       let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap) (c)
                                                       (current_method_cert) (current_policy) in
       (b || b', resSeMap, resTypeMap)
     )
     | Pop -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let (_, newSt) = ST_op.pop_n cSt 1 in
       let (b, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
       let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap) (c)
                                                       (current_method_cert) (current_policy) in
       (b || b', resSeMap, resTypeMap) )
     | Load -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let locVar = JVM.ArgsArgs.arg_at 0 (JVM.getArgsArgs ins) in
       let locVar_level = MethodPolicy.ka_at locVar current_policy in
       let lvl_rel = (JVM_Cert.levels_rel cert) in
       let k = Level.lub_ext cSe locVar_level lvl_rel in
       let newSt = ST_op.add (k) cSt in
       let (b, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
       let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap) (c)
                                                       (current_method_cert) (current_policy) in
       (b || b', resSeMap, resTypeMap) )
     | Store -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let locVar = JVM.ArgsArgs.arg_at 0 (JVM.getArgsArgs ins) in
       let locVar_level = MethodPolicy.ka_at locVar current_policy in
       let ([k], newSt) = ST_op.pop_n cSt 1 in
       let lvl_rel = (JVM_Cert.levels_rel cert) in
       let kr = Level.lub_ext cSe k lvl_rel in
       match Level.leq_ext (kr) (locVar_level) (lvl_rel) with
       | true -> (
         let (b, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
         let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap)
                                                         (c) (current_method_cert) (current_policy) in
         (b || b', resSeMap, resTypeMap) )
       | false ->
          let error_message = "attempting to store incompatible value for label " ^
                                (string_of_int (JVM_Instruction.label ins)) in
          let error_message = error_message ^ " " ^ (Level.string_of_ext_level kr)
                              ^ " " ^ (Level.string_of_ext_level locVar_level) in
          raise (Invalid_argument error_message) )
     | Swap -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let ([k1; k2], cSt2) = ST_op.pop_n cSt 2 in
       let cSt' = ST_op.add k2 cSt2 in
       let newSt = ST_op.add k1 cSt' in
       let (b, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
       let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap)
                                                       (c) (current_method_cert) (current_policy) in
       (b || b', resSeMap, resTypeMap)
     )
     | Goto -> (
       let arg = JVM.getGotoArgs ins in
       let target32 = (Int32.add label32 (Int32.of_int arg)) in
       let (b, newTypeMap) = fix_result_one (target32) (cSt) (cert) (typeMap) in
       let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap)
                                                       (c) (current_method_cert) (current_policy) in
       (b || b', resSeMap, resTypeMap) )
     | If -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let arg = JVM.getIfArgs ins in
       let target32 = Int32.add label32 (Int32.of_int (IfArgs.offset arg)) in
       let lvl_rel = JVM_Cert.levels_rel cert in
       let regMap = JVM_Method_cert.reg_map current_method_cert in
       let regList = List.map (fun h -> Int32.of_int h) (Region.find (label32) regMap) in
       match IfArgs.kind arg with
       | Eq | Ne | Gt | Ge | Lt | Le -> (
         let ([k1; k2], cSt2) = ST_op.pop_n cSt 2 in
         let kr = Level.lub_ext (Level.get_basic_level (Level.lub_ext (cSe) k1 lvl_rel)) (k2) (lvl_rel) in
         let newSt = ST_op.lift_st cSt2 (Level.get_basic_level kr) lvl_rel in
         let se = Level.get_basic_level kr in
         let (b, newSeMap) = fix_se_region (regList) (se) (seMap) (cert) in
         let (b1, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
         let (b2, newTypeMap) = fix_result_one (target32) (newSt) (cert) (newTypeMap) in
         let (b3, resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (newSeMap) (newTypeMap)
                                                         (c) (current_method_cert) (current_policy) in
         (b || b1 || b2 || b3, resSeMap, resTypeMap)
       )
       | Eqz | Nez | Gtz | Gez | Ltz | Lez -> (
         let ([k1], cSt1) = ST_op.pop_n (cSt) 1 in
         let kr = Level.lub_ext (cSe) k1 lvl_rel in
         let newSt = ST_op.lift_st cSt1 (Level.get_basic_level kr) lvl_rel in
         let se = Level.get_basic_level kr in
         let (b, newSeMap) = fix_se_region (regList) (se) (seMap) (cert) in
         let (b1, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
         let (b2, newTypeMap) = fix_result_one (target32) (newSt) (cert) (newTypeMap) in
         let (b3, resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (newSeMap) (newTypeMap)
                                                         (c) (current_method_cert) (current_policy) in
         (b || b1 || b2 || b3, resSeMap, resTypeMap) )
     )
     | Return -> (
       match MethodPolicy.kr_n current_policy with
       | 0 -> infer_type_rec (t) (jInsnMap) (cert) (seMap) (typeMap) (c)
                             (current_method_cert) (current_policy)
       | _ ->
          let kn = MethodPolicy.kr_at 0 current_policy in
          let k = ST_op.top cSt in
          let kr = Level.lub_ext cSe k (JVM_Cert.levels_rel cert) in
          match Level.leq_ext (kr) (kn) (JVM_Cert.levels_rel cert) with
          | true -> infer_type_rec (t) (jInsnMap) (cert) (seMap) (typeMap) (c)
                                   (current_method_cert) (current_policy)
          | false -> raise (Invalid_argument "attempting to return incompatible return value")
     )
     | Binop -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let ([ka; kb], cSt2) = ST_op.pop_n cSt 2 in
       let lvl_rel = JVM_Cert.levels_rel cert in
       let k = Level.lub_ext (Level.get_basic_level (Level.lub_ext cSe ka lvl_rel)) kb lvl_rel in
       let newSt = ST_op.add (k) (cSt2) in
       let (b, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
       let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap)
                                                       (c) (current_method_cert) (current_policy) in
       (b || b', resSeMap, resTypeMap) )
     | IInc -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let locVar = JVM.ArgsArgs.arg_at 0 (JVM.getArgsArgs ins) in (* TODO : need to check the position *)
       let locVar_level = MethodPolicy.ka_at locVar current_policy in
       let kr = Level.lub_ext cSe locVar_level (JVM_Cert.levels_rel cert) in
       match Level.leq_ext (kr) (locVar_level) (JVM_Cert.levels_rel cert) with
       | true ->
          let (b, newTypeMap) = fix_result_one (succ32) (cSt) (cert) (typeMap) in
          let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap)
                                                          (c) (current_method_cert) (current_policy) in
          (b || b', resSeMap, resTypeMap)
       | false -> raise (Invalid_argument "attempting to Iinc a low local variable in a high environment")
     )
     | New -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let newSt = ST_op.add (Level.simple_ext_level cSe) cSt in
       let (b, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
       let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap) (c)
                                                       (current_method_cert) (current_policy) in
       (b || b', resSeMap, resTypeMap)
     )
     | Getfield -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let arg = JVM.getFMIArgs ins in
       let ([k], cSt') = ST_op.pop_n cSt 1 in
       let ft_level = FieldPolicy.ft (JVM.FMIArgs.class_name arg) (JVM.FMIArgs.name arg)
                                     (JVM_Cert.field_policy cert) in
       let kr = Level.lub_ext cSe k (JVM_Cert.levels_rel cert) in
       let kr' = Level.lub_ext (Level.get_basic_level kr) ft_level (JVM_Cert.levels_rel cert) in
       let newSt = ST_op.add kr' cSt' in
       let (b, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
       let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap)
                                                       (c) (current_method_cert) (current_policy) in
       (b || b', resSeMap, resTypeMap)
     )
     | Putfield -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let arg = JVM.getFMIArgs ins in
       let lvl_rel = JVM_Cert.levels_rel cert in
       let ([k1;k2], newSt) = ST_op.pop_n cSt 2 in
       let ft_level = FieldPolicy.ft (JVM.FMIArgs.class_name arg) (JVM.FMIArgs.name arg)
                                     (JVM_Cert.field_policy cert) in
       let kr = Level.lub_ext (Level.lub cSe (Level.get_basic_level k2) lvl_rel) k1 lvl_rel in
       match Level.leq_ext (kr) (ft_level) (lvl_rel) with
       | true -> (
         let (b, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
         let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap)
                                                         (c) (current_method_cert) (current_policy) in
         (b || b', resSeMap, resTypeMap) )
       | false ->
          let error_message = "attempting to putfield incompatible value for label " ^
                                (string_of_int (JVM_Instruction.label ins)) in
          let error_message = error_message ^ " " ^ (Level.string_of_ext_level kr)
                              ^ " " ^ (Level.string_of_ext_level ft_level) in
          raise (Invalid_argument error_message)
     )
     | Dup -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let k = ST_op.top cSt in
       let kr = Level.lub_ext cSe k (JVM_Cert.levels_rel cert) in
       let newSt = ST_op.add kr cSt in
       let (b, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
       let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap)
                                                       (c) (current_method_cert) (current_policy) in
       (b || b', resSeMap, resTypeMap)
     )
     | Invoke m -> (
       let size = JVM_Instruction.size ins in
       let succ32 = (Int32.add label32 (Int32.of_int size)) in
       let args = JVM.getFMIArgs ins in
       let class_name = JVM.FMIArgs.class_name args in
       let method_name = JVM.FMIArgs.name args in
       let method_desc = JVM.FMIArgs.fmi_type args in

       let m' = JVM_Cert.method_cert (class_name) (method_name) (method_desc) (cert) in
       let lvl_rel = JVM_Cert.levels_rel cert in

       let nArgs = Utility.countArgs_from_desc (method_desc) in
       let nRet = Utility.countRet_from_desc (method_desc) in

       let (k_st, cSt) = ST_op.pop_n cSt nArgs in
       let ([k_o], cSt') = ST_op.pop_n cSt 1 in
       let target_lvt = JVM_Method_cert.policy (Level.basic_level_id (Level.get_basic_level k_o)) m' in
       let kh = MethodPolicy.kh target_lvt in

       let constraint1 = Level.leq_ext k_o (MethodPolicy.ka_at 0 current_policy) lvl_rel in
       let constraint2 = Level.leq_ext (Level.lub_ext (Level.lub cSe kh lvl_rel) k_o lvl_rel)
                                       (Level.simple_ext_level (MethodPolicy.kh target_lvt)) lvl_rel in
       let constraint3 = param_leq (k_st) (target_lvt) (1) (lvl_rel) in

       let newSt = (match nRet with
                    | 0 -> cSt'
                    | _ -> let kr' = MethodPolicy.kr_at 0 target_lvt in
                           ST_op.add (Level.lub_ext (Level.lub cSe (Level.get_basic_level k_o) lvl_rel)
                                                    kr' lvl_rel) cSt'
                   ) in

       match constraint1 && constraint2 && constraint3 with
       | true ->
          let (b, newTypeMap) = fix_result_one (succ32) (newSt) (cert) (typeMap) in
          let (b', resSeMap, resTypeMap) = infer_type_rec (t) (jInsnMap) (cert) (seMap) (newTypeMap)
                                                          (c) (current_method_cert) (current_policy) in
          (b || b', resSeMap, resTypeMap)
       | false ->
          let error_message = "attempting to invoke incompatible value for label " ^
                                (string_of_int (JVM_Instruction.label ins)) in
          raise (Invalid_argument error_message)
     )
     | Throw -> raise (Invalid_argument "Not dealing with exception yet")
;;

let rec infer_type_initrec (orderedLabel : int32 list) (jInsnMap : JVM_Instruction.t Int32Map.t)
                           (cert : JVM_Cert.t) (seMap : Level.basic_level Int32Map.t) (typeMap : StackType.t Int32Map.t)
                           (c : JVM_Class.t) (current_method_cert:JVM_Method_cert.t) (current_policy:MethodPolicy.t) =
  let (changed, seMap, typeMap) = infer_type_rec (orderedLabel) (jInsnMap) (cert) (seMap) (typeMap)
                                                 (c) (current_method_cert) (current_policy) in
  match changed with
  | true -> infer_type_initrec (orderedLabel) (jInsnMap) (cert) (seMap) (typeMap)
                               (c) (current_method_cert) (current_policy)
  | false -> (seMap, typeMap);;

let rec initial_seMap (orderedLabel : int32 list) (default_se : Level.basic_level) =
  match orderedLabel with
  | [] -> Int32Map.empty
  | h :: t -> Int32Map.add h default_se (initial_seMap t default_se);;

let infer_type (c:JVM_Class.t) (m : JVM_Method.t) (cert : JVM_Cert.t) (object_level:int)
               (default_se : Level.basic_level)  =
  let jInsnList = JVM_Method.insnList m in
  let jInsnMap = JVM_Method.insnMap m in
  let orderedLabel = Flow_tracer.trace (jInsnList) (jInsnMap) in
  (* get all possible security level to enumerate all the level of the object *)
  let current_method_cert = JVM_Cert.method_cert (JVM_Class.get_class_name c)
                                                 (JVM_Method.name m) (JVM_Method.desc m) cert in
  let current_policy = JVM_Method_cert.policy (object_level) current_method_cert in
  let (seMap, typeMap) = infer_type_initrec (orderedLabel) (jInsnMap) (cert)
                                            (initial_seMap orderedLabel default_se)
                                            (Int32Map.add (List.hd orderedLabel) (StackType.empty) Int32Map.empty)
                                            (c) (current_method_cert) (current_policy) in
  Type_assigner.assign_type (jInsnList) (jInsnMap) (seMap) (typeMap);;
