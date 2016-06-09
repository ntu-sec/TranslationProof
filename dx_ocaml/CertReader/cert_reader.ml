open Certificate;;
open Utility;;
open Map_type;;

let readBytes (ic: in_channel) (n:int) =
  read_n (readByte) (ic) (n)
;;

let readShorts (ic:in_channel) (n:int) = read_n (readShort) (ic) (n);;

let rec readString_rec (ic: in_channel) (n: int) =
  match n <= 0 with
  | true -> ""
  | false -> let inp = readString_rec (ic) (n -1) in
             inp ^ (Char.escaped (Char.chr (readByte (ic))))
;;

let read_string (ic: in_channel) =
  let n = readByte (ic) in readString_rec (ic) (n);;

let rec read_levels_rec (ic: in_channel) (i: int) (n: int) =
  match i < n with
  | true -> let current_name = read_string (ic) in
            (Level.create_basic_level (i) (current_name)) :: read_levels_rec (ic) (i + 1) (n)
  | false -> []
;;

(* assume that the file will be read from the correct pointer, that is the *)
(* start of the file                                                       *)
let read_levels (ic: in_channel) : (Level_pool.t * Level.basic_level * Level.basic_level) =
  let n = readByte (ic) in
  let lvl_pool = Level_pool.create_levels (n) (read_levels_rec (ic) (0) (n)) in
  let default_lvl_idx = readByte(ic) in
  let default_top_lvl_idx = readByte(ic) in
  (lvl_pool, (Level_pool.level (lvl_pool) (default_lvl_idx)),
   (Level_pool.level (lvl_pool) (default_top_lvl_idx)));;

let rec read_levels_relationship_rec (ic: in_channel) (n: int) (m : Levels_relationship.t) =
  match n <= 0 with
  | true -> m
  | false ->
     let idx1 = readByte (ic) in
     let idx2 = readByte (ic) in
     Levels_relationship.add (Int32.of_int idx1) (Int32.of_int idx2)
                             (read_levels_relationship_rec (ic) (n - 1) (m))
;;

let read_levels_relationship (ic: in_channel) (n_levels : int) : Levels_relationship.t =
  let n = readShort (ic) in
  read_levels_relationship_rec (ic) (n) (Levels_relationship.initial_map n_levels)
;;

let rec read_n_bytes (ic:in_channel) (n:int) =
  match n <= 0 with
  | true -> []
  | false -> let n_b = readByte (ic) in
             let current_bytes = readBytes (ic) (n_b) in
             current_bytes :: read_n_bytes (ic) (n - 1)

let getJun = fun inp -> match inp with None -> "None" | Some x -> string_of_int (x)

let rec list_to_map (l : JVM_Method_cert.t list) =
  match l with
  | [] -> StringMap.empty
  | method_cert :: t -> let resMap = list_to_map (t) in
                        let class_name = JVM_Method_cert.class_name method_cert in
                        let name = JVM_Method_cert.name method_cert in
                        let desc = JVM_Method_cert.descriptor method_cert in
                        match StringMap.mem (class_name) (resMap) with
                        | true -> (let cn_map = StringMap.find (class_name) (resMap) in
                                   let new_cnMap = match StringMap.mem (name) cn_map with
                                     | false -> StringMap.add (name) (StringMap.add (desc) (method_cert) StringMap.empty) cn_map
                                     | true -> let subMap = StringMap.find (name) cn_map in
                                               StringMap.add (name) (StringMap.add (desc) (method_cert) subMap) cn_map in
                                   StringMap.add (class_name) (new_cnMap) (resMap)
                                  )
                        | false -> StringMap.add (class_name) (StringMap.add (name)
                                                                             (StringMap.add (desc) (method_cert) StringMap.empty) StringMap.empty) (resMap);;

let read_certificate (cn:string) (ic:in_channel) =
  let (levels, default_level, default_top_level) = read_levels (ic) in
  let levels_relationship = read_levels_relationship (ic) (Level_pool.length levels) in
  let field_policy = FieldPolicy.read (ic) (levels) in
  let n_method = readShort (ic) in
  let method_cert_list = read_n (fun ic' -> JVM_Method_cert.read_wo_typing (ic') (levels))
                                (ic) (n_method) in
  let method_cert_map = list_to_map (method_cert_list) in
  let method_cert_map = match cn with
    | "" -> method_cert_map
    | _ -> let temp_map = Default_methods.JVM.add_default_methods (method_cert_map) (levels) in
           Default_methods.JVM.add_current_class_methods (temp_map) (cn) (levels)
  in
  JVM_Cert.create_cert (levels) (levels_relationship) (method_cert_map) (field_policy)
                       (default_level) (default_top_level);;
