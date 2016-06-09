open Map_type;;
open Utility;;

let rec readString_rec (ic: in_channel) (n: int) =
  match n <= 0 with
  | true -> ""
  | false -> let inp = readString_rec (ic) (n -1) in
             inp ^ (Char.escaped (Char.chr (readByte (ic))))
;;

let read_string (ic: in_channel) =
  let n = readByte (ic) in readString_rec (ic) (n);;

module Levels_relationship = struct
    type t = (bool Int32Map.t) Int32Map.t;;
    let leq32 = fun l1 l2 t -> Int32Map.find l2 (Int32Map.find l1 t);;
    let leq = fun l1 l2 t -> leq32 (Int32.of_int l1) (Int32.of_int l2) (t)
    let add = fun idx1 idx2 t -> let subMap = Int32Map.find idx1 t in
                                 Int32Map.add idx1 (Int32Map.add idx2 true subMap) t;;

    let rec initial_map2 (i: int) (n: int) =
      match i < n with
      | false -> Int32Map.empty
      | true -> Int32Map.add (Int32.of_int i) false (initial_map2 (i +1) (n))
    ;;

    let rec initial_map1 (i: int) (n: int) =
      match i < n with
      | false -> Int32Map.empty
      | true -> let subMap = initial_map2 (0) (n) in
                Int32Map.add (Int32.of_int i) (Int32Map.add (Int32.of_int i) true subMap)
                             (initial_map1 (i +1) (n))
    ;;

    let initial_map (n : int) = initial_map1 (0) (n);;

    let to_byteString = fun t -> let (n, res) = Int32Map.fold
                                                  (fun key32 innerMap result -> Int32Map.fold
                                                                                  (fun key'32 rel result' -> if rel then let key = Int32.to_int key32 in
                                                                                                                         let key' = Int32.to_int key'32 in if (key<>key') then
                                                                                                                                                             let (n', res') = result' in
                                                                                                                                                             (n'+1, res'^(string_of_byte_value key) ^ (string_of_byte_value key'))
                                                                                                                                                           else result' else result')
                                                                                  innerMap result) t (0, "") in
                                 string_of_short_value (n) ^ res;;
  end

module Level = struct
    type basic_level = { sec_id: int; sec_name: string };;
    type ext_level = Simple of basic_level | Array of (basic_level * ext_level);;
    let basic_level_id = fun t -> t.sec_id;;
    let rec ext_level_ids (ext:ext_level) = match ext with
      | Simple lvl -> [lvl.sec_id]
      | Array (k, kc) -> k.sec_id :: ext_level_ids (kc);;

    let create_basic_level = fun id name -> {sec_id = id; sec_name = name};;
    let create_ext_level = fun k kc -> Array (k, kc);;
    let simple_ext_level = fun k -> Simple k;;
    let get_basic_level = fun t -> match t with | Simple k -> k | Array (k, kc) -> k;;

    let bot_level = { sec_id = -1; sec_name = "Bottom" };;
    let string_of_basic_level (t: basic_level) = t.sec_name;;
    let rec string_of_ext_level (t:ext_level) = match t with
      | Simple k -> string_of_basic_level (k)
      | Array (k, kc) -> string_of_basic_level (k)^"["^string_of_ext_level (kc) ^"]"
    let rec string_of_level_list (l: ext_level list) =
      match l with
      | [] -> ""
      | h :: [] -> string_of_ext_level (h)
      | h :: t -> string_of_ext_level (h) ^ ", " ^ string_of_level_list (t)
    ;;

    let byteString_of_basic_level = fun t ->
      string_of_byte_value (String.length t.sec_name) ^ t.sec_name;;

    let leq_ext (k1:ext_level) (k2:ext_level) (lvl_rel:Levels_relationship.t) =
      match k1, k2 with
      | Simple k1', Simple k2' -> Levels_relationship.leq (k1'.sec_id) (k2'.sec_id) lvl_rel
      | Array (k1', kc1), Array (k2', kc2) -> Levels_relationship.leq (k1'.sec_id) (k2'.sec_id) lvl_rel
      | _, _ -> raise (Invalid_argument "comparing a simple level and an array level");;

    let lub (k1:basic_level) (k2:basic_level) (lvl_rel:Levels_relationship.t) =
      match Levels_relationship.leq (k1.sec_id) (k2.sec_id) lvl_rel with
      | true -> k2
      | false -> k1;;

    let lub_ext (k1:basic_level) (k2:ext_level) (lvl_rel:Levels_relationship.t) =
      match k2 with
      | Simple k -> (match Levels_relationship.leq (k1.sec_id) (k.sec_id) lvl_rel with
                     | true -> k2
                     | false -> Simple k1)
      | Array (k, kc) -> (match Levels_relationship.leq (k1.sec_id) (k.sec_id) lvl_rel with
                          | true -> k2
                          | false -> Array (k1, kc));;
  end;;

module Level_pool = struct
    type t = { length : int; levels : Level.basic_level list };;
    let length = fun t -> t.length;;
    let level = fun t idx -> List.nth t.levels idx;;
    let create_levels = fun n l -> { length = n; levels = l };;

    let rec int_list2ext_level (input:int list) (lvl_pool:t) =
      match input with
      | [] -> raise (Invalid_argument ("Index to security level must not be empty"))
      | h :: [] -> Level.simple_ext_level (level lvl_pool h)
      | h :: t -> Level.create_ext_level (level lvl_pool h) (int_list2ext_level (t) (lvl_pool));;

    let to_string = fun t i -> Level.string_of_basic_level (level t i);;

    let get_id_list = fun t -> List.map (fun lvl -> Level.basic_level_id lvl) t.levels;;

    let rec to_strings_rec t (n: int) =
      match n < length t with
      | true -> (to_string t n) :: to_strings_rec (t) (n +1)
      | false -> [];;
    let to_strings = fun t -> to_strings_rec (t) (0);;

    let empty = { length = 0; levels = []};;

    let rec to_byteString_rec (lvl_list : Level.basic_level list) =
      match lvl_list with
      | [] -> ""
      | h :: t -> (Level.byteString_of_basic_level h) ^ to_byteString_rec (t);;
    let to_byteString = fun t -> string_of_byte_value (t.length) ^ to_byteString_rec (t.levels);;
  end;;

let translate_level = Level_pool.int_list2ext_level;;
let rec translate_levels (l: (int list) list) (lvl_pool: Level_pool.t) : Level.ext_level list =
  match l with
  | [] -> []
  | h:: t -> (translate_level h lvl_pool) :: translate_levels (t) (lvl_pool)
;;

module type Ins_cert_type = sig
    type t;;
    val to_string : t -> string;;
    val to_byteString : t -> string;;
    val empty : t;;
  end

module StackType = struct
    type t = Level.ext_level list;;
    let to_string = Level.string_of_level_list;;
    let to_byteString = fun t -> "";; (* for now we don't care about stacktype as it is not used *)
    let empty = [];;
  end

module ST_op = struct
    let rec lub_st (oldSt : StackType.t) (newSt : StackType.t) (lvl_rel : Levels_relationship.t) =
      match oldSt, newSt with
      | [], [] -> (false, [])
      | h1 :: t1, h2 :: t2 -> (let (b, newSt) = lub_st (t1) (t2) (lvl_rel) in
                               match Level.leq_ext (h2) (h1) (lvl_rel) with
                               | true -> (b, h1 :: newSt)
                               | false -> (true, h2 :: newSt) )
      | _ -> Printf.printf "%d - %d\n" (List.length oldSt) (List.length newSt) ;
             raise (Invalid_argument "trying to lub non-matching stack type");;

    let convert_from_raw = fun raw_t lvl_pool ->
      translate_levels raw_t lvl_pool;;
    let add = fun h t -> h :: t;;

    let rec pop_n (l : StackType.t) (n : int) =
      match n > 0 with
      | true -> (match l with
                 | h :: l' -> let (values, result) = pop_n (l') (n-1) in
                              (h::values, result)
                 | [] -> raise (Invalid_argument "Attempting to pop an empty stack"))
      | false -> ([], l);;

    let get = fun idx t -> List.nth t idx;;
    let top = fun t -> List.hd t;;
    let length = fun t -> List.length t;;

    let rec lift_st (st : StackType.t) (k : Level.basic_level) (lvl_rel : Levels_relationship.t) =
      match st with
      | [] -> []
      | h :: t -> Level.lub_ext (k) (h) (lvl_rel) :: (lift_st t k lvl_rel)
  end;;

type registersType_t = {rt:Level.ext_level Int32Map.t; res:(Level.ext_level) option;
                        flag: int};;

(* TODO : the details are still messed up *)
module RegistersType = struct
    type t = registersType_t;;
    let to_string = fun t -> (Int32Map.fold (fun key v str -> str ^ (Int32.to_string key) ^ "->" ^
                                                                (Level.string_of_ext_level v) ^ ", ") t.rt "") ^
                               (match t.res with None -> "" | Some res -> Level.string_of_ext_level res);; (* TODO *)

    let to_byteString = fun t -> string_of_short_value (Int32Map.cardinal (t.rt)) ^
                                   (Int32Map.fold (fun key v str -> (string_of_short_value (Int32.to_int key))
                                                                    ^ (string_of_byte_list (Level.ext_level_ids v)) ^ str) (t.rt) "") ^
                                     (match t.res with None -> string_of_byte_value 0 | Some res -> string_of_byte_list (Level.ext_level_ids res))
                                     ^ (string_of_byte_value (t.flag));;
    let empty = {rt=Int32Map.empty; res=None; flag=0};;
  end

module RT_op = struct
    let add = fun key value t ->
      {t with rt=Int32Map.add (Int32.of_int key) value (t.rt)};;
    let get = fun key t -> Int32Map.find (Int32.of_int key) (t.rt);;
    let create = fun rt res flag -> {rt=rt; res=res; flag=flag};;
    let update_flag = fun flag t -> {t with flag=flag};;

    let rec add_missing_registers_rt (lvl:Level.ext_level) (keys:int list)
                                     (rt:Level.ext_level Int32Map.t) : Level.ext_level Int32Map.t =
      match keys with
      | [] -> rt
      | key :: t -> match Int32Map.mem (Int32.of_int key) rt with
                    | true -> add_missing_registers_rt (lvl) (t) (rt)
                    | false -> (add_missing_registers_rt (lvl) (t)
                                                         (Int32Map.add (Int32.of_int key) (lvl) (rt)))
    ;;

    let add_missing_registers = fun lvl max_registers t ->
      let temp_rt = t.rt in
      let temp_rt = add_missing_registers_rt lvl (Utility.int_iterator max_registers)
                                             temp_rt in
      {t with rt=temp_rt};;
  end;;

module type Ins_typing_type = sig
    type ins_cert_t
    type t
    val se : t -> Level.basic_level
    val typing : t -> ins_cert_t
    val create : Level.basic_level -> ins_cert_t -> t
    val to_string : t -> string
    val to_byteString : t -> string
    val empty : t
  end

module Ins_typing = functor (X: Ins_cert_type) ->
                    struct
                      type ins_cert_t = X.t
                      type t = { se: Level.basic_level; cert: ins_cert_t }
                      let se = fun t -> t.se;;
                      let typing = fun t -> t.cert;;
                      let create = fun se t -> {se=se; cert=t};;
                      let to_string = fun t -> "(se:" ^ (Level.string_of_basic_level (t.se)) ^ ") - type:[" ^
                                                 X.to_string t.cert ^ "]"
                      let empty = {se=Level.bot_level; cert=X.empty};;

                      let to_byteString = fun t ->
                        string_of_byte_value (Level.basic_level_id t.se) ^
                          (X.to_byteString t.cert);;

                      let update_cert = fun f t -> {t with cert=f(t.cert)};;
                    end
module JVM_ins_typing = Ins_typing (StackType);;
module DEX_ins_typing = Ins_typing (RegistersType);;

module DEX_typing_op = struct
    let add_missing_registers = fun lvl max_registers (t:DEX_ins_typing.t) ->
      DEX_ins_typing.update_cert (RT_op.add_missing_registers lvl max_registers) (t)
  end;;

module MethodPolicy = struct
    type t = { localN : int; _ka : Level.ext_level list; _kh : Level.basic_level;
               returnN : int; _kr : Level.ext_level list }
    let ka = fun t -> t._ka;;
    let ka_at = fun i t -> List.nth t._ka i;;
    let kh = fun t -> t._kh;;
    let kr_n = fun t -> t.returnN;;
    let kr = fun t -> t._kr;;
    let kr_at = fun i t -> List.nth t._kr i;;

    let create_policy = fun localN raw_ka raw_kh returnN raw_kr lvl_pool ->
      let ka = translate_levels (raw_ka) (lvl_pool) in
      let kr = translate_levels (raw_kr) (lvl_pool) in
      { localN = localN; _ka = ka; _kh = (Level_pool.level lvl_pool raw_kh);
        returnN = returnN; _kr = kr };;
    let empty = { localN = 0; _ka = []; _kh = Level.bot_level; returnN = 0; _kr = []};;

    let to_string = fun t -> "{" ^ Level.string_of_level_list (ka t) ^ "} -> " ^
                               (Level.string_of_basic_level t._kh) ^ " -> {" ^ Level.string_of_level_list (kr t) ^ "}"
    let to_byteString = fun t -> string_of_byte_value (t.localN) ^
                                   (List.fold_right (fun h res -> (string_of_byte_list (Level.ext_level_ids h))^res) t._ka "")
                                   ^ string_of_byte_value (Level.basic_level_id t._kh) ^ string_of_byte_value (t.returnN) ^
                                     (List.fold_right (fun h res -> (string_of_byte_list (Level.ext_level_ids h))^res) t._kr "")

    let read (ic: in_channel) (lvl_pool:Level_pool.t) =
      let localN = readByte (ic) in
      let ka = read_n (fun ic' -> let n = readByte(ic) in
                                  read_n (readByte) ic' n) (ic) (localN) in (* originally working version used read_n_bytes *)
      let kh = readByte (ic) in
      let returnN = readByte (ic) in
      let kr = read_n (fun ic' -> let n = readByte(ic) in
                                  read_n (readByte) ic' n) (ic) (returnN) in (* originally working version used read_n_bytes *)
      create_policy (localN) (ka) (kh) (returnN) (kr) (lvl_pool)
    ;;

  end

module Region = struct
    type t = (int list) Int32Map.t;;
    let find = Int32Map.find;;
    let add = Int32Map.add;;
    let empty = Int32Map.empty;;
    let bindings = Int32Map.bindings;;
    let fold = Int32Map.fold;;
    let iter = Int32Map.iter;;
    let cardinal = Int32Map.cardinal;;
    let to_string = fun t -> "";;
    (* NOTE : to byteString for region and junction is not for the entire map, but only*)
    (* for a particular element *)
    let rec to_byteString_rec (input: int list) = match input with
      | [] -> ""
      | h :: t ->  string_of_short_value (h) ^ to_byteString_rec (t)
    let to_byteString = fun input ->
      let str = string_of_short_value (List.length input) ^ to_byteString_rec (input) in
      str;;

    let read_item (ic:in_channel) =
      let n = readShort (ic) in read_n (readShort) (ic) (n)
  end;;

module Junction = struct
    type t = (int option) Int32Map.t;;
    let find = Int32Map.find;;
    let add = Int32Map.add;;
    let empty = Int32Map.empty;;
    let bindings = Int32Map.bindings;;
    let fold = Int32Map.fold;;
    let cardinal = Int32Map.cardinal;;
    let to_string = fun t -> "";;
    (* NOTE : to byteString for region and junction is not for the entire map, but only*)
    (* for a particular element *)
    let to_byteString = fun input -> match input with
                                     | None -> string_of_short_value (0)
                                     | Some x -> string_of_short_value (x);;
  end;;

module type BytecodeMethod_cert_type = sig
    type ins_typing_t
    type t
    val typing_at : int -> t -> ins_typing_t
    val empty : t
    val to_string : t -> string
    val to_byteString : t -> string
  end

module BytecodeMethod_cert = functor (Ins_typing : Ins_typing_type) ->
                             struct
                               type ins_typing_t = Ins_typing.t;;
                               type t = (ins_typing_t) Int32Map.t;;
                               let typing_at = fun i t -> Int32Map.find (Int32.of_int i) t;;

                               let empty = Int32Map.empty;;

                               let rec to_string_rec (typeList: (int32 * ins_typing_t) list) =
                                 match typeList with
                                 | [] -> ""
                                 | (k1, _type)::t1 ->
                                    let str = Ins_typing.to_string (_type) ^ "\n"
                                    in str ^ (to_string_rec (t1));;

                               let to_string = fun t -> let typeList = Int32Map.bindings t in
                                                        to_string_rec (typeList);;

                               let rec to_byteString_rec (typeList: (int32 * ins_typing_t) list) =
                                 match typeList with
                                 | [] -> ""
                                 | (k1, _type)::t1 ->
                                    let str = string_of_short_value (Int32.to_int k1) ^  Ins_typing.to_byteString (_type)
                                    in str ^ (to_byteString_rec (t1));;

                               let to_byteString = fun t -> let typeList = Int32Map.bindings t in
                                                            string_of_short_value (List.length typeList) ^ to_byteString_rec (typeList);;
                             end
module JVM_BytecodeMethod_cert = BytecodeMethod_cert (JVM_ins_typing);;
module DEX_BytecodeMethod_cert = BytecodeMethod_cert (DEX_ins_typing);;

module type Method_cert_type = sig
    type bytecodeMethod_cert_t;;
    module Item : sig type t;; val policy : t -> MethodPolicy.t;;
                      val create : Level.basic_level -> MethodPolicy.t -> bytecodeMethod_cert_t -> t;;
                      val bytecodeMethod_cert : t -> bytecodeMethod_cert_t;; end;;
    type t;;
    val class_name : t -> string;;
    val name : t -> string;;
    val descriptor : t -> string;;
    val policy : int -> t -> MethodPolicy.t;;
    val bytecodeMethod_cert : int -> t -> bytecodeMethod_cert_t;;
    val create : string -> string -> string -> Region.t -> Junction.t -> Item.t Int32Map.t -> t;;
    val empty : t;;
    val to_string : t -> string;;
    val to_byteString : t -> string;;
    val reg_map : t -> Region.t
    val jun_map : t -> Junction.t
    val reg_at : int -> t -> int list
    val jun_at : int -> t -> int option
    val read_wo_typing : in_channel -> Level_pool.t -> t;;
  end

module Method_cert = functor (BM : BytecodeMethod_cert_type) ->
                     struct
                       type bytecodeMethod_cert_t = BM.t;;
                       module Item = struct
                           type t = {level:Level.basic_level; policy: MethodPolicy.t;
                                     bytecodeMethod_cert:bytecodeMethod_cert_t};;
                           let policy = fun t -> t.policy;;
                           let bytecodeMethod_cert = fun t -> t.bytecodeMethod_cert;;

                           let create = fun lvl pol cert -> {level=lvl; policy=pol; bytecodeMethod_cert=cert};;

                           let read_wo_typing (ic:in_channel) (lvl_pool:Level_pool.t) =
                             let level_id = readByte (ic) in
                             let policy = MethodPolicy.read (ic) (lvl_pool) in
                             let level = Level_pool.level lvl_pool level_id in
                             let result = {level=level; policy=policy; bytecodeMethod_cert=BM.empty} in
                             (level_id, result);;

                           let to_byteString = fun t -> string_of_byte_value (Level.basic_level_id t.level) ^
                                                          (MethodPolicy.to_byteString t.policy) ^ (BM.to_byteString t.bytecodeMethod_cert);;
                         end;;

                       type t = {class_name:string; name: string; descriptor: string;
                                 contents:Item.t Int32Map.t; region: Region.t; junction: Junction.t};;

                       let reg_map = fun t -> t.region;;
                       let jun_map = fun t -> t.junction;;
                       let reg_at = fun i t -> Region.find (Int32.of_int i) t.region;;
                       let jun_at = fun i t -> Junction.find (Int32.of_int i) t.junction;;
                       let class_name = fun t -> t.class_name;;
                       let name = fun t -> t.name;;
                       let descriptor = fun t -> t.descriptor;;

                       let policy = fun lvl_id t -> Item.policy (Int32Map.find (Int32.of_int lvl_id) t.contents);;
                       let bytecodeMethod_cert = fun lvl_id t -> Item.bytecodeMethod_cert (Int32Map.find (Int32.of_int lvl_id) t.contents);;
                       let create = fun cn nm desc reg jun contents ->
                         {class_name=cn; name=nm; descriptor=desc; region=reg; junction=jun; contents=contents};;
                       let empty = {class_name=""; name=""; descriptor=""; region=Region.empty;
                                    junction=Junction.empty; contents=Int32Map.empty};;

                       let to_string = fun t -> "name = " ^ t.name ^ "; desc = " ^ t.descriptor ^ "\n";;

                       let rec reg_jun_byteString_rec (regionList: (int32 * (int list)) list)
                                                      (junctionList: (int32 * int option) list) =
                         match regionList, junctionList with
                         | [], [] -> ""
                         | (k2, reg)::t2, (k3, jun)::t3 ->
                            let str = string_of_short_value (Int32.to_int k2) ^
                                        Region.to_byteString (reg) ^ Junction.to_byteString (jun)
                            in
                            str ^ (reg_jun_byteString_rec (t2) (t3))
                         | _ ->  raise (Invalid_argument "Region, and junction don't match");;

                       let to_byteString = fun t ->
                         string_of_byte_value (String.length t.class_name) ^ t.class_name ^
                           string_of_byte_value (String.length t.name) ^ t.name ^
                             string_of_byte_value (String.length t.descriptor) ^ t.descriptor ^
                               string_of_short_value (Region.cardinal t.region) ^
                                 reg_jun_byteString_rec (Region.bindings t.region) (Junction.bindings t.junction) ^
                                   (Int32Map.fold (fun key value str -> str ^ Item.to_byteString (value)) (t.contents) "") ;;

                       let rec read_reg_and_jun (ic:in_channel) (n:int) =
                         match n > 0 with
                         | false -> (Region.empty, Junction.empty)
                         | true ->
                            let label = readShort (ic) in
                            let reg = Region.read_item (ic) in
                            let jun = match readShort (ic) with 0 -> None | x -> Some x in
                            let (resReg, resJun) = read_reg_and_jun (ic) (n - 1) in
                            (Region.add (Int32.of_int label) (reg) resReg,
                             Junction.add (Int32.of_int label) (jun) resJun);;

                       let read_wo_typing (ic:in_channel) (lvl_pool:Level_pool.t) =
                         let class_name = read_string (ic) in
                         let name = read_string (ic) in
                         let descriptor = read_string (ic) in
                         (* only need to read region and junction once, they are not affected by the object's security level *)
                         let n_code = readShort (ic) in
                         let (regMap, junMap) = read_reg_and_jun (ic) (n_code) in
                         let items = read_n (fun ic' -> Item.read_wo_typing ic' lvl_pool) (ic) (Level_pool.length lvl_pool) in
                         let itemsMap = list_to_intMap (items) in
                         {class_name=class_name; name=name; descriptor=descriptor; contents=itemsMap;
                          region=regMap; junction= junMap};;
                     end;;
module JVM_Method_cert = Method_cert (JVM_BytecodeMethod_cert);;
module DEX_Method_cert = Method_cert (DEX_BytecodeMethod_cert);;

module FieldPolicy = struct
    module Item = struct
        type t = {class_name:string; name:string; sec_level:Level.ext_level};;
        let class_name = fun t -> t.class_name;;
        let name = fun t -> t.name;;
        let sec_level = fun t -> t.sec_level;;
        let read (ic:in_channel) (lvl_pool:Level_pool.t) =
          let class_name = read_string (ic) in
          let name = read_string (ic) in
          let n_lvl = readByte (ic) in
          let lvl_ids = read_n (readByte) ic n_lvl in
          {class_name=class_name; name=name; sec_level=translate_level(lvl_ids) lvl_pool}
      end;;
    type t = {n:int; map:(Item.t StringMap.t) StringMap.t};;

    let ft = fun class_name name t -> Item.sec_level (StringMap.find name
                                                                     (StringMap.find class_name t.map));;

    let to_byteString = fun t -> Utility.string_of_short_value (t.n) ^
                                   StringMap.fold (fun class_name subMap str ->
                                                   StringMap.fold (fun name v str' -> str' ^ (string_of_byte_value (String.length class_name))
                                                                                      ^ (string_of_byte_value (String.length name)) ^
                                                                                        (string_of_byte_list (Level.ext_level_ids (Item.sec_level v))) ) subMap str) t.map "";;

    let rec transform_items (items:Item.t list) =
      match items with
      | [] -> StringMap.empty
      | h :: t ->
         let m = transform_items t in
         let class_name = Item.class_name h in
         match StringMap.mem class_name m with
         | true -> let subMap = StringMap.find class_name m in
                   StringMap.add (class_name) (StringMap.add (Item.name h) h subMap) m
         | false -> StringMap.add (class_name) (StringMap.add (Item.name h) h StringMap.empty) m
    ;;

    let read (ic:in_channel) (lvl_pool:Level_pool.t) =
      let n = readShort (ic) in
      let items = read_n (fun ic' -> Item.read ic' lvl_pool) ic n in
      {n=n; map=transform_items (items)}
  end;;

module type Cert_type = sig
    type method_cert_t;;
    type t;;
    val lvl_pool : t -> Level_pool.t;;
    val levels_rel : t -> Levels_relationship.t;;
    val method_cert : string -> string -> string -> t -> method_cert_t;;
    val field_policy : t -> FieldPolicy.t;;
    val default_level : t -> Level.basic_level;;
    val default_top_level : t -> Level.basic_level;;
    val create_cert : Level_pool.t -> Levels_relationship.t ->
                      ((method_cert_t StringMap.t) StringMap.t) StringMap.t -> FieldPolicy.t ->
                      Level.basic_level -> Level.basic_level -> t;;
    val to_byteString : t -> string;;
  end

module Cert = functor (Method: Method_cert_type) -> struct
                  type method_cert_t = Method.t;;
                  type t = { lvl_pool: Level_pool.t; levels_rel: Levels_relationship.t;
                             method_certs: ((method_cert_t StringMap.t) StringMap.t) StringMap.t;
                             field_policy: FieldPolicy.t;
                             default_level:Level.basic_level; default_top_level:Level.basic_level};;
                  let lvl_pool = fun t -> t.lvl_pool;;
                  let levels_rel = fun t -> t.levels_rel;;
                  let method_cert = fun class_name name desc t ->
                    StringMap.find (desc) (StringMap.find name (StringMap.find class_name t.method_certs));;
                  let field_policy = fun t -> t.field_policy;;
                  let create_cert = fun lvl_pool lvl_rel method_cert field_policy def_lvl
                                        def_top_lvl ->
                    { lvl_pool = lvl_pool; levels_rel = lvl_rel;
                      method_certs = method_cert; field_policy=field_policy;
                      default_level = def_lvl; default_top_level = def_top_lvl };;
                  let default_level = fun t -> t.default_level;;
                  let default_top_level = fun t -> t.default_top_level;;
                  let to_byteString = fun t -> (Level_pool.to_byteString t.lvl_pool) ^
                                                 (string_of_byte_value (Level.basic_level_id t.default_level)) ^
                                                   (Levels_relationship.to_byteString t.levels_rel) ^
                                                     (FieldPolicy.to_byteString t.field_policy) ^
                                                       (string_of_short_value (StringMap.fold (fun k cn_map count ->
                                                                                               StringMap.fold (fun k name_map count' ->
                                                                                                               count' + (StringMap.cardinal name_map))
                                                                                                              cn_map count)
                                                                                              t.method_certs 0)) ^
                                                         (StringMap.fold (fun k cn_map str -> str ^ (StringMap.fold
                                                                                                       (fun k' name_map str' -> (StringMap.fold
                                                                                                                                   (fun k'' method_cert str'' -> str'' ^ Method.to_byteString method_cert)
                                                                                                                                   name_map str')) cn_map ""))
                                                                         t.method_certs "" )  ;;
                end

module JVM_Cert = Cert (JVM_Method_cert);;
module DEX_Cert = Cert (DEX_Method_cert);;

let rec translate_st_rt_rec (st:StackType.t) (rt:RegistersType.t) (localN : int) : RegistersType.t =
  match st with
  | [] -> rt
  | h :: t -> translate_st_rt_rec (t)
                                  (RT_op.add (localN + List.length st - 1) h rt) (localN);;

let rec base_rt_rec (localN:int) (lvt:MethodPolicy.t) : RegistersType.t =
  match localN with
  | 0 -> RegistersType.empty
  | _ -> RT_op.add (localN - 1) (MethodPolicy.ka_at (localN - 1) lvt)
                   (base_rt_rec (localN -1) (lvt));;

let translate_st_rt (st:StackType.t) (localN:int) (lvt:MethodPolicy.t)
                    (res:Level.ext_level option) : RegistersType.t =
  {(translate_st_rt_rec (st) (base_rt_rec (localN) (lvt)) (localN)) with res=res};;
