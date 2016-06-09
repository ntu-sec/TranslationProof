open Certificate;;

module type Type = sig type t val to_string : t -> string end;;

module type Instruction_type = sig
    type insnType
    type argsType
    type certType
    type t
    val label : t -> int;;
    val label32 : t -> int32;;
    val size : t -> int;;
    val code : t -> insnType;;
    val args : t -> argsType;;
    val typeInfo : t -> certType;;
    val create : int -> int -> insnType -> argsType -> certType -> t
    val to_string : t -> string
    val update_label : int -> t -> t;;
    val update_size : int -> t -> t;;
    val update_label_size : int -> int -> t -> t;;
    val update_typing : certType -> t -> t;;
  end;;

module Instruction = functor (CertType : Certificate.Ins_typing_type) ->
                     functor (ArgsType : Type) -> functor (InsnType : Type) -> struct
                                                      type insnType = InsnType.t
                                                      type argsType = ArgsType.t
                                                      type certType = CertType.t
                                                      type t = { label : int; size : int; code : insnType;
                                                                 args: argsType; typeInfo: certType };;
                                                      let label = fun t -> t.label;;
                                                      let label32 = fun t -> Int32.of_int t.label;;
                                                      let size = fun t -> t.size;;
                                                      let code = fun t -> t.code;;
                                                      let args = fun t -> t.args;;
                                                      let typeInfo = fun t -> t.typeInfo;;
                                                      let create = fun lbl sz code args typeInfo ->
                                                        { label = lbl; size = sz; code = code; args = args; typeInfo = typeInfo };;
                                                      let to_string = fun t ->
                                                        string_of_int(t.label) ^ "(" ^ string_of_int(t.size)^") : "
                                                        ^ InsnType.to_string (t.code) ^ ArgsType.to_string (t.args) ^ " - " ^
                                                          CertType.to_string (t.typeInfo);;

                                                      let update_label = fun label t -> {t with label = label};;
                                                      let update_size = fun size t -> {t with size = size};;
                                                      let update_label_size = fun label size t -> {t with label=label; size=size};;
                                                      let update_typing = fun typing t -> {t with typeInfo = typing};;
                                                    end;;

module JVM = struct

    type valType = Int | Void;;

    type insnType = Nop | Push | Pop | Load | Store | Swap | Goto | If | Return | Binop | IInc
                    | New | Getfield | Putfield | Dup
                    | Invoke of valType (* for now we collapse all the kinds of invoke *)
                    | Throw;;

    let string_of_valType (input: valType) =
      match input with
        Int -> "Int" | Void -> "Void";;

    let string_of_insnType = fun t ->
      match t with | Nop -> "Nop"
              |       Push -> "Push" | Pop -> "Pop" | Load -> "Load" | Store -> "Store"
              | Swap -> "Swap" | Goto -> "Goto" | If -> "If" | Return -> "Return"
              | Binop -> "Binop" | IInc -> "IInc" | New -> "New" | Putfield -> "Putfield"
              | Getfield -> "Getfield" | Dup -> "Dup"
              | Invoke m -> "Invoke " ^ string_of_valType(m)
              | Throw -> "Throw";;

    let char_to_valType (desc: string) =
      let retChar = desc.[(String.index (desc) (')')) + 1] in
      match retChar with
        'V' -> Void
      | 'I' | 'L' -> Int (* For now we collapse the objects and other return values *)
      | _ -> raise (Invalid_argument ("Not a valid return descriptor : " ^ Char.escaped(retChar)))

    type binopKind = Add | Sub | Div | Mul | Mod | And | Or | Xor | Shl | Shr | Ushr;;
    type ifKind = Eq | Ne | Gt | Ge | Lt | Le |
                  Eqz | Nez | Gtz | Gez | Ltz | Lez;;

    (* String Converter *)
    let string_of_binop (input: binopKind) =
      match input with
        Add -> "Add" | Sub -> "Sub" | Div -> "Div" | Mul -> "Mul" | Mod -> "Mod"
        | And -> "And" | Or -> "Or" | Xor -> "Xor" | Shl -> "Shl" | Shr -> "Shr" | Ushr -> "Ushr";;

    let string_of_ifKind (input: ifKind) =
      match input with
        Eq -> "Eq" | Ne -> "Ne" | Gt -> "Gt" | Ge -> "Ge" | Lt -> "Lt" | Le -> "Le" |
        Eqz -> "Eqz" | Nez -> "Nez" | Gtz -> "Gtz" | Gez -> "Gez" | Ltz -> "Ltz" | Lez -> "Lez";;

    module PlainArgs = struct type t = unit
                              let create () = ();;
                              let to_string = fun t -> "";;
                       end

    module ArgsArgs = struct type t = int list
                             let create = fun args -> args;;
                             let to_string = fun t -> Utility.string_of_int_list (t) (fun h -> " " ^ string_of_int (h));;
                             let arg_at = fun i t -> List.nth t i;;
                      end

    module BinopArgs = struct type t = binopKind
                              let create = fun kind -> kind;;
                              let op = fun t -> t;;
                              let to_string = fun t -> " " ^ string_of_binop (t);;
                       end

    module IfArgs = struct type t = { kind: ifKind; offset: int }
                           let kind = fun t -> t.kind;;
                           let offset = fun t -> t.offset;;
                           let create = fun kind offset -> { kind = kind; offset = offset };;
                           let to_string = fun t -> " " ^ string_of_ifKind (kind t) ^ " " ^
                                                      string_of_int (offset t)
                    end

    module GotoArgs = struct type t = int
                             let create = fun offset -> offset;;
                             let to_string = fun t -> " " ^ string_of_int (t);;
                      end

    module ReturnArgs = struct type t = valType
                               let create = fun kind -> kind;;
                               let to_string = fun t -> " " ^ string_of_valType (t);;
                               let retType = fun t -> t;;
                        end

    module ClassArgs = struct type t = { index : int; name : string }
                              let index = fun t -> t.index;;
                              let name = fun t -> t.name;;
                              let create = fun idx nm -> { index = idx; name = nm };;
                              let to_string = fun t -> " " ^ t.name;;
                       end

    module FMIArgs = struct type t = { index : int; class_name: string; name: string; fmi_type: string }
                            let index = fun t -> t.index;;
                            let class_name = fun t -> t.class_name;;
                            let name = fun t -> t.name;;
                            let fmi_type = fun t -> t.fmi_type;;
                            let create = fun idx cn nm fmi_t ->
                              { index = idx; class_name = cn; name = nm; fmi_type = fmi_t }
                            let to_string = fun t -> " " ^ (t.class_name) ^ "." ^ (t.name) ^ ":" ^ (t.fmi_type);;
                     end

    type insArgsType = PlainArgsType of PlainArgs.t | ArgsArgsType of ArgsArgs.t |
                       BinopArgsType of BinopArgs.t | IfArgsType of IfArgs.t | GotoArgsType of GotoArgs.t |
                       ReturnArgsType of ReturnArgs.t | ClassArgsType of ClassArgs.t |
                       FMIArgsType of FMIArgs.t;;

    module JVM_InsnType = struct
        type t = insnType
        let to_string = string_of_insnType
      end;;

    module ArgsPrint = struct type t = insArgsType;;
                              let to_string = fun t -> match t with
                                                       | PlainArgsType x -> PlainArgs.to_string x
                                                       | ArgsArgsType x -> ArgsArgs.to_string x
                                                       | BinopArgsType x -> BinopArgs.to_string x
                                                       | IfArgsType x -> IfArgs.to_string x
                                                       | GotoArgsType x -> GotoArgs.to_string x
                                                       | ReturnArgsType x -> ReturnArgs.to_string x
                                                       | ClassArgsType x -> ClassArgs.to_string x
                                                       | FMIArgsType x -> FMIArgs.to_string x;;
                       end;;

    module JVM_Instruction =
      Instruction (Certificate.JVM_ins_typing) (ArgsPrint) (JVM_InsnType);;

    let getPlainArgs = fun t -> match JVM_Instruction.args t with
                                | PlainArgsType args -> args
                                | _ -> match JVM_Instruction.code t with
                                       | Nop | Throw | Dup | Swap | Pop | Return ->
                                                                           (raise (Invalid_argument "error:Non-Plain args type for a plain instruction"))
                                       | _ -> (raise (Invalid_argument "error:Attempting to get a plain args type from a non-plain instruction"));;

    let getArgsArgs = fun t -> match JVM_Instruction.args t with
                               | ArgsArgsType args -> args
                               | _ -> match JVM_Instruction.code t with
                                      | Store | Load | Push | IInc ->
                                                               (raise (Invalid_argument "error:Non-Args args type for an args instruction"))
                                      | _ -> (raise (Invalid_argument "error:Attempting to get an Args args type from a non-Args instruction"));;

    let getBinopArgs = fun t -> match JVM_Instruction.args t with
                                | BinopArgsType args -> args
                                | _ -> match JVM_Instruction.code t with
                                       | Binop ->
                                          (raise (Invalid_argument "error:Non-Binop args type for a Binop instruction"))
                                       | _ -> (raise (Invalid_argument "error:Attempting to get a Binop type from a non-Binop instruction"));;

    let getIfArgs = fun t -> match JVM_Instruction.args t with
                             | IfArgsType args -> args
                             | _ -> match JVM_Instruction.code t with
                                    | If ->
                                       (raise (Invalid_argument "error:Non-If args type for an If instruction"))
                                    | _ -> (raise (Invalid_argument "error:Attempting to get an If type from a non-If instruction"));;

    let getGotoArgs = fun t -> match JVM_Instruction.args t with
                               | GotoArgsType args -> args
                               | _ -> match JVM_Instruction.code t with
                                      | Goto ->
                                         (raise (Invalid_argument "error:Non-Goto args type for a Goto instruction"))
                                      | _ -> (raise (Invalid_argument "error:Attempting to get a Goto type from a non-Goto instruction"));;

    let getReturnArgs = fun t -> match JVM_Instruction.args t with
                                 | ReturnArgsType args -> args
                                 | _ -> match JVM_Instruction.code t with
                                        | Return ->
                                           (raise (Invalid_argument "error:Non-Return args type for a Return instruction"))
                                        | _ -> (raise (Invalid_argument "error:Attempting to get a Return type from a non-Return instruction"));;

    let getClassArgs = fun t -> match JVM_Instruction.args t with
                                | ClassArgsType args -> args
                                | _ -> match JVM_Instruction.code t with
                                       | New ->
                                          (raise (Invalid_argument "error:Non-Class args type for a Class instruction"))
                                       | _ -> (raise (Invalid_argument "error:Attempting to get a plain Class type from a non-Class instruction"));;

    let getFMIArgs = fun t -> match JVM_Instruction.args t with
                              | FMIArgsType args -> args
                              | _ -> match JVM_Instruction.code t with
                                     | Getfield | Putfield ->
                                                   (raise (Invalid_argument "error:Non-FMI args type for a FMI instruction"))
                                     | Invoke x -> (raise (Invalid_argument "error:Non-FMI args type for a FMI instruction"))
                                     | _ -> (raise (Invalid_argument "error:Attempting to get a plain FMI type from a non-FMI instruction"));;

    let createNopInsn = fun lbl sz typing -> JVM_Instruction.create lbl sz Nop
                                                                    (PlainArgsType (PlainArgs.create ())) typing;;
    let createPushInsn = fun lbl sz value typing -> JVM_Instruction.create lbl sz
                                                                           Push (ArgsArgsType (ArgsArgs.create [value])) typing;;
    let createBinopInsn = fun lbl sz opcode typing -> let op = match opcode with
                                                        | "60" -> Add | "64" -> Sub | "6C" -> Div | "68" -> Mul | "70" -> Mod
                                                        | "78" -> Shl | "7A" -> Shr | "7C" -> Ushr | "7E" -> And | "80" -> Or
                                                        | "82" -> Xor | _ -> raise (Invalid_argument "Bogus opcode")
                                                      in
                                                      JVM_Instruction.create lbl sz Binop (BinopArgsType (BinopArgs.create op)) typing;;
    let createLoadInsn = fun lbl sz value typing -> JVM_Instruction.create lbl sz
                                                                           Load (ArgsArgsType (ArgsArgs.create [value])) typing;;
    let createStoreInsn = fun lbl sz value typing -> JVM_Instruction.create lbl sz
                                                                            Store (ArgsArgsType (ArgsArgs.create [value])) typing;;
    let createPopInsn = fun lbl sz typing -> JVM_Instruction.create lbl sz
                                                                    Pop (PlainArgsType (PlainArgs.create ())) typing;;
    let createSwapInsn = fun lbl sz typing -> JVM_Instruction.create lbl sz
                                                                     Swap (PlainArgsType (PlainArgs.create ())) typing;;
    let createIfInsn = fun lbl sz offset opcode typing ->
      let ifType = match opcode with
          "9F" -> Eq | "A0" -> Ne | "A1" -> Lt | "A2" -> Ge | "A3" -> Gt |
          "A4" -> Le | "99" -> Eqz | "9A" -> Nez | "9B" -> Ltz | "9C" -> Gez |
          "9D" -> Gtz | "9E" -> Lez | _ -> raise (Invalid_argument "Bogus opcode") in
      JVM_Instruction.create lbl sz If (IfArgsType (IfArgs.create ifType offset)) typing;;
    let createGotoInsn = fun lbl sz offset typing -> JVM_Instruction.create lbl sz
                                                                            Goto (GotoArgsType (GotoArgs.create offset)) typing;;
    let createReturnInsn = fun lbl sz sgn typing -> let retType = match sgn with
                                                        "AC" -> Int | "B1" -> Void | _ -> raise (Invalid_argument "Bogus opcode") in
                                                    JVM_Instruction.create lbl sz Return (ReturnArgsType (ReturnArgs.create retType)) typing;;
    let createNewInsn = fun lbl sz classIndex className typing -> JVM_Instruction.create lbl sz
                                                                                         New (ClassArgsType (ClassArgs.create classIndex className)) typing;;
    let createFieldInsn = fun lbl sz opcode field_index className natName desc typing ->
      let op = match opcode with "B4" -> Getfield | "B5" -> Putfield | _ -> raise (Invalid_argument "Bogus opcode") in
      JVM_Instruction.create lbl sz op (FMIArgsType (FMIArgs.create field_index className natName desc))
                             typing;;
    let createInvokeInsn = fun lbl sz desc method_index className natName desc typing ->
      let retType = char_to_valType (desc) in
      JVM_Instruction.create lbl sz (Invoke retType) (FMIArgsType (FMIArgs.create method_index className natName desc))
                             typing;;
    let createThrowInsn = fun lbl sz typing -> JVM_Instruction.create lbl sz
                                                                      Throw (PlainArgsType (PlainArgs.create ())) typing;;
    let createIincInsn = fun lbl sz value1 value2 typing -> JVM_Instruction.create lbl sz
                                                                                   IInc (ArgsArgsType (ArgsArgs.create [value1; value2])) typing;;
    let createDupInsn = fun lbl sz typing -> JVM_Instruction.create lbl sz
                                                                    Dup (PlainArgsType (PlainArgs.create ())) typing;;
  end

module DEX = struct
    type valType = Int | Void;;
    type fieldOp = Get | Put ;;
    type insnType = Nop | Const | Move | Goto | If | Ifz | Return | Binop | BinopConst
                    | New | Field of fieldOp | Invoke | MoveResult | MoveException
                    | Throw;;
    type binopKind = Add | Sub | Div | Mul | Mod | And | Or | Xor | Shl | Shr | Ushr;;
    (* The if type will be distinguished by its argument type *)
    type ifKind = Eq | Ne | Gt | Ge | Lt | Le;;

    (* String Converter *)
    let string_of_valType (input: valType) =
      match input with
        Int -> "Int" | Void -> "Void";;

    let string_of_binop (input: binopKind) =
      match input with
        Add -> "Add" | Sub -> "Sub" | Div -> "Div" | Mul -> "Mul" | Mod -> "Mod"
        | And -> "And" | Or -> "Or" | Xor -> "Xor" | Shl -> "Shl" | Shr -> "Shr" | Ushr -> "Ushr";;

    let string_of_ifKind (input: ifKind) =
      match input with
        Eq -> "Eq" | Ne -> "Ne" | Gt -> "Gt" | Ge -> "Ge" | Lt -> "Lt" | Le -> "Le";;

    let string_of_insnType (input: insnType) =
      match input with | Nop -> "Nop"
                  |       Const -> "Const" | Move -> "Move" | Goto -> "Goto" | If -> "If" | Ifz -> "Ifz"
                  | Return -> "Return" | Binop -> "Binop" | BinopConst -> "BinopConst" | New -> "New"
                  | Field x -> (match x with Put -> "Iput" | Get -> "Iget")
                  | MoveResult -> "MoveResult" | Invoke -> "Invoke"
                  | MoveException -> "MoveException" | Throw -> "Throw";;

    module NopArgs = struct type t = unit;;
                            let to_string = fun t -> "";;
                            let create () = ();;
                     end;;

    module ConstArgs = struct
        type t = {reg:int; value:int};;
        let create = fun reg value -> {reg = reg; value = value};;
        let reg = fun t -> t.reg;;
        let value = fun t -> t.value;;
        let to_string = fun t -> "v"^string_of_int(t.reg)^", #"^string_of_int(t.value);;
      end;;

    module ReturnArgs = struct
        type t = { retType : valType; retReg : int };;
        let create = fun retType retReg -> { retType = retType; retReg = retReg };;
        let retType = fun t -> t.retType;;
        let retReg = fun t -> t.retReg;;
        let to_string = fun t -> string_of_valType (t.retType) ^ " v" ^ string_of_int (t.retReg);;
      end;;

    module MoveArgs = struct
        type t = { target : int; source : int };;
        let create = fun target source -> { target = target; source = source };;
        let target = fun t -> t.target;;
        let source = fun t -> t.source;;
        let to_string = fun t -> "(v"^string_of_int (t.target)^", v"^string_of_int(t.source)^")";;
      end;;

    module BinopArgs = struct
        type t = { op: binopKind; target: int; source1: int; source2: int };;
        let create = fun op target source1 source2 ->
          { op = op; target = target; source1 = source1; source2 = source2 };;
        let op = fun t -> t.op;;
        let target = fun t -> t.target;;
        let source1 = fun t -> t.source1;;
        let source2 = fun t -> t.source2;;
        let to_string = fun t -> "(" ^ string_of_binop (t.op) ^ "-v"^string_of_int(t.target)^
                                   ", v"^string_of_int(t.source1)^", v"^string_of_int(t.source2);;
      end;;

    module BinopConstArgs = struct
        type t = { op: binopKind; target: int; source: int; const: int };;
        let create = fun op target source const ->
          { op = op; target = target; source = source; const = const };;
        let target = fun t -> t.target;;
        let source = fun t -> t.source;;
        let const = fun t -> t.const;;
        let to_string = fun t -> "(" ^ string_of_binop (t.op) ^ "-"^string_of_int(t.target)^
                                   ", v"^string_of_int(t.source)^", #"^string_of_int(t.const);;
      end;;

    let get_OppositeIf (inp: ifKind) =
      match inp with
        Eq -> Ne
      | Ne -> Eq
      | Gt -> Le
      | Ge -> Lt
      | Lt -> Ge
      | Le -> Gt;;

    module IfArgs = struct
        type t = { kind: ifKind; offset: int; register1: int; register2: int };;
        let kind = fun t -> t.kind;;
        let offset = fun t -> t.offset;;
        let register1 = fun t -> t.register1;;
        let register2 = fun t -> t.register2;;
        let create = fun kind register1 register2 offset->
          { kind = kind; offset = offset; register1 = register1; register2 = register2 };;
        let to_string = fun t -> string_of_ifKind (t.kind) ^ " (" ^
                                   string_of_int(t.register1)^", "^string_of_int(t.register2)^", #"^string_of_int (t.offset)^")";;
      end

    module IfzArgs = struct
        type t = { kind: ifKind; offset: int; register: int };;
        let kind = fun t -> t.kind;;
        let offset = fun t -> t.offset;;
        let register = fun t -> t.register;;
        let create = fun kind register offset ->
          { kind = kind; offset = offset; register = register };;
        let to_string = fun t -> string_of_ifKind (t.kind) ^ "z (" ^
                                   string_of_int(t.register)^", #"^string_of_int (t.offset)^")";;
      end

    module GotoArgs = struct
        type t = int
        let create = fun offset -> offset;;
        let offset = fun t -> t;;
        let to_string = fun t -> string_of_int (t);;
      end

    module TargetArgs = struct
        type t = int
        let create = fun reg -> reg;;
        let to_string = fun t -> "v"^string_of_int (t);;
      end

    module ClassArgs = struct
        type t = { target : int; name : string }
        let target = fun t -> t.target;;
        let name = fun t -> t.name;;
        let create = fun target name -> { target = target; name = name };;
        let to_string = fun t -> "v" ^string_of_int(t.target)^", "^t.name;;
      end

    module FieldArgs = struct
        type t = { op: fieldOp; valReg : int; objectReg: int; fieldName: string }
        let op = fun t -> t.op;;
        let valReg = fun t -> t.valReg;;
        let objectReg = fun t -> t.objectReg;;
        let fieldName = fun t -> t.fieldName;;
        let create = fun op valReg objectReg fieldName ->
          { op = op; valReg = valReg; objectReg = objectReg; fieldName = fieldName }
        let to_string = fun t -> let opStr = match t.op with Get -> "Get" | Put -> "Put" in
                                 opStr^" (v" ^ string_of_int (t.valReg) ^ ", "
                                 ^ string_of_int(t.objectReg) ^ ", " ^ (t.fieldName)^")";;
      end

    module MethodArgs = struct
        type t = { argNum: int; params: int list; name: string };;
        let argNum = fun t -> t.argNum;;
        let params = fun t -> t.params;;
        let name = fun t -> t.name;;
        let create = fun argNum params name ->
          { argNum = argNum; params = params; name = name };;
        let to_string = fun t -> "("^t.name^", "^string_of_int(t.argNum)^")";;
          (* TODO : string for parameters *)
      end;;

    type insArgsType = NopArgsType of NopArgs.t| ConstArgsType of ConstArgs.t |
                       ReturnArgsType of ReturnArgs.t | MoveArgsType of MoveArgs.t |
                       BinopArgsType of BinopArgs.t | BinopConstArgsType of BinopConstArgs.t |
                       IfArgsType of IfArgs.t | IfzArgsType of IfzArgs.t |
                       GotoArgsType of GotoArgs.t | ClassArgsType of ClassArgs.t |
                       FieldArgsType of FieldArgs.t | InvokeArgsType of MethodArgs.t |
                       TargetArgsType of TargetArgs.t;;

    module ArgsPrint = struct type t = insArgsType;;
                              let to_string = fun t -> match t with
                                                       | NopArgsType x -> NopArgs.to_string x
                                                       | ConstArgsType x -> ConstArgs.to_string x
                                                       | MoveArgsType x -> MoveArgs.to_string x
                                                       | BinopArgsType x -> BinopArgs.to_string x
                                                       | BinopConstArgsType x -> BinopConstArgs.to_string x
                                                       | IfArgsType x -> IfArgs.to_string x
                                                       | IfzArgsType x -> IfzArgs.to_string x
                                                       | GotoArgsType x -> GotoArgs.to_string x
                                                       | ReturnArgsType x -> ReturnArgs.to_string x
                                                       | ClassArgsType x -> ClassArgs.to_string x
                                                       | FieldArgsType x -> FieldArgs.to_string x
                                                       | InvokeArgsType x -> MethodArgs.to_string x
                                                       | TargetArgsType x -> TargetArgs.to_string x
                       end;;

    module DEX_InsnType = struct
        type t = insnType
        let to_string = string_of_insnType
      end;;

    module DEX_Instruction =
      Instruction (Certificate.DEX_ins_typing) (ArgsPrint) (DEX_InsnType);;

    let createNopInsn = fun lbl sz se rt ->
      DEX_Instruction.create lbl sz Nop (NopArgsType (NopArgs.create ()))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createConstInsn = fun lbl sz reg value se rt ->
      DEX_Instruction.create lbl sz Const (ConstArgsType (ConstArgs.create reg value))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createMoveInsn = fun lbl sz target source se rt ->
      DEX_Instruction.create lbl sz Move (MoveArgsType (MoveArgs.create target source))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createReturnInsn = fun lbl sz retType se rt ->
      DEX_Instruction.create lbl sz Return (ReturnArgsType (ReturnArgs.create retType 0))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createMoveExceptionInsn = fun lbl sz reg se rt ->
      DEX_Instruction.create lbl sz MoveException (TargetArgsType (TargetArgs.create reg))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createMoveResultInsn = fun lbl sz reg se rt ->
      DEX_Instruction.create lbl sz MoveResult (TargetArgsType (TargetArgs.create reg))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createThrowInsn = fun lbl sz reg se rt ->
      DEX_Instruction.create lbl sz Throw (TargetArgsType (TargetArgs.create reg))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createBinopInsn = fun lbl sz op target source1 source2 se rt ->
      DEX_Instruction.create lbl sz Binop (BinopArgsType (BinopArgs.create op target source1 source2))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createBinopConstInsn = fun lbl sz op target source value se rt ->
      DEX_Instruction.create lbl sz BinopConst (BinopConstArgsType (BinopConstArgs.create op target source value))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createIfInsn = fun lbl sz kind reg1 reg2 offset se rt ->
      DEX_Instruction.create lbl sz If (IfArgsType (IfArgs.create kind reg1 reg2 offset))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createIfzInsn = fun lbl sz kind reg offset se rt ->
      DEX_Instruction.create lbl sz Ifz (IfzArgsType (IfzArgs.create kind reg offset))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createGotoInsn = fun lbl sz offset se rt ->
      DEX_Instruction.create lbl sz Goto (GotoArgsType (GotoArgs.create offset))
                             (Certificate.DEX_ins_typing.create se rt);;

    let createNewInsn = fun lbl sz reg cn se rt ->
      DEX_Instruction.create lbl sz New (ClassArgsType (ClassArgs.create reg cn))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createGetfieldInsn = fun lbl sz target obj field se rt ->
      DEX_Instruction.create lbl sz (Field Get) (FieldArgsType (FieldArgs.create Get target obj field))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createPutfieldInsn = fun lbl sz source obj field se rt ->
      DEX_Instruction.create lbl sz (Field Put) (FieldArgsType (FieldArgs.create Put source obj field))
                             (Certificate.DEX_ins_typing.create se rt);;
    let createInvokeInsn = fun lbl sz argNum params name se rt ->
      DEX_Instruction.create lbl sz (Invoke) (InvokeArgsType (MethodArgs.create argNum params name))
                             (Certificate.DEX_ins_typing.create se rt);;

    let getConstArgs = fun t -> match DEX_Instruction.args t with
                                | ConstArgsType args -> args
                                | _ -> match DEX_Instruction.code t with
                                       | Const ->
                                          (raise (Invalid_argument "error:Non-Const args type for a Const instruction"))
                                       | _ -> (raise (Invalid_argument "error:Attempting to get a Const type from a non-Const instruction"));;

    let getMoveArgs = fun t -> match DEX_Instruction.args t with
                               | MoveArgsType args -> args
                               | _ -> match DEX_Instruction.code t with
                                      | Move ->
                                         (raise (Invalid_argument "error:Non-Move args type for a Move instruction"))
                                      | _ -> (raise (Invalid_argument "error:Attempting to get a Move type from a non-Move instruction"));;

    let getBinopArgs = fun t -> match DEX_Instruction.args t with
                                | BinopArgsType args -> args
                                | _ -> match DEX_Instruction.code t with
                                       | Binop ->
                                          (raise (Invalid_argument "error:Non-Binop args type for a Binop instruction"))
                                       | _ -> (raise (Invalid_argument "error:Attempting to get an Binop type from a non-Binop instruction"));;

    let getBinopConstArgs = fun t -> match DEX_Instruction.args t with
                                     | BinopConstArgsType args -> args
                                     | _ -> match DEX_Instruction.code t with
                                            | BinopConst ->
                                               (raise (Invalid_argument "error:Non-BinopConst args type for a BinopConst instruction"))
                                            | _ -> (raise (Invalid_argument "error:Attempting to get a BinopConst type from a non-BinopConst instruction"));;

    let getIfArgs = fun t -> match DEX_Instruction.args t with
                             | IfArgsType args -> args
                             | _ -> match DEX_Instruction.code t with
                                    | If ->
                                       (raise (Invalid_argument "error:Non-If args type for an If instruction"))
                                    | _ -> (raise (Invalid_argument "error:Attempting to get an If type from a non-If instruction"));;

    let getIfzArgs = fun t -> match DEX_Instruction.args t with
                              | IfzArgsType args -> args
                              | _ -> match DEX_Instruction.code t with
                                     | Ifz ->
                                        (raise (Invalid_argument "error:Non-Ifz args type for an Ifz instruction"))
                                     | _ -> (raise (Invalid_argument "error:Attempting to get an Ifz type from a non-Ifz instruction"));;

    let getGotoArgs = fun t -> match DEX_Instruction.args t with
                               | GotoArgsType args -> args
                               | _ -> match DEX_Instruction.code t with
                                      | Goto ->
                                         (raise (Invalid_argument "error:Non-Goto args type for an Goto instruction"))
                                      | _ -> (raise (Invalid_argument "error:Attempting to get an Goto type from a non-Goto instruction"));;

    let opposite_if_insn (inp:DEX_Instruction.t) (offset:int) =
      let arg = getIfArgs (inp) in
      let typeInfo = DEX_Instruction.typeInfo inp in
      createIfInsn (DEX_Instruction.label inp) (DEX_Instruction.size inp)
                   (get_OppositeIf (IfArgs.kind arg)) (IfArgs.register1 arg)
                   (IfArgs.register2 arg) (offset) (DEX_ins_typing.se typeInfo) (DEX_ins_typing.typing typeInfo);;

    let opposite_ifz_insn (inp:DEX_Instruction.t) (offset:int) =
      let arg = getIfzArgs (inp) in
      let typeInfo = DEX_Instruction.typeInfo inp in
      createIfzInsn (DEX_Instruction.label inp) (DEX_Instruction.size inp)
                    (get_OppositeIf (IfzArgs.kind arg)) (IfzArgs.register arg)
                    (offset) (DEX_ins_typing.se typeInfo) (DEX_ins_typing.typing typeInfo);;

    let fixGotoTarget (inp:DEX_Instruction.t) (offset:int) =
      let typeInfo = DEX_Instruction.typeInfo inp in
      createGotoInsn (DEX_Instruction.label inp) (DEX_Instruction.size inp)
                     (offset - (DEX_Instruction.label inp)) (DEX_ins_typing.se typeInfo) (DEX_ins_typing.typing typeInfo);;

    let fixIfTarget (inp:DEX_Instruction.t) (offset:int) =
      let arg = getIfArgs (inp) in
      let typeInfo = DEX_Instruction.typeInfo inp in
      createIfInsn (DEX_Instruction.label inp) (DEX_Instruction.size inp)
                   (IfArgs.kind arg) (IfArgs.register1 arg) (IfArgs.register2 arg)
                   (offset - DEX_Instruction.label inp) (DEX_ins_typing.se typeInfo) (DEX_ins_typing.typing typeInfo);;

    let fixIfzTarget (inp:DEX_Instruction.t) (offset:int) =
      let arg = getIfzArgs (inp) in
      let typeInfo = DEX_Instruction.typeInfo inp in
      createIfzInsn (DEX_Instruction.label inp) (DEX_Instruction.size inp)
                    (IfzArgs.kind arg) (IfzArgs.register arg)
                    (offset - DEX_Instruction.label inp) (DEX_ins_typing.se typeInfo) (DEX_ins_typing.typing typeInfo);;
  end

let map_ifKind (kind: JVM.ifKind) =
  match kind with
    JVM.Eq -> DEX.Eq
  | JVM.Ne -> DEX.Ne
  | JVM.Gt -> DEX.Gt
  | JVM.Ge -> DEX.Ge
  | JVM.Lt -> DEX.Lt
  | JVM.Le -> DEX.Le
  | JVM.Eqz -> DEX.Eq
  | JVM.Nez -> DEX.Ne
  | JVM.Gtz -> DEX.Gt
  | JVM.Gez -> DEX.Ge
  | JVM.Ltz -> DEX.Lt
  | JVM.Lez -> DEX.Le
;;

let map_binopKind (kind: JVM.binopKind) =
  match kind with
    JVM.Add -> DEX.Add
  | JVM.Sub -> DEX.Sub
  | JVM.Mul -> DEX.Mul
  | JVM.Div -> DEX.Div
  | JVM.Mod -> DEX.Mod
  | JVM.And -> DEX.And
  | JVM.Or -> DEX.Or
  | JVM.Xor -> DEX.Xor
  | JVM.Shl -> DEX.Shl
  | JVM.Shr -> DEX.Shr
  | JVM.Ushr -> DEX.Ushr
;;
