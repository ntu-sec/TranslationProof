module ClassConstant : sig
	type t
	val idx : t -> int
	val name : t -> string
	val to_string : t -> string
	val create : int -> t
	val update_ref : string -> t -> t
	val empty : t
end;;

module NATConstant : sig
	type t
	val name_idx : t -> int
	val desc_idx : t -> int
	val name : t -> string
	val desc : t -> string
	val to_string : t -> string
	val create : int -> int -> t
	val update_ref : string -> string -> t -> t
	val empty : t
end;;

module FMIConstant : sig
	type refType 
	type t
  val fmi_type : t -> refType
  val class_idx : t -> int
  val nat_idx : t -> int
  val classConstant : t -> ClassConstant.t
  val natConstant : t -> NATConstant.t
  val to_string : t -> string
  val create : refType -> int -> int -> t
	val update_ref : ClassConstant.t -> NATConstant.t -> t -> t
end;;

module StringConstant : sig
	type t
  val idx : t -> int
  val str : t -> string
  val to_string : t -> string
  val create : int -> t
	val update_ref : string -> t -> t
end;;

module ValueConstant : sig 
	type t
  val to_string : t -> string
  val create : int -> t
	val value : t -> int
end;;

module UTF8Constant : sig
	type t
  val length : t -> int
  val bytes : t -> int list
  val utf_string : t -> string
  val to_string : t -> string
  val create : int -> int list -> string -> t
end;;

type t =
		Class of ClassConstant.t
	| NAT of NATConstant.t
	| FMI of FMIConstant.t
	| String of StringConstant.t
	| Value of ValueConstant.t
	| UTF8 of UTF8Constant.t;;
val to_string : t -> string;;

val newConstantClass : int -> t
val newConstantNat : int -> int -> t
val newConstantFieldRef : int -> int -> t 
val newConstantMethodRef : int -> int -> t
val newConstantInterfaceMethodRef : int -> int -> t
val newConstantString : int -> t
val newConstantValue : int -> t
val newConstantUtf : int -> int list -> string -> t

val getConstantUtf : int -> (int -> t) -> UTF8Constant.t;;
val getConstantClass : int -> (int -> t) -> ClassConstant.t;;
val getConstantValue : int -> (int -> t) -> ValueConstant.t;;
