(* Actually these mapping modules are the same, I just put them 
  there because I want to ``distinguish'' between the mapping easier *)

module Int32Map = Map.Make(Int32);;
module StringMap = Map.Make(String);;

type bpMap = int32 Int32Map.t;;
type tsMap = int Int32Map.t;;
type sbMap = bool Int32Map.t;;  
type constantMap = Constant.t Int32Map.t;;
type saMap = int32 Int32Map.t;; 
type valTypeMap = (bool list) Int32Map.t;; 
