(* Actually these mapping modules are the same, I just put them 
  there because I want to ``distinguish'' between the mapping easier *)
  
module Int32Map : Map.S with type key = int32;;
module StringMap : Map.S with type key = string;;
  
type bpMap = int32 Int32Map.t;;
type tsMap = int Int32Map.t;;
type sbMap = bool Int32Map.t;; 
type constantMap = Constant.t Int32Map.t;; (* a map from integer to a constant *)
type saMap = int32 Int32Map.t;; (* scope address map for consolidating code later on *)
type valTypeMap = (bool list) Int32Map.t;; (* a boolean indicating whether a value is constant or not *)
