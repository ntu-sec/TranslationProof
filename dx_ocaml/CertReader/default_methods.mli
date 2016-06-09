module DEX : sig
    val add_default_methods :
      ((Certificate.DEX_Method_cert.t Map_type.StringMap.t) Map_type.StringMap.t) Map_type.StringMap.t
      -> Certificate.Level_pool.t ->
      ((Certificate.DEX_Method_cert.t Map_type.StringMap.t) Map_type.StringMap.t) Map_type.StringMap.t;;

    val add_current_class_methods :
      ((Certificate.DEX_Method_cert.t Map_type.StringMap.t) Map_type.StringMap.t) Map_type.StringMap.t
      -> string -> Certificate.Level_pool.t ->
      ((Certificate.DEX_Method_cert.t Map_type.StringMap.t) Map_type.StringMap.t) Map_type.StringMap.t;;
  end;;

module JVM : sig
    val add_default_methods :
      ((Certificate.JVM_Method_cert.t Map_type.StringMap.t) Map_type.StringMap.t) Map_type.StringMap.t
      -> Certificate.Level_pool.t ->
      ((Certificate.JVM_Method_cert.t Map_type.StringMap.t) Map_type.StringMap.t) Map_type.StringMap.t;;

    val add_current_class_methods :
      ((Certificate.JVM_Method_cert.t Map_type.StringMap.t) Map_type.StringMap.t) Map_type.StringMap.t
      -> string -> Certificate.Level_pool.t ->
      ((Certificate.JVM_Method_cert.t Map_type.StringMap.t) Map_type.StringMap.t) Map_type.StringMap.t;;
  end;;
