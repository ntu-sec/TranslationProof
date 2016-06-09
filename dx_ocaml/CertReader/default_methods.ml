open Certificate;;
open Map_type;;

module DEX = struct 
let java_lang_Object_init (lvl_pool:Level_pool.t) = 
	DEX_Method_cert.create ("Ljava/lang/Object;") ("<init>") ("V") (Region.empty) (Junction.empty)
  (Int32Map.add (0l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let android_support_v7_app_AppCompatActivity_onCreate (lvl_pool:Level_pool.t) =
	DEX_Method_cert.create ("Landroid/support/v7/app/AppCompatActivity;") ("onCreate") ("VL") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let android_support_v7_app_AppCompatActivity_init (lvl_pool:Level_pool.t) =
	DEX_Method_cert.create ("Landroid/support/v7/app/AppCompatActivity;") ("<init>") ("V") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let java_lang_StringBuilder_init (lvl_pool:Level_pool.t) =
	DEX_Method_cert.create ("Ljava/lang/StringBuilder;") ("<init>") ("V") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let java_lang_StringBuilder_append (lvl_pool:Level_pool.t) =
	DEX_Method_cert.create ("Ljava/lang/StringBuilder;") ("append") ("LL") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (1) ([[0]]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (1) ([[1]]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let java_lang_StringBuilder_toString (lvl_pool:Level_pool.t) =
	DEX_Method_cert.create ("Ljava/lang/StringBuilder;") ("toString") ("L") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[0]]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[1]]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let android_widget_EditText_getText (lvl_pool:Level_pool.t) =
	DEX_Method_cert.create ("Landroid/widget/EditText;") ("getText") ("L") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[0]]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[1]]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let java_lang_Object_toString (lvl_pool:Level_pool.t) =
	DEX_Method_cert.create ("Ljava/lang/Object;") ("toString") ("L") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[0]]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[1]]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let android_widget_TextView_setText (lvl_pool:Level_pool.t) =
	DEX_Method_cert.create ("Landroid/widget/TextView;") ("setText") ("VL") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let add_method (cn:string) (method_name:string) (desc:string) (content:DEX_Method_cert.t) 
    (m:((DEX_Method_cert.t StringMap.t) StringMap.t) StringMap.t) =
	(* just assume no method, since the map will be replaced anyway *)
	let class_map = match StringMap.mem cn m with
	  | true -> (let methodsMap = StringMap.find cn m in
		    let descs_map = match StringMap.mem method_name methodsMap with
				  | true -> let descsMap = StringMap.find method_name methodsMap in
				      (StringMap.add (desc) (content) (descsMap))
					| false -> (StringMap.add (desc) (content) (StringMap.empty))
				in StringMap.add (method_name) (descs_map) (methodsMap))
		| false -> (StringMap.add (method_name) 
	    (StringMap.add (desc) (content) StringMap.empty) StringMap.empty)
	in
	  StringMap.add (cn) (class_map) m;;	

let default_method_list (lvl_pool:Level_pool.t) = 
	[android_support_v7_app_AppCompatActivity_onCreate (lvl_pool) ;
	android_support_v7_app_AppCompatActivity_init (lvl_pool);
	java_lang_StringBuilder_init (lvl_pool);
	java_lang_StringBuilder_append (lvl_pool);
	java_lang_StringBuilder_toString (lvl_pool);
	android_widget_EditText_getText (lvl_pool);
	android_widget_TextView_setText (lvl_pool);
	java_lang_Object_init (lvl_pool);
	java_lang_Object_toString (lvl_pool)];;

let add_default_methods (m:((DEX_Method_cert.t StringMap.t) StringMap.t) StringMap.t) 
    (lvl_pool:Level_pool.t) =
	List.fold_right (fun current_method m' -> 
	add_method (DEX_Method_cert.class_name current_method) (DEX_Method_cert.name current_method) 
	  (DEX_Method_cert.descriptor current_method) (current_method) (m'))
		(default_method_list lvl_pool) (m);;

let setContentView (cn:string) (lvl_pool:Level_pool.t) =
	DEX_Method_cert.create (cn) ("setContentView") ("VI") (Region.empty) (Junction.empty)
  (Int32Map.add (0l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let findViewById (cn:string) (lvl_pool:Level_pool.t) =
	DEX_Method_cert.create (cn) ("findViewById") ("LI") (Region.empty) (Junction.empty)
  (Int32Map.add (0l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (1) ([[0]]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (DEX_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (1) ([[1]]) lvl_pool) 
		(DEX_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let current_class_method_list (cn:string) (lvl_pool:Level_pool.t) =
	[setContentView (cn) (lvl_pool); findViewById (cn) (lvl_pool)];;

let add_current_class_methods (m:((DEX_Method_cert.t StringMap.t) StringMap.t) StringMap.t) 
    (cn:string) (lvl_pool:Level_pool.t) =
	List.fold_right (fun current_method m' -> 
	add_method (DEX_Method_cert.class_name current_method) (DEX_Method_cert.name current_method) 
	  (DEX_Method_cert.descriptor current_method) (current_method) (m'))
		(current_class_method_list cn lvl_pool) (m);;	
end;;

module JVM = struct 
let java_lang_Object_init (lvl_pool:Level_pool.t) = 
	JVM_Method_cert.create ("java/lang/Object") ("<init>") ("()V") (Region.empty) (Junction.empty)
  (Int32Map.add (0l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let android_support_v7_app_AppCompatActivity_onCreate (lvl_pool:Level_pool.t) =
	JVM_Method_cert.create ("android/support/v7/app/AppCompatActivity") ("onCreate") ("(Landroid/os/Bundle;)V") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let android_support_v7_app_AppCompatActivity_init (lvl_pool:Level_pool.t) =
	JVM_Method_cert.create ("android/support/v7/app/AppCompatActivity") ("<init>") ("()V") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let java_lang_StringBuilder_init (lvl_pool:Level_pool.t) =
	JVM_Method_cert.create ("java/lang/StringBuilder") ("<init>") ("()V") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let java_lang_StringBuilder_append (lvl_pool:Level_pool.t) =
	JVM_Method_cert.create ("java/lang/StringBuilder") ("append") ("(Ljava/lang/String;)Ljava/lang/StringBuilder;") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (1) ([[0]]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (1) ([[1]]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let java_lang_StringBuilder_toString (lvl_pool:Level_pool.t) =
	JVM_Method_cert.create ("java/lang/StringBuilder") ("toString") ("()Ljava/lang/String;") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[0]]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[1]]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let android_widget_EditText_getText (lvl_pool:Level_pool.t) =
	JVM_Method_cert.create ("android/widget/EditText") ("getText") ("()Landroid/text/Editable;") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[0]]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[1]]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let java_lang_Object_toString (lvl_pool:Level_pool.t) =
	JVM_Method_cert.create ("java/lang/Object") ("toString") ("()Ljava/lang/String;") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[0]]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (1) ([[1]]) (1) (1) ([[1]]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let android_widget_TextView_setText (lvl_pool:Level_pool.t) =
	JVM_Method_cert.create ("android/widget/TextView") ("setText") ("(Ljava/lang/CharSequence;)V") 
	(Region.empty) (Junction.empty)
  (Int32Map.add (0l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let add_method (cn:string) (method_name:string) (desc:string) (content:JVM_Method_cert.t) 
    (m:((JVM_Method_cert.t StringMap.t) StringMap.t) StringMap.t) =
	(* just assume no method, since the map will be replaced anyway *)
	let class_map = match StringMap.mem cn m with
	  | true -> (let methodsMap = StringMap.find cn m in
		    let descs_map = match StringMap.mem method_name methodsMap with
				  | true -> let descsMap = StringMap.find method_name methodsMap in
				      (StringMap.add (desc) (content) (descsMap))
					| false -> (StringMap.add (desc) (content) (StringMap.empty))
				in StringMap.add (method_name) (descs_map) (methodsMap))
		| false -> (StringMap.add (method_name) 
	    (StringMap.add (desc) (content) StringMap.empty) StringMap.empty)
	in
	  StringMap.add (cn) (class_map) m;;	

let default_method_list (lvl_pool:Level_pool.t) = 
	[android_support_v7_app_AppCompatActivity_onCreate (lvl_pool) ;
	android_support_v7_app_AppCompatActivity_init (lvl_pool);
	java_lang_StringBuilder_init (lvl_pool);
	java_lang_StringBuilder_append (lvl_pool);
	java_lang_StringBuilder_toString (lvl_pool);
	android_widget_EditText_getText (lvl_pool);
	android_widget_TextView_setText (lvl_pool);
	java_lang_Object_init (lvl_pool);
	java_lang_Object_toString (lvl_pool)];;

let add_default_methods (m:((JVM_Method_cert.t StringMap.t) StringMap.t) StringMap.t) 
    (lvl_pool:Level_pool.t) =
	List.fold_right (fun current_method m' -> 
	add_method (JVM_Method_cert.class_name current_method) (JVM_Method_cert.name current_method) 
	  (JVM_Method_cert.descriptor current_method) (current_method) (m'))
		(default_method_list lvl_pool) (m);;

let setContentView (cn:string) (lvl_pool:Level_pool.t) =
	JVM_Method_cert.create (cn) ("setContentView") ("(I)V") (Region.empty) (Junction.empty)
  (Int32Map.add (0l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (0) ([]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let findViewById (cn:string) (lvl_pool:Level_pool.t) =
	JVM_Method_cert.create (cn) ("findViewById") ("(I)Landroid/view/View;") (Region.empty) (Junction.empty)
  (Int32Map.add (0l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 0) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (1) ([[0]]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty))
	(Int32Map.add (1l) (JVM_Method_cert.Item.create 
	  (Level_pool.level lvl_pool 1) (MethodPolicy.create_policy (2) ([[1];[1]]) (1) (1) ([[1]]) lvl_pool) 
		(JVM_BytecodeMethod_cert.empty)) Int32Map.empty)  );;

let current_class_method_list (cn:string) (lvl_pool:Level_pool.t) =
	[setContentView (cn) (lvl_pool); findViewById (cn) (lvl_pool)];;

let add_current_class_methods (m:((JVM_Method_cert.t StringMap.t) StringMap.t) StringMap.t) 
    (cn:string) (lvl_pool:Level_pool.t) =
	List.fold_right (fun current_method m' -> 
	add_method (JVM_Method_cert.class_name current_method) (JVM_Method_cert.name current_method) 
	  (JVM_Method_cert.descriptor current_method) (current_method) (m'))
		(current_class_method_list cn lvl_pool) (m);;
	
end;;