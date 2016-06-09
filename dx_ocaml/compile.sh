ocamlc compiler_meta.mli compiler_meta.ml
ocamlc utility.mli utility.ml
ocamlc compiler_meta.ml utility.ml parse_byte.mli parse_byte.ml
ocamlc compiler_meta.ml start_block.mli start_block.ml
ocamlc compiler_meta.ml trace_parent_child.mli trace_parent_child.ml
ocamlc compiler_meta.ml translate_java_program.mli translate_java_program.ml
ocamlc compiler_meta.ml pick_order.mli pick_order.ml
ocamlc compiler_meta.ml consolidate_block.mli consolidate_block.ml
ocamlc compiler_meta.ml adding_gotos.mli adding_gotos.ml
ocamlc compiler_meta.ml utility.ml constant_parsing.mli constant_parsing.ml
ocamlc -g compiler_meta.ml start_block.ml utility.ml parse_byte.ml trace_parent_child.ml translate_java_program.ml pick_order.ml consolidate_block.ml adding_gotos.ml constant_parsing.ml main.ml

