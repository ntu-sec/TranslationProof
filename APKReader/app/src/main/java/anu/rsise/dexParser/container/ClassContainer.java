package anu.rsise.dexParser.container;

import anu.rsise.dexParser.encoding.EncodedArray;
import anu.rsise.dexParser.encoding.EncodedField;
import anu.rsise.dexParser.encoding.EncodedMethod;

public class ClassContainer {

    public ClassItem items[];
    public int size;


    public ClassContainer(int size) {
        items = new ClassItem[size];
        for (int i = 0; i < size; i++) items[i] = new ClassItem();

        this.size = size;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("############################\n");
        sb.append("#      Class Container     #\n");
        sb.append("############################\n");

        for (int i = 0; i < size; i++) {
            sb.append("\n");
            sb.append("Class #" + (i + 1) + "\n");
            sb.append("\t Class Idx \t\t: " + items[i].class_idx + "\n");
            sb.append("\t Access_flags \t\t: " + items[i].access_flag + "\n");
            sb.append("\t Superclass Idx \t: " + items[i].superclass_idx + "\n");
            sb.append("\t Interfaces Off \t: " + items[i].interface_off + "\n");
            sb.append("\t Annotations Off \t: " + items[i].annotations_off + "\n");
            sb.append("\t Class Data Off \t: " + items[i].class_data_off + "\n");
            sb.append("\t Static Fields Size \t: " + items[i].class_data.static_fields_size + "\n");
            sb.append("\t Instance Fields Size \t: " + items[i].class_data.instance_fields_size + "\n");
            sb.append("\t Direct Method Size \t: " + items[i].class_data.direct_methods_size + "\n");
            sb.append("\t Virtual Method Size \t: " + items[i].class_data.virtual_methods_size + "\n");

            sb.append("\n");
            if (items[i].hasAnnotation)
                sb.append("\t Annotation \t: " + items[i].classAnnotation.toString() + "\n");
            sb.append("\t Class Name \t: " + items[i].class_str + "\n");
            sb.append("\t SuperClass \t: " + items[i].superclass_str + "\n");
            sb.append("\t Access \t: " + items[i].access_flag_str + "\n");
            sb.append("\t Source File \t: " + items[i].source_file_str + "\n");
            sb.append("\t Interfaces \t: \n");
            for (int j = 0; j < items[i].interfaces.size; j++) {
                sb.append("\t\t#" + j + " : " + items[i].interfaces.list[j].type_str + "\n");
            }

            // Printing Fields
            sb.append("\t Static Fields :\n");
            EncodedField[] ef = items[i].class_data.static_fields;
            for (int j = 0; j < items[i].class_data.static_fields_size; j++) {
                sb.append("\t\t#" + j + " : " + ef[j].field.class_str + "\n");
                if (ef[j].field.hasAnnotation())
                    sb.append("\t\t\t" + ef[j].field.annotation.toString() + "\n");
                sb.append("\t\t\t Name\t: " + ef[j].field.name_str + "\n");
                sb.append("\t\t\t Access\t: " + ef[j].access_flags_str + "\n");
                sb.append("\t\t\t Type\t: " + ef[j].field.type_str + "\n");
            }
            sb.append("\t Instance Fields :\n");
            ef = items[i].class_data.instance_fields;
            for (int j = 0; j < items[i].class_data.instance_fields_size; j++) {
                sb.append("\t\t#" + j + " : " + ef[j].field.class_str + "\n");
                if (ef[j].field.hasAnnotation())
                    sb.append("\t\t\t" + ef[j].field.annotation.toString() + "\n");
                sb.append("\t\t\t Name\t: " + ef[j].field.name_str + "\n");
                sb.append("\t\t\t Access\t: " + ef[j].access_flags_str + "\n");
                sb.append("\t\t\t Type\t: " + ef[j].field.type_str + "\n");
            }

            // Printing Methods
            sb.append("\t Direct Methods :\n");
            sb.append(methodString(items[i].class_data.direct_methods, items[i].class_data.direct_methods_size));

            sb.append("\t Virtual Methods :\n");
            sb.append(methodString(items[i].class_data.virtual_methods, items[i].class_data.virtual_methods_size));

            sb.append("\n");
        }

        sb.append("############################\n");

        return sb.toString();
    }

    private String methodString(EncodedMethod[] em, int size) {
        StringBuilder sb = new StringBuilder();
        for (int j = 0; j < size; j++) {
            sb.append("\t\t#" + j + " : " + em[j].method.class_str + "\n");
            sb.append("\t\t\t Name\t: " + em[j].method.name_str + "\n");
            if (em[j].method.hasMethodAnnotation())
                sb.append("\t\t\t Meth Annot\t: " + em[j].method.methodAnnotation + "\n");
            if (em[j].method.hasParameterAnnotation())
                sb.append("\t\t\t Param Annot\t: " + em[j].method.parameterAnnotation + "\n");
            sb.append("\t\t\t Access\t: " + em[j].access_flags_str + "\n");
            sb.append("\t\t\t Regs\t: " + em[j].code.register_size + "\n");
            sb.append("\t\t\t Ins\t: " + em[j].code.ins_size + "\n");
            sb.append("\t\t\t Outs\t: " + em[j].code.outs_size + "\n");
            sb.append("\t\t\t Debug Off\t: " + Integer.toHexString(em[j].code.debug_info_off) + "\n");
            sb.append("\t\t\t Tries\t: " + em[j].code.tries_size + "\n");
            sb.append("\t\t\t Code_off\t: " + em[j].code_off + "\n");
            sb.append("\t\t\t Insn Size\t: " + em[j].code.insns_size + "\n");

            for (int k = 0; k < em[j].code.insns_count; k++) {
                sb.append(em[j].code.insns[k] + "\n");
            }

            sb.append("\t\t\t Catches \t:\n");
            for (int k = 0; k < em[j].code.tries_size; k++) {
                sb.append("\t\t\t\t#" + k + " : " + em[j].code.tries[k].start_addr + " - " + (em[j].code.tries[k].start_addr + em[j].code.tries[k].insn_count) + "\n");
                for (int l = 0; l < em[j].code.tries[k].handler.size; l++) {
                    sb.append("\t\t\t\t\t " + em[j].code.tries[k].handler.handlers[l].toString() + "\n");
                }
                if (em[j].code.tries[k].handler.catch_all_exists) {
                    sb.append("\t\t\t\t\t Catch_all : " + em[j].code.tries[k].handler.catch_all_addr + "\n");
                }
            }

            if (em[j].code.debug_info_off != 0) {
                sb.append("\t\t\t Debug\t:\n");
                sb.append("\t\t\t\t Line Start\t: " + em[j].code.debug_info.line_start + "\n");
                sb.append("\t\t\t\t Param Size\t: " + em[j].code.debug_info.parameter_size + "\n");
                sb.append("\t\t\t\t Parameters\t:\n");
                for (int k = 0; k < em[j].code.debug_info.parameter_size; k++) {
                    sb.append("\t\t\t\t\t- " + em[j].code.debug_info.parameter_names_str[k] + "\n");
                }

                sb.append(em[j].code.debug_summary.toString("\t\t\t\t "));
            }
        }
        return sb.toString();
    }

    public static class ACCESS_FLAG {
        public static final int ACC_PUBLIC = 0x1;
        public static final int ACC_PRIVATE = 0x2;
        public static final int ACC_PROTECTED = 0x4;
        public static final int ACC_STATIC = 0x8;
        public static final int ACC_FINAL = 0x10;
        public static final int ACC_SYNCHRONIZED = 0x20;
        public static final int ACC_VOLATILE = 0x40;
        public static final int ACC_BRIDGE = 0x40;
        public static final int ACC_TRANSIENT = 0x80;
        public static final int ACC_VARARGS = 0x80;
        public static final int ACC_NATIVE = 0x100;
        public static final int ACC_INTERFACE = 0x200;
        public static final int ACC_ABSTRACT = 0x400;
        public static final int ACC_STRICT = 0x800;
        public static final int ACC_SYNTHETIC = 0x1000;
        public static final int ACC_ANNOTATION = 0x2000;
        public static final int ACC_ENUM = 0x4000;
        public static final int unused = 0x8000;
        public static final int ACC_CONSTRUCTOR = 0x10000;
        public static final int ACC_DECLARED_SYNCHRONIZED = 0x20000;

        public static String resolveClasses(int access_flag) {
            StringBuilder sb = new StringBuilder();
            if ((access_flag & ACC_PUBLIC) != 0) sb.append("PUBLIC ");
            if ((access_flag & ACC_PRIVATE) != 0) sb.append("PRIVATE ");
            if ((access_flag & ACC_PROTECTED) != 0) sb.append("PROTECTED ");
            if ((access_flag & ACC_STATIC) != 0) sb.append("STATIC ");
            if ((access_flag & ACC_FINAL) != 0) sb.append("FINAL ");
            if ((access_flag & ACC_INTERFACE) != 0) sb.append("INTERFACE ");
            if ((access_flag & ACC_ABSTRACT) != 0) sb.append("ABSTRACT ");
            if ((access_flag & ACC_SYNTHETIC) != 0) sb.append("SYNTHETIC ");
            if ((access_flag & ACC_ANNOTATION) != 0) sb.append("ANNOTATION ");
            return sb.toString();
        }

        public static String resolveFields(int access_flag) {
            StringBuilder sb = new StringBuilder();
            if ((access_flag & ACC_PUBLIC) != 0) sb.append("PUBLIC ");
            if ((access_flag & ACC_PRIVATE) != 0) sb.append("PRIVATE ");
            if ((access_flag & ACC_PROTECTED) != 0) sb.append("PROTECTED ");
            if ((access_flag & ACC_STATIC) != 0) sb.append("STATIC ");
            if ((access_flag & ACC_FINAL) != 0) sb.append("FINAL ");
            if ((access_flag & ACC_VOLATILE) != 0) sb.append("VOLATILE ");
            if ((access_flag & ACC_TRANSIENT) != 0) sb.append("TRANSIENT ");
            if ((access_flag & ACC_SYNTHETIC) != 0) sb.append("SYNTHETIC ");
            if ((access_flag & ACC_ENUM) != 0) sb.append("ENUM ");
            return sb.toString();
        }

        public static String resolveMethods(int access_flag) {
            StringBuilder sb = new StringBuilder();
            if ((access_flag & ACC_PUBLIC) != 0) sb.append("PUBLIC ");
            if ((access_flag & ACC_PRIVATE) != 0) sb.append("PRIVATE ");
            if ((access_flag & ACC_PROTECTED) != 0) sb.append("PROTECTED ");
            if ((access_flag & ACC_STATIC) != 0) sb.append("STATIC ");
            if ((access_flag & ACC_FINAL) != 0) sb.append("FINAL ");
            if ((access_flag & ACC_SYNCHRONIZED) != 0) sb.append("SYNCHRONIZED ");
            if ((access_flag & ACC_BRIDGE) != 0) sb.append("BRIDGE ");
            if ((access_flag & ACC_VARARGS) != 0) sb.append("VARARGS ");
            if ((access_flag & ACC_ABSTRACT) != 0) sb.append("ABSTRACT ");
            if ((access_flag & ACC_STRICT) != 0) sb.append("STRICT ");
            if ((access_flag & ACC_SYNTHETIC) != 0) sb.append("SYNTHETIC ");
            if ((access_flag & ACC_CONSTRUCTOR) != 0) sb.append("CONSTRUCTOR ");
            if ((access_flag & ACC_DECLARED_SYNCHRONIZED) != 0) sb.append("DECLARED_SYNCHRONIZED ");
            return sb.toString();
        }
    }

    public static class ClassItem {

        // Class Definition Item
        public int class_idx;
        public String class_str;
        public int access_flag;
        public String access_flag_str;
        public int superclass_idx;
        public String superclass_str;
        public int interface_off;
        public TypeList interfaces;
        public int source_file_idx;
        public String source_file_str;
        public int annotations_off;
        public AnnotationsDirectory annotationDirectory;
        public boolean hasAnnotation = false;
        public AnnotationsDirectory.AnnotationSetItem classAnnotation;
        public int class_data_off;
        public ClassDataItem class_data;
        public int static_values_off;
        public EncodedArray static_values;

        public void setClassAnnotation(AnnotationsDirectory.AnnotationSetItem annotation) {
            hasAnnotation = true;
            this.classAnnotation = annotation;
        }

        public static class ClassDataItem {
            // Class Data Item
            public int static_fields_size;
            public int instance_fields_size;
            public int direct_methods_size;
            public int virtual_methods_size;
            public EncodedField static_fields[];
            public EncodedField instance_fields[];
            public EncodedMethod direct_methods[];
            public EncodedMethod virtual_methods[];

            public void setStaticField(int size) {
                static_fields_size = size;
                static_fields = new EncodedField[size];
                for (int i = 0; i < size; i++) static_fields[i] = new EncodedField();
            }

            public void setInstanceField(int size) {
                instance_fields_size = size;
                instance_fields = new EncodedField[size];
                for (int i = 0; i < size; i++) instance_fields[i] = new EncodedField();
            }

            public void setDirectMethod(int size) {
                direct_methods_size = size;
                direct_methods = new EncodedMethod[size];
                for (int i = 0; i < size; i++) direct_methods[i] = new EncodedMethod();
            }

            public void setVirtualMethod(int size) {
                virtual_methods_size = size;
                virtual_methods = new EncodedMethod[size];
                for (int i = 0; i < size; i++) virtual_methods[i] = new EncodedMethod();
            }
        }
    }

}
