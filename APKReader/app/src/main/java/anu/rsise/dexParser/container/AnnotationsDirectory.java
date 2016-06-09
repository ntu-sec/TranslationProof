package anu.rsise.dexParser.container;

import anu.rsise.dexParser.encoding.EncodedAnnotation;

public class AnnotationsDirectory {

    public int class_annotations_off;
    public int field_size;
    public int method_size;
    public int parameter_size;
    public FieldAnnotation field_annotations[];
    public MethodAnnotation method_annotations[];
    public ParameterAnnotation parameter_annotations[];

    public void setFieldSize(int size) {
        field_size = size;
        field_annotations = new FieldAnnotation[size];
        for (int i = 0; i < size; i++) field_annotations[i] = new FieldAnnotation();
    }

    public void setMethodSize(int size) {
        method_size = size;
        method_annotations = new MethodAnnotation[size];
        for (int i = 0; i < size; i++) method_annotations[i] = new MethodAnnotation();
    }

    public void setParameterSize(int size) {
        parameter_size = size;
        parameter_annotations = new ParameterAnnotation[size];
        for (int i = 0; i < size; i++) parameter_annotations[i] = new ParameterAnnotation();
    }

    public static class FieldAnnotation {
        public int field_idx;
        public FieldContainer.FieldItem field;
        public int annotation_off;
        public AnnotationSetItem annotationSetItem;
    }

    public static class MethodAnnotation {
        public int method_idx;
        public MethodContainer.MethodItem method;
        public int annotation_off;
        public AnnotationSetItem annotationSetItem;
    }

    public static class ParameterAnnotation {
        public int method_idx;
        public MethodContainer.MethodItem method;
        public int annotation_off;
        public AnnotationSetRefList annotationSetRefList;
    }

    public static class AnnotationSetRefList {
        public int size;
        public AnnotationSetRefItem list[];

        public void setSize(int size) {
            this.size = size;
            list = new AnnotationSetRefItem[size];
            for (int i = 0; i < size; i++) list[i] = new AnnotationSetRefItem();
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("<annot_set_ref size=\"" + size + "\">");
            for (int i = 0; i < size; i++) sb.append(list[i].annotationSetItem.toString());
            sb.append("</annot_set_ref>");
            return sb.toString();
        }

        public static class AnnotationSetRefItem {
            public int annotation_off;
            public AnnotationSetItem annotationSetItem;
        }
    }

    public static class AnnotationSetItem {
        public int size;
        public AnnotationItem[] entries;

        public void setSize(int size) {
            this.size = size;
            entries = new AnnotationItem[size];
            for (int i = 0; i < size; i++) entries[i] = new AnnotationItem();
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();

            if (size > 0) {
                sb.append("<annot_set size=\"" + size + "\">");
                sb.append(entries[0].toString());
                for (int i = 1; i < size; i++) sb.append(", " + entries[i].toString());
                sb.append("</annot_set>");
            }

            return sb.toString();
        }

        public static class AnnotationItem {
            public static final int VISIBILITY_BUILD = 0x00;
            public static final int VISIBILITY_RUNTIME = 0x01;
            public static final int VISIBILITY_SYSTEM = 0x02;

            public byte visibility;
            public int annotation_off;
            public EncodedAnnotation annotation;

            public String toString() {
                StringBuilder sb = new StringBuilder();
                sb.append("<annot_item visibility=\"");
                switch (visibility) {
                    case (VISIBILITY_BUILD):
                        sb.append("VISIBILITY_BUILD");
                        break;
                    case (VISIBILITY_RUNTIME):
                        sb.append("VISIBILITY_RUNTIME");
                        break;
                    case (VISIBILITY_SYSTEM):
                        sb.append("VISIBILITY_SYSTEM");
                        break;
                }
                sb.append("\">");
                sb.append(annotation.toString());
                sb.append("</annot_item>");
                return sb.toString();
            }
        }
    }
}
