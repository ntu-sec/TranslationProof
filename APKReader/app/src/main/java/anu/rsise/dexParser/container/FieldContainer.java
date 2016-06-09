package anu.rsise.dexParser.container;

public class FieldContainer {

    public FieldItem items[];
    public int size;
    public FieldContainer(int size) {
        items = new FieldItem[size];
        for (int i = 0; i < size; i++) items[i] = new FieldItem();
        this.size = size;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("############################\n");
        sb.append("#      Field Container     #\n");
        sb.append("############################\n");
        sb.append("<class> <type> <name>\n");

        for (int i = 0; i < size; i++) {
            sb.append(i + ". " + items[i].toString() + "\n");
        }

        sb.append("############################\n");

        return sb.toString();
    }

    public static class FieldItem {
        public short class_idx;
        public String class_str;
        public short type_idx;
        public String type_str;
        public int name_idx;
        public String name_str;
        public boolean annotationExists = false;
        public AnnotationsDirectory.AnnotationSetItem annotation;

        public String toString() {
            StringBuilder sb = new StringBuilder(class_str + " : " + type_str + " : " + name_str);
            if (annotationExists) sb.append(annotation.toString());
            return sb.toString();
        }

        public void setAnnotation(AnnotationsDirectory.AnnotationSetItem annotation) {
            annotationExists = true;
            this.annotation = annotation;
        }

        public boolean hasAnnotation() {
            return annotationExists;
        }
    }
}
