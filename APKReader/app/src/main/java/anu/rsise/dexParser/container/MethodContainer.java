package anu.rsise.dexParser.container;

public class MethodContainer {

    public MethodItem items[];
    public int size;
    public MethodContainer(int size) {
        items = new MethodItem[size];
        for (int i = 0; i < size; i++) items[i] = new MethodItem();
        this.size = size;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("############################\n");
        sb.append("#      Method Container    #\n");
        sb.append("############################\n");

        for (int i = 0; i < size; i++) {
            sb.append(i + ". " + items[i].toString() + "\n");
        }

        sb.append("############################\n");

        return sb.toString();
    }

    public static class MethodItem {
        public short class_idx;
        public String class_str;
        public short proto_idx;
        public ProtoContainer.ProtoItem proto_item;
        public int name_idx;
        public String name_str;
        public boolean methodAnnotationExists = false;
        public AnnotationsDirectory.AnnotationSetItem methodAnnotation;
        public boolean parameterAnnotationExists = false;
        public AnnotationsDirectory.AnnotationSetRefList parameterAnnotation;

        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(class_str + " : " + name_str + "<proto>" + proto_item.toString() + "</proto>");
            if (methodAnnotationExists)
                sb.append(" method annotation : " + methodAnnotation.toString());
            if (parameterAnnotationExists)
                sb.append(" parameter annotation : " + parameterAnnotation.toString());
            return sb.toString();
        }

        public void setMethodAnnotation(AnnotationsDirectory.AnnotationSetItem annotation) {
            methodAnnotationExists = true;
            this.methodAnnotation = annotation;
        }

        public void setParameterAnnotation(AnnotationsDirectory.AnnotationSetRefList annotation) {
            parameterAnnotationExists = true;
            this.parameterAnnotation = annotation;
        }

        public boolean hasMethodAnnotation() {
            return methodAnnotationExists;
        }

        public boolean hasParameterAnnotation() {
            return parameterAnnotationExists;
        }
    }
}
