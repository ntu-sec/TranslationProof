package anu.rsise.dexParser.encoding;

import anu.rsise.dexParser.container.TypeContainer;

public class EncodedAnnotation {

    public int type_idx;
    public TypeContainer.TypeItem type;
    public int size;
    public AnnotationElement elements[];

    public void setSize(int size) {
        this.size = size;
        elements = new AnnotationElement[size];
        for (int i = 0; i < size; i++) elements[i] = new AnnotationElement();
    }

    public String toString() {
        StringBuilder sb = new StringBuilder("@" + type.toString() + " : ");
        if (size > 0) {
            sb.append(elements[0].toString());
            for (int i = 1; i < size; i++) sb.append("- " + elements[i].toString());
        }
        return sb.toString();
    }

    public static class AnnotationElement {
        public int name_idx;
        public String name_str;
        public EncodedValue value;

        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("@" + name_str + "{" + value.valueContainer.toString() + "}");
            return sb.toString();
        }
    }
}
