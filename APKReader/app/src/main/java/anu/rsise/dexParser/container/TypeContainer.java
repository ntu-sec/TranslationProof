package anu.rsise.dexParser.container;

public class TypeContainer {

    public TypeItem items[];
    public int size;
    public TypeContainer(int size) {
        items = new TypeItem[size];
        for (int i = 0; i < size; i++) items[i] = new TypeItem();
        this.size = size;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("############################\n");
        sb.append("#       Type Container     #\n");
        sb.append("############################\n");
        sb.append("<descriptor_idx> <str_type> \n");

        for (int i = 0; i < size; i++) {
            sb.append(i + ". " + items[i].descriptor_idx + " : " + items[i].str_type + "\n");
        }

        sb.append("############################\n");

        return sb.toString();
    }

    public static class TypeItem {
        public int descriptor_idx;
        public String str_type;

        public String toString() {
            return str_type;
        }
    }

}
