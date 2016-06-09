package anu.rsise.dexParser.container;

public class ProtoContainer {

    public ProtoItem items[];
    public int size;
    public ProtoContainer(int size) {
        items = new ProtoItem[size];
        for (int i = 0; i < size; i++) items[i] = new ProtoItem();
        this.size = size;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("############################\n");
        sb.append("#      Proto Container     #\n");
        sb.append("############################\n");
        sb.append("<desc> <ret_type> <params>\n");

        for (int i = 0; i < size; i++) {
            sb.append(i + ". " + items[i].toString());
            sb.append("\n");
        }

        sb.append("############################\n");

        return sb.toString();
    }

    public static class ProtoItem {
        public int shorty_idx;
        public String shorty_desc;
        public int return_type_idx;
        public String return_type_str;
        public int parameters_off;
        public TypeList parameters;

        public String toString() {
            StringBuilder sb = new StringBuilder();

            sb.append(shorty_desc + " : " + return_type_str + " (" + parameters_off + ")");
            if (parameters.size > 0) {
                sb.append(" : " + parameters.list[0].type_str);
                for (int j = 1; j < parameters.size; j++) {
                    sb.append(", " + parameters.list[j].type_str);
                }
            }

            return sb.toString();
        }
    }

}
