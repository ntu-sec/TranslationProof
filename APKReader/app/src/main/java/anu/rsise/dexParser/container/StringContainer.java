package anu.rsise.dexParser.container;

public class StringContainer {

    public StringItem items[];
    public int size;
    public StringContainer(int size) {
        items = new StringItem[size];
        for (int i = 0; i < size; i++) items[i] = new StringItem();
        this.size = size;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("############################\n");
        sb.append("#     String Container     #\n");
        sb.append("############################\n");
        sb.append("<str_off> <size> <data>\n");

        for (int i = 0; i < size; i++) {
            sb.append(i + ". " + items[i].string_data_off + " : " + items[i].utf16_size + " : " + items[i].str_data + "\n");
        }

        sb.append("############################\n");

        return sb.toString();
    }

    public static class StringItem {
        public int string_data_off;
        public int utf16_size;
        public String str_data;

        public String toString() {
            return str_data;
        }
    }
}
