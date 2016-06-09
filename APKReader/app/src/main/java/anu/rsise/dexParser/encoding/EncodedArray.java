package anu.rsise.dexParser.encoding;

public class EncodedArray {

    public int size;
    public EncodedValue values[];

    public void setSize(int size) {
        this.size = size;
        values = new EncodedValue[size];
        for (int i = 0; i < size; i++) values[i] = new EncodedValue();
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("<array>");
        for (int i = 0; i < size; i++) {
            sb.append("<arrv>");
            sb.append(values[i].toString());
            sb.append("</arrv>");
        }
        sb.append("</array>");

        return sb.toString();
    }
}
