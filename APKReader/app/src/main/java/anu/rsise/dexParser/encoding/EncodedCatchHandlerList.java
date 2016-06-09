package anu.rsise.dexParser.encoding;

public class EncodedCatchHandlerList {

    public int size;
    public EncodedCatchHandler[] list;

    public void setSize(int size) {
        this.size = size;

        list = new EncodedCatchHandler[size];
        for (int i = 0; i < size; i++) list[i] = new EncodedCatchHandler();
    }
}
