package anu.rsise.dexParser.encoding;

public class EncodedCatchHandler {
    public int size;
    public boolean catch_all_exists;
    public int offset_from_start_list;
    public EncodedTypeAddrPair[] handlers;
    public int catch_all_addr;

    public void setSize(int size) {
        if (size <= 0) catch_all_exists = true;
        else catch_all_exists = false;
        this.size = Math.abs(size);
        handlers = new EncodedTypeAddrPair[this.size];
        for (int i = 0; i < this.size; i++) handlers[i] = new EncodedTypeAddrPair();
    }
}
