package anu.rsise.dexParser.encoding;

import anu.rsise.dexParser.container.TypeContainer;

public class EncodedTypeAddrPair {

    public int type_idx;
    public TypeContainer.TypeItem type;
    public int addr;

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(type.str_type + " -> " + addr);
        return sb.toString();
    }

}
