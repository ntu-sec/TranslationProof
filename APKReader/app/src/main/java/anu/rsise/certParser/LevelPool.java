package anu.rsise.certParser;

import java.io.IOException;
import java.io.InputStream;

import anu.rsise.GeneralUtil;
/* level pool is set as a direct mapping from id to level */

public class LevelPool {
    private int _length;
    private BasicLevel[] _content;

    public LevelPool() {
    }

    public static LevelPool read(InputStream isr) {
        LevelPool result = new LevelPool();

        try {
            int length = isr.read();
            result._content = new BasicLevel[length];
            result._length = length;
            for (int i = 0; i < length; i++) {
                String temp = GeneralUtil.readStringFromInputStream(isr);
                result._content[i] = new BasicLevel(i, temp);
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
        return result;
    }

    public BasicLevel get(int n) {
        return _content[n];
    }

    public BasicLevel[] get_contents() {
        return _content;
    }

    public int get_length() {
        return _length;
    }

    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append('[');
        for (int i = 0; i < _length; i++) {
            sb.append(_content[i].toString());
            sb.append(", ");
        }
        sb.append(']');
        return sb.toString();
    }
}
