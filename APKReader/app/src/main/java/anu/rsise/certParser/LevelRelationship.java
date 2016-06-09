package anu.rsise.certParser;

import java.io.IOException;
import java.io.InputStream;

import anu.rsise.GeneralUtil;

public class LevelRelationship {
    private boolean[][] _content;
    private int _length;

    public LevelRelationship(int n) {
        _length = n;
        _content = new boolean[n][];
        for (int i = 0; i < n; i++) {
            _content[i] = new boolean[n];
            for (int j = 0; j < n; j++) {
                _content[i][j] = (i == j);
            }
        }
    }

    public static LevelRelationship read(InputStream isr, int n_level) {
        LevelRelationship result = new LevelRelationship(n_level);

        try {
            int length = GeneralUtil.readShortFromInputStream(isr, GeneralUtil.Direction.Left_to_right);
            for (int i = 0; i < length; i++) {
                int idx1 = isr.read();
                int idx2 = isr.read();
                result._content[idx1][idx2] = true;
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
        return result;
    }

    public boolean leq(int a, int b) {
        return _content[a][b];
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < _length; i++) {
            for (int j = 0; j < _length; j++) {
                sb.append('(');
                sb.append(i);
                sb.append(", ");
                sb.append(j);
                sb.append(") = ");
                sb.append(_content[i][j]);
                sb.append("\n");
            }
        }
        return sb.toString();
    }
}
