package anu.rsise.certParser;

import java.io.IOException;
import java.io.InputStream;

import anu.rsise.GeneralUtil;

public class MethodPolicy {
    private int _n_ka;
    private ExtendedLevel[] _ka;
    private BasicLevel _kh;
    private int _n_kr;
    private ExtendedLevel[] _kr;

    public MethodPolicy() {
    }

    ;

    public static MethodPolicy read(InputStream isr, LevelPool lvl_pool) {
        MethodPolicy result = new MethodPolicy();
        try {
            int n_ka = isr.read();
            result._n_ka = n_ka;
            result._ka = new ExtendedLevel[n_ka];
            for (int i = 0; i < n_ka; i++) {
                result._ka[i] = GeneralUtil.readExtLevel(isr, lvl_pool);
            }
            result._kh = lvl_pool.get(isr.read());
            int n_kr = isr.read();
            result._n_kr = n_kr;
            result._kr = new ExtendedLevel[n_kr];
            for (int i = 0; i < n_kr; i++) {
                result._kr[i] = GeneralUtil.readExtLevel(isr, lvl_pool);
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
        return result;
    }

    public int localVariable_size() {
        return _n_ka;
    }

    public ExtendedLevel ka_at(int localVar) {
        return _ka[localVar];
    }

    public int returnType_size() {
        return _n_kr;
    }

    // 0 for the security level of a normal return
    public ExtendedLevel kr_at(int retType) {
        return _kr[retType];
    }

    public BasicLevel kh() {
        return _kh;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (int i = 0; i < _n_ka; i++) {
            sb.append(i);
            sb.append("-");
            sb.append(_ka[i].toString());
            sb.append(", ");
        }
        sb.append("] ");
        sb.append(_kh.toString());
        sb.append(" [");
        for (int i = 0; i < _n_kr; i++) {
            sb.append(i);
            sb.append("-");
            sb.append(_kr[i].toString());
            sb.append(", ");
        }
        sb.append("]\n");
        return sb.toString();
    }
}
