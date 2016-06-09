package anu.rsise.certParser;

import java.io.IOException;
import java.io.InputStream;
import java.util.TreeMap;

import anu.rsise.GeneralUtil;

public class BytecodeMethod {
    private short[] _labels;
    private int _objectLevel; /* for now just use the index, it might be more beneficial to use proper level */
    private MethodPolicy _policy;
    private TreeMap<Integer, Typing> _typingMap;

    public BytecodeMethod() {
        _typingMap = new TreeMap<Integer, Typing>();
    }

    public static BytecodeMethod read(InputStream isr, LevelPool lvl_pool) {
        BytecodeMethod bm = new BytecodeMethod();
        GeneralUtil.Direction dir = GeneralUtil.Direction.Left_to_right;
        try {
            bm._objectLevel = isr.read();

	  /* policy */
            bm._policy = MethodPolicy.read(isr, lvl_pool);

            short length = GeneralUtil.readShortFromInputStream(isr, dir);
            bm._labels = new short[length];

            for (int i = 0; i < length; i++) {
                short label = GeneralUtil.readShortFromInputStream(isr, dir);
                bm._labels[i] = label;

	    /* typing */
                BasicLevel se = lvl_pool.get(isr.read());
                Typing typeInfo = new Typing(se);
                short type_length = GeneralUtil.readShortFromInputStream(isr, dir);
                for (int j = 0; j < type_length; j++) {
                    short localVar = GeneralUtil.readShortFromInputStream(isr, dir);
                    ExtendedLevel value = GeneralUtil.readExtLevel(isr, lvl_pool);
                    typeInfo.add_rt((int) localVar, value);
                }
                typeInfo.update_res(GeneralUtil.readExtLevel(isr, lvl_pool));
                typeInfo.update_flag(isr.read() == 1);
                bm._typingMap.put((int) label, typeInfo);
            }

        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
        return bm;
    }

    public MethodPolicy policy() {
        return _policy;
    }

    public Typing get_type(int label) {
        return _typingMap.get(label);
    }

    public int get_objectLevel() {
        return _objectLevel;
    }

    public int ins_count() {
        return _labels.length;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
	/* policy */
        sb.append(_policy.toString());
	
	/* typing */
        sb.append(_typingMap.toString());
        sb.append("\n");
        return sb.toString();
    }
}
