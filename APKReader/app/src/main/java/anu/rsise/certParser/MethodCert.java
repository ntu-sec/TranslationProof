package anu.rsise.certParser;

import java.io.IOException;
import java.io.InputStream;
import java.util.TreeMap;

import anu.rsise.GeneralUtil;

public class MethodCert {
    private String _class_name;
    private String _name;
    private String _desc;
    private short[] _labels;
    private TreeMap<Integer, Integer[]> _region;
    private TreeMap<Integer, Integer> _junction;
    private TreeMap<Integer, BytecodeMethod> _bm_map;

    public MethodCert() {
        _class_name = "";
        _name = "";
        _desc = "";
        _region = new TreeMap<Integer, Integer[]>();
        _junction = new TreeMap<Integer, Integer>();
        _bm_map = new TreeMap<Integer, BytecodeMethod>();
    }

    public static MethodCert read(InputStream isr, LevelPool lvl_pool) {
        MethodCert result = new MethodCert();
        try {
            result._class_name = GeneralUtil.readStringFromInputStream(isr);
            result._name = GeneralUtil.readStringFromInputStream(isr);
            result._desc = GeneralUtil.readStringFromInputStream(isr);
            GeneralUtil.Direction dir = GeneralUtil.Direction.Left_to_right;

            short length = GeneralUtil.readShortFromInputStream(isr, dir);
            result._labels = new short[length];

            for (int i = 0; i < length; i++) {
                short label = GeneralUtil.readShortFromInputStream(isr, dir);
                result._labels[i] = label;

		/* region */
                short reg_length = GeneralUtil.readShortFromInputStream(isr, dir);
                Integer[] elm = new Integer[reg_length];
                for (int j = 0; j < reg_length; j++) {
                    elm[j] = (int) GeneralUtil.readShortFromInputStream(isr, dir);
                }
                result._region.put((int) label, elm);

	    /* junction */
                short jun = GeneralUtil.readShortFromInputStream(isr, dir);
                if (jun <= 0) {
                    result._junction.put((int) label, null);
                } else {
                    result._junction.put((int) label, (int) jun);
                }
            }

            int lvl_length = lvl_pool.get_length();
            for (int i = 0; i < lvl_length; i++) {
                BytecodeMethod bm = BytecodeMethod.read(isr, lvl_pool);
                result._bm_map.put(bm.get_objectLevel(), bm);
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
        return result;
    }

    public Integer[] get_region(int label) {
        return _region.get(label);
    }

    public Integer get_junction(int label) {
        return _junction.get(label);
    }

    public void set_class_name(String class_name) {
        _class_name = class_name;
    }

    public String class_name() {
        return _class_name;
    }

    public void set_name(String name) {
        _name = name;
    }

    public String name() {
        return _name;
    }

    public void set_desc(String desc) {
        _desc = desc;
    }

    public String desc() {
        return _desc;
    }

    public BytecodeMethod get_bytecodeMethod(int objectLevel) {
        return _bm_map.get(objectLevel);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(_class_name);
        sb.append(_name);
        sb.append(" - ");
        sb.append(_desc);
        sb.append("\n");

        for (BytecodeMethod bm : _bm_map.values()) {
            sb.append(bm.toString());
            sb.append("\n");
        }

        return sb.toString();
    }
}
