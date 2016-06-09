package anu.rsise.certParser;

import java.io.IOException;
import java.io.InputStream;
import java.util.TreeMap;

import anu.rsise.GeneralUtil;

public class FieldPolicy {
    private TreeMap<ItemKey, ExtendedLevel> _ft;

    public FieldPolicy() {
        _ft = new TreeMap<ItemKey, ExtendedLevel>();
    }

    public static FieldPolicy read(InputStream isr, LevelPool lvl_pool) {
        FieldPolicy result = new FieldPolicy();

        try {
            GeneralUtil.Direction dir = GeneralUtil.Direction.Left_to_right;
            short n_field = GeneralUtil.readShortFromInputStream(isr, dir);

            for (int i = 0; i < n_field; i++) {
                String class_name = GeneralUtil.readStringFromInputStream(isr);
                String name = GeneralUtil.readStringFromInputStream(isr);
                ExtendedLevel lvl = GeneralUtil.readExtLevel(isr, lvl_pool);
                result.add(class_name, name, lvl);
            }

        } catch (IOException ioe) {
            ioe.printStackTrace();
        }

        return result;
    }

    public void add(String class_name, String name, ExtendedLevel lvl) {
        _ft.put(new ItemKey(class_name, name), lvl);
    }

    public ExtendedLevel get(String class_name, String name) {
        return _ft.get(new ItemKey(class_name, name));
    }

    static class ItemKey implements Comparable<ItemKey> {
        private String _class_name;
        private String _name;

        public ItemKey(String class_name, String name) {
            _class_name = class_name;
            _name = name;
        }

        public boolean equals(ItemKey another) {
            return ((_class_name == another._class_name) && (_name == another._name));
        }

        public int compareTo(ItemKey another) {
            if (equals(another)) return 0;
            return (_class_name + _name).compareTo(another._class_name + another._name);
        }
    }
}
