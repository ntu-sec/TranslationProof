package anu.rsise.certParser;

import java.util.TreeMap;

public class CertContainer {
    private LevelPool _lvl_pool;
    private LevelRelationship _lvl_rel;
    private int _default_level_id;
    private TreeMap<MethodKey, MethodCert> _methods;
    private FieldPolicy _ft;
    public CertContainer() {
        _methods = new TreeMap<MethodKey, MethodCert>();
    }

    public void set_lvl_pool(LevelPool lvl_pool) {
        _lvl_pool = lvl_pool;
    }

    public LevelPool lvl_pool() {
        return _lvl_pool;
    }

    public void set_lvl_rel(LevelRelationship lvl_rel) {
        _lvl_rel = lvl_rel;
    }

    public LevelRelationship lvl_rel() {
        return _lvl_rel;
    }

    public void set_default_level_id(int default_level_id) {
        _default_level_id = default_level_id;
    }

    public int default_level_id() {
        return _default_level_id;
    }

    public void set_field_policy(FieldPolicy ft) {
        _ft = ft;
    }

    public FieldPolicy ft() {
        return _ft;
    }

    public void add_method(String class_name, String name, String desc, MethodCert method) {
        _methods.put(new MethodKey(class_name, name, desc), method);
    }

    public MethodCert method(String class_name, String name, String desc) {
        return _methods.get(new MethodKey(class_name, name, desc));
    }

    public boolean containMethod(String class_name, String name, String desc) {
        return _methods.containsKey(new MethodKey(class_name, name, desc));
    }

    static class MethodKey implements Comparable<MethodKey> {
        private String _class_name;
        private String _name;
        private String _desc;

        public MethodKey(String class_name, String name, String desc) {
            _class_name = class_name;
            _name = name;
            _desc = desc;
        }

        public boolean equals(MethodKey another) {
            return ((_class_name == another._class_name) && (_name == another._name) && (_desc == another._desc));
        }

        public int compareTo(MethodKey another) {
            if (equals(another)) return 0;
            return (_class_name + _name + _desc).compareTo(another._class_name + another._name + another._desc);
        }
    }
}
