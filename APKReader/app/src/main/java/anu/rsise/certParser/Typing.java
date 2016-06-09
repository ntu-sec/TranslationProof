package anu.rsise.certParser;

import java.util.Set;
import java.util.TreeMap;

public class Typing {
    private BasicLevel _se;
    private RT _rt;
    private ExtendedLevel _res;
    private boolean _flag;
    public Typing(BasicLevel se, RT rt, ExtendedLevel res, boolean flag) {
        _se = se;
        _rt = rt;
        _res = res;
        _flag = flag;
    }

    public Typing(BasicLevel se) {
        this(se, new RT(), null, false);
    }

    public Typing() {
        this(BasicLevel.bot);
    }

    public void add_rt(Integer key, ExtendedLevel value) {
        _rt.put(key, value);
    }

    public ExtendedLevel get_rt_at(Integer key) {
        return _rt.get(key);
    }

    public void update_res(ExtendedLevel res) {
        _res = res;
    }

    public ExtendedLevel get_res() {
        return _res;
    }

    public boolean exists_res() {
        return (_res != null);
    }

    public void update_flag(boolean flag) {
        _flag = flag;
    }

    public boolean get_flag() {
        return _flag;
    }

    public RT get_rt() {
        return _rt;
    }

    public void update_se(BasicLevel se) {
        _se = se;
    }

    public BasicLevel get_se() {
        return _se;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        sb.append(_se.toString());
        sb.append(")");
        sb.append(_rt.toString());
        sb.append("-");
        sb.append(_res.toString());
        return sb.toString();
    }

    public static class RT extends TreeMap<Integer, ExtendedLevel> {
        private static final long serialVersionUID = 1L;

        public boolean leq(RT another_rt, int localN, LevelRelationship lvl_rel) {
            Set<Integer> keys = this.keySet();
            for (Integer key : keys) {
                if (key < localN) continue;
                ExtendedLevel v1 = this.get(key);
                ExtendedLevel v2 = another_rt.get(key);
                if ((v2 != null) && (!v1.leq(v2, lvl_rel))) return false;
            }
            return true;
        }

        public void lift(BasicLevel lvl, int localN, LevelRelationship lvl_rel) {
            Set<Integer> keys = this.keySet();
            for (Integer key : keys) {
                if (key < localN) continue;
                ExtendedLevel v = this.get(key);
                this.put(key, v.lub(ExtendedLevel.createSimple(lvl), lvl_rel));
            }
        }
    }
}
