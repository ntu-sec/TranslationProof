package anu.rsise.certParser;

public class BasicLevel {
    public static BasicLevel bot = new BasicLevel(-1, "Bot");
    private int _id;
    private String _name;

    public BasicLevel(int id, String name) {
        this._id = id;
        this._name = name;
    }

    public String name() {
        return _name;
    }

    public int id() {
        return _id;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(_name);
        return sb.toString();
    }

    public boolean leq(BasicLevel another, LevelRelationship lvl_rel) {
        return lvl_rel.leq(_id, another._id);
    }

    public BasicLevel lub(BasicLevel another, LevelRelationship lvl_rel) {
        if (!leq(another, lvl_rel)) return another;
        return this;
    }
}
