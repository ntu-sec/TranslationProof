package anu.rsise.certParser;

public abstract class ExtendedLevel {
    public static ExtendedLevel createSimple(byte lvl_id, LevelPool lvl_pool) {
        return new SimpleLevel(lvl_id, lvl_pool);
    }

    public static ExtendedLevel createSimple(BasicLevel lvl) {
        return new SimpleLevel(lvl);
    }

    public static ExtendedLevel createArray(byte[] lvl_ids, LevelPool lvl_pool) {
        return new ArrayLevel(lvl_ids, 0, (lvl_ids.length), lvl_pool);
    }

    public static ExtendedLevel createArray(BasicLevel lvl, ExtendedLevel content) {
        return new ArrayLevel(lvl, content);
    }

    public abstract boolean leq(ExtendedLevel lvl, LevelRelationship lvl_rel);

    /* TODO : fix the mechanism of least upper bound for partial level */
    public ExtendedLevel lub(ExtendedLevel another, LevelRelationship lvl_rel) {
        if (leq(another, lvl_rel)) return another;
        return this;
    }

    public abstract BasicLevel getBasicLevel();

    public abstract String toString();

    static class SimpleLevel extends ExtendedLevel {
        private BasicLevel _lvl;

        public SimpleLevel(BasicLevel lvl) {
            _lvl = lvl;
        }

        public SimpleLevel(byte lvl_id, LevelPool lvl_pool) {
            _lvl = lvl_pool.get(lvl_id);
        }

        public BasicLevel getBasicLevel() {
            return _lvl;
        }

        public boolean leq(ExtendedLevel another_lvl, LevelRelationship lvl_rel) {
            return lvl_rel.leq(_lvl.id(), another_lvl.getBasicLevel().id());
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(_lvl.toString());
            return sb.toString();
        }
    }

    static class ArrayLevel extends ExtendedLevel {
        private BasicLevel _lvl;
        private ExtendedLevel _content;

        public ArrayLevel(BasicLevel lvl, ExtendedLevel content) {
            _lvl = lvl;
            _content = content;
        }

        public ArrayLevel(byte[] lvl_ids, int i, int n, LevelPool lvl_pool) {
            _lvl = lvl_pool.get(lvl_ids[i]);
            if (i < n - 1) {
                _content = new ArrayLevel(lvl_ids, i + 1, n, lvl_pool);
            } else if (i == n - 1) {
                _content = new SimpleLevel(lvl_ids[n - 1], lvl_pool);
            }
        }

        public ExtendedLevel getContent() {
            return _content;
        }

        public BasicLevel getBasicLevel() {
            return _lvl;
        }

        public boolean leq(ExtendedLevel another_lvl, LevelRelationship lvl_rel) {
            boolean outer = lvl_rel.leq(_lvl.id(), another_lvl.getBasicLevel().id());
            if (another_lvl instanceof ArrayLevel)
                outer = outer && (_content.leq(((ArrayLevel) another_lvl).getContent(), lvl_rel));
            return outer;
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(_lvl.toString());
            sb.append('[');
            sb.append(_content.toString());
            sb.append(']');
            return sb.toString();
        }
    }
}
