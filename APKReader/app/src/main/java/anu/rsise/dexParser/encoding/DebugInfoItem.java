package anu.rsise.dexParser.encoding;

import anu.rsise.GeneralUtil;
import anu.rsise.dexParser.container.DexContainer;
import anu.rsise.dexParser.container.TypeContainer;

public class DebugInfoItem {

    public static final int DBG_LINE_BASE = -4;
    public static final int DBG_LINE_RANGE = 15;

    public static final byte DBG_END_SEQUENCE = 0x00;
    public static final byte DBG_ADVANCE_PC = 0x01;
    public static final byte DBG_ADVANCE_LINE = 0x02;
    public static final byte DBG_START_LOCAL = 0x03;
    public static final byte DBG_START_LOCAL_EXTENDED = 0x04;
    public static final byte DBG_END_LOCAL = 0x05;
    public static final byte DBG_RESTART_LOCAL = 0x06;
    public static final byte DBG_SET_PROLOGUE_END = 0x07;
    public static final byte DBG_SET_EPILOGUE_BEGIN = 0x08;
    public static final byte DBG_SET_FILE = 0x09;
    public DebugOpCode[] debug_codes;
    public int line_start;
    public int size;
    public int parameter_size;
    public int parameter_names[];
    public String parameter_names_str[];

    // parameter size, sorry for the confusion
    public void setSize(int size) {
        parameter_size = size;
        parameter_names = new int[size];
        parameter_names_str = new String[size];
    }

    public static class DebugOpCode {
        public static final byte HAS_NONE = 0x00;
        public static final byte HAS_REG = 0x01;
        public static final byte HAS_ADDR = 0x02;
        public static final byte HAS_LINE = 0x04;
        public static final byte HAS_NAME = 0x08;
        public static final byte HAS_TYPE = 0x10;
        public static final byte HAS_SIG = 0x20;

        public byte opcode_raw;
        public String opcode_str;
        public int register_num;
        public int addr_diff;
        public int line_diff;
        public int name_idx;
        public int type_idx;
        public int sig_idx;
        public String name_str;
        public TypeContainer.TypeItem type;
        public String sig_str;
        private byte flag;
        private boolean register_exists = false;
        private boolean addr_exists = false;
        private boolean line_exists = false;
        private boolean name_exists = false;
        private boolean type_exists = false;
        private boolean sig_exists = false;

        public DebugOpCode(byte opcode_raw, String opcode_str, byte flag, DexContainer dc, int... args) {
            this.opcode_raw = opcode_raw;
            this.opcode_str = opcode_str;
            this.flag = flag;
            int args_pos = 0;
            if ((flag & HAS_REG) != 0) {
                register_exists = true;
                register_num = args[args_pos++];
            }
            if ((flag & HAS_ADDR) != 0) {
                addr_exists = true;
                addr_diff = args[args_pos++];
            }
            if ((flag & HAS_LINE) != 0) {
                line_exists = true;
                line_diff = args[args_pos++];
            }
            if ((flag & HAS_NAME) != 0) {
                name_exists = true;
                name_idx = args[args_pos++];
                if (name_idx != GeneralUtil.NO_INDEX) name_str = dc.sc.items[name_idx].str_data;
                else name_str = "NO_INDEX";
            }
            if ((flag & HAS_TYPE) != 0) {
                type_exists = true;
                type_idx = args[args_pos++];
                if (type_idx != GeneralUtil.NO_INDEX) type = dc.tc.items[type_idx];
                else type = null;
            }
            if ((flag & HAS_SIG) != 0) {
                sig_exists = true;
                sig_idx = args[args_pos++];
                if (sig_idx != GeneralUtil.NO_INDEX) sig_str = dc.sc.items[sig_idx].str_data;
                else sig_str = "NO_INDEX";
            }
        }

        public boolean hasRegister() {
            return register_exists;
        }

        public boolean hasAddress() {
            return addr_exists;
        }

        public boolean hasLine() {
            return line_exists;
        }

        public boolean hasName() {
            return name_exists;
        }

        public boolean hasType() {
            return type_exists;
        }

        public boolean hasSig() {
            return sig_exists;
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(opcode_str);
            if (flag != 0) {
                sb.append(" : ");
                if (register_exists) sb.append("regs={" + register_num + "} ");
                if (addr_exists) sb.append("addr+=" + addr_diff + " ");
                if (line_exists) sb.append("line+=" + line_diff + " ");
                if (name_exists) sb.append("name=\"" + name_str + "\" ");
                if (type_exists) {
                    if (type_idx != GeneralUtil.NO_INDEX)
                        sb.append("type={" + type.toString() + "} ");
                    else sb.append("type={NO_INDEX} ");
                }
                if (sig_exists) sb.append("sig=\"" + sig_str + "\"");
            }
            return sb.toString();
        }
    }
}
