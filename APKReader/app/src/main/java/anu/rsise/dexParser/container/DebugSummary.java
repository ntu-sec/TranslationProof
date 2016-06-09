package anu.rsise.dexParser.container;

import java.util.ArrayList;

import anu.rsise.GeneralUtil;
import anu.rsise.dexParser.encoding.DebugInfoItem;
import anu.rsise.dexParser.encoding.DebugInfoItem.DebugOpCode;

public class DebugSummary {

    public int start_line;
    public short register_size;
    public Register registers[];
    public int location_size;
    public ShortPair locations[]; // value_1 = address, value_2 = line
    private String indent;
    public DebugSummary(String owner_file, short start_line, short register_size, int opcode_size, DebugOpCode[] opcodes) {
        String current_file = owner_file;
        short current_line = start_line;
        short current_address = 0x00;

        this.start_line = start_line;
        this.register_size = register_size;
        registers = new Register[register_size];
        ArrayList<ShortPair> temp = new ArrayList<ShortPair>();
        ArrayList<ArrayList<ShortPair>> register_scopes = new ArrayList<ArrayList<ShortPair>>();
        for (int i = 0; i < register_size; i++) {
            register_scopes.add(new ArrayList<ShortPair>());
            registers[i] = new Register();
        }

        for (int i = 0; i < opcode_size; i++) {
            System.out.print(opcodes[i].opcode_raw);
            switch (opcodes[i].opcode_raw) {
                case (DebugInfoItem.DBG_END_SEQUENCE): // not used
                case (DebugInfoItem.DBG_SET_EPILOGUE_BEGIN): // not yet used
                case (DebugInfoItem.DBG_SET_PROLOGUE_END): {
                    break;
                } // not yet used
                case (DebugInfoItem.DBG_START_LOCAL):
                case (DebugInfoItem.DBG_START_LOCAL_EXTENDED): // at the moment the same treatment both local and extended
                {
                    registers[opcodes[i].register_num].register_name = opcodes[i].name_str;
                    registers[opcodes[i].register_num].register_type = opcodes[i].type.toString();
                }
                case (DebugInfoItem.DBG_RESTART_LOCAL): {
                    register_scopes.get(opcodes[i].register_num).add(new ShortPair(current_address, (short) 0, current_file));
                    registers[opcodes[i].register_num].scope_size++;
                    registers[opcodes[i].register_num].register_closed = false;
                    break;
                }
                case (DebugInfoItem.DBG_ADVANCE_LINE): {
                    current_line += opcodes[i].line_diff;
                    break;
                }
                case (DebugInfoItem.DBG_ADVANCE_PC): {
                    current_address += opcodes[i].addr_diff;
                    break;
                }
                case (DebugInfoItem.DBG_END_LOCAL): {
                    register_scopes.get(opcodes[i].register_num).
                            get(registers[opcodes[i].register_num].scope_size - 1).value_2 = current_address;
                    registers[opcodes[i].register_num].register_closed = true;
                    break;
                }
                case (DebugInfoItem.DBG_SET_FILE): {
                    current_file += opcodes[i].name_str;
                    break;
                }
                default: {
                    current_line += opcodes[i].line_diff;
                    current_address += opcodes[i].addr_diff;
                    temp.add(new ShortPair(current_address, current_line, current_file));
                    break;
                }
            }
        }

        for (int i = 0; i < register_size; i++) {
            if (!registers[i].register_closed)
                register_scopes.get(i).get(registers[i].scope_size - 1).value_2 = current_address;
            registers[i].scopes = register_scopes.get(i).toArray(new ShortPair[registers[i].scope_size]);
        }
        location_size = temp.size();
        locations = temp.toArray(new ShortPair[temp.size()]);
    }

    public String toString(String indent) {
        this.indent = indent;
        return toString();
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append(indent + "locations : \n");
        for (int i = 0; i < location_size; i++)
            sb.append(indent + "\t" + GeneralUtil.shortToHex(locations[i].value_1) + " line=" + locations[i].value_2 + "\n");
        sb.append(indent + "locals : \n");
        for (int i = 0; i < register_size; i++) {
            for (int j = 0; j < registers[i].scope_size; j++) {
                sb.append(indent + "\t" + GeneralUtil.shortToHex(registers[i].scopes[j].value_1) + " - " + GeneralUtil.shortToHex(registers[i].scopes[j].value_2) +
                        " reg=" + i + " " + registers[i].register_name + " " + registers[i].register_type + "\n");
            }
        }

        return sb.toString();
    }

    // used for start - end register and address line pair
    public static class ShortPair {
        public short value_1;
        public short value_2;
        public String file_name;

        public ShortPair(short value_1, short value_2, String file_name) {
            this.value_1 = value_1;
            this.value_2 = value_2;
            this.file_name = file_name;
        }
    }

    public static class Register {
        public String register_name;
        public String register_type;
        public int scope_size = 0;
        public boolean register_closed = true;
        public ShortPair[] scopes;
    }
}
