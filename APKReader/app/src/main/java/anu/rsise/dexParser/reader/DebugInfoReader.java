package anu.rsise.dexParser.reader;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;

import anu.rsise.GeneralUtil;
import anu.rsise.dexParser.container.DexContainer;
import anu.rsise.dexParser.encoding.DebugInfoItem;
import anu.rsise.dexParser.encoding.DebugInfoItem.DebugOpCode;

public class DebugInfoReader {

    public static DebugInfoItem readDebugInfo(RandomAccessFile raf, DexContainer dc) throws IOException {
        DebugInfoItem dif = new DebugInfoItem();

        // read debug header
        dif.line_start = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
        dif.setSize(GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128));
        for (int i = 0; i < dif.parameter_size; i++) {
            dif.parameter_names[i] = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128p1);
            if (dif.parameter_names[i] != GeneralUtil.NO_INDEX)
                dif.parameter_names_str[i] = dc.sc.items[dif.parameter_names[i]].str_data;
            else dif.parameter_names_str[i] = "";
        }

        // read debug byte codes
        byte tmp;
        ArrayList<DebugOpCode> arr = new ArrayList<DebugOpCode>();
        while ((tmp = raf.readByte()) != DebugInfoItem.DBG_END_SEQUENCE) {
            switch (tmp) {
                case (DebugInfoItem.DBG_ADVANCE_PC): {
                    arr.add(new DebugOpCode(tmp, "DBG_ADVANCE_PC", DebugOpCode.HAS_ADDR, dc, GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128)));
                    break;
                }
                case (DebugInfoItem.DBG_ADVANCE_LINE): {
                    arr.add(new DebugOpCode(tmp, "DBG_ADVANCE_LINE", DebugOpCode.HAS_LINE, dc, GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.sleb128)));
                    break;
                }
                case (DebugInfoItem.DBG_START_LOCAL): {
                    arr.add(new DebugOpCode(tmp, "DBG_START_LOCAL", (byte) (DebugOpCode.HAS_REG | DebugOpCode.HAS_NAME | DebugOpCode.HAS_TYPE),
                            dc, GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128), GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128p1),
                            GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128p1)));
                    break;
                }
                case (DebugInfoItem.DBG_START_LOCAL_EXTENDED): {
                    arr.add(new DebugOpCode(tmp, "DBG_START_LOCAL_EXTENDED", (byte) (DebugOpCode.HAS_REG
                            | DebugOpCode.HAS_NAME | DebugOpCode.HAS_TYPE | DebugOpCode.HAS_SIG),
                            dc, GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128), GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128p1),
                            GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128p1), GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128p1)));
                    break;
                }
                case (DebugInfoItem.DBG_END_LOCAL): {
                    arr.add(new DebugOpCode(tmp, "DBG_END_LOCAL", DebugOpCode.HAS_REG, dc, GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128)));
                    break;
                }
                case (DebugInfoItem.DBG_RESTART_LOCAL): {
                    arr.add(new DebugOpCode(tmp, "DBG_RESTART_LOCAL", DebugOpCode.HAS_REG, dc, GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128)));
                    break;
                }
                case (DebugInfoItem.DBG_SET_PROLOGUE_END): {
                    arr.add(new DebugOpCode(tmp, "DBG_SET_PROLOGUE_END", DebugOpCode.HAS_NONE, dc));
                    break;
                }
                case (DebugInfoItem.DBG_SET_EPILOGUE_BEGIN): {
                    arr.add(new DebugOpCode(tmp, "DBG_SET_EPILOGUE_BEGIN", DebugOpCode.HAS_NONE, dc));
                    break;
                }
                case (DebugInfoItem.DBG_SET_FILE): {
                    arr.add(new DebugOpCode(tmp, "DGB_SET_FILE", DebugOpCode.HAS_NAME, dc, GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128p1)));
                    break;
                }
                default: {
                    arr.add(new DebugOpCode(tmp, Byte.toString(tmp), (byte) (DebugOpCode.HAS_ADDR | DebugOpCode.HAS_LINE), dc, ((tmp & 0xFF) - 0x0a) / DebugInfoItem.DBG_LINE_RANGE,
                            (DebugInfoItem.DBG_LINE_BASE + ((tmp & 0xFF) - 0x0a) % DebugInfoItem.DBG_LINE_RANGE)));
                    break;
                }
            }
        }
        arr.add(new DebugOpCode(DebugInfoItem.DBG_END_SEQUENCE, "DBG_END_SEQUENCE", DebugOpCode.HAS_NONE, dc));

        dif.debug_codes = arr.toArray(new DebugOpCode[arr.size()]);
        dif.size = arr.size();

        return dif;
    }

}
