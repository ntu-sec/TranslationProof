package anu.rsise.dexParser.encoding;

import anu.rsise.dexParser.container.DexContainer;
import anu.rsise.dexParser.container.FieldContainer;
import anu.rsise.dexParser.container.MethodContainer;
import anu.rsise.dexParser.container.StringContainer;
import anu.rsise.dexParser.container.TypeContainer;

public class CodeInterpreter {

    public static OpCode interpretCode(DexContainer dc, short[] bytecode, int index) {
        OpCode retVal = null;
        switch ((bytecode[index] & 0x00FF) >> 0) {
            case (0x00): {
                switch ((bytecode[index] & 0xFF00) >> 8) {
                    case (0x00):
                        retVal = new OpCode(index, OpCode.Code.Nop, "00 (nop)", 1, 0, OpCode.NO_PARAM, dc);
                        break;
                    // packed-switch-payload
                    case (0x01): {
                        int size = bytecode[index + 1];
                        long data[] = new long[size + 2];
                        data[0] = size;
                        data[1] = bytecode[index + 1] | (bytecode[index + 2] << 16);
                        for (int i = 1; i <= size; i++) {
                            data[i + 1] = bytecode[index + (2 * i) + 1] | (bytecode[index + (2 * i) + 2] << 16);
                        }
                        retVal = new OpCode(index, OpCode.Code.Payload, "00 (packed-switch-payload)", (size * 2) + 4,
                                size + 2, OpCode.NO_PARAM, dc, data);
                        break;
                    }
                    // sparse-switch-payload
                    case (0x02): {
                        int size = bytecode[index + 1];
                        long data[] = new long[2 * size + 1];
                        data[0] = size;
                        for (int i = 0; i < size; i++) {
                            data[i + 1] = bytecode[index + (2 * (i + 1))] | (bytecode[index + (2 * (i + 1)) + 1] << 16);
                        }
                        for (int i = 0; i < size; i++) {
                            data[size + i + 1] = bytecode[index + (2 * (size + i + 1))] |
                                    (bytecode[index + (2 * (size + i + 1)) + 1] << 16);
                        }
                        retVal = new OpCode(index, OpCode.Code.Payload, "00 (sparse-switch-payload)", (size * 4) + 2,
                                2 * size + 1, OpCode.NO_PARAM, dc, data);
                        break;
                    }
                    // fill-array-data-payload
                    case (0x03): {
                        int width = bytecode[index + 1];
                        int size = bytecode[index + 2] | (bytecode[index + 3] << 16);
                        long data[] = new long[width * size + 2];
                        data[0] = width;
                        data[1] = size;
                        for (int i = 0; i < (size * width) / 2; i++) {
                            data[2 * i + 2] = bytecode[index + i + 4] & 255;
                            data[2 * i + 3] = bytecode[index + i + 4] >> 8;
                        }
                        retVal = new OpCode(index, OpCode.Code.Payload, "00 (fill-array-data)",
                                (size * width + 1) / 2 + 4,
                                size * width + 2, OpCode.NO_PARAM, dc, data);
                        break;
                    }
                }
                break;
            }
            case (0x01):
                retVal = new OpCode(index, OpCode.Code.Move, "01 (move)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x02):
                retVal = new OpCode(index, OpCode.Code.Move, "02 (move/from16)", 2, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x03):
                retVal = new OpCode(index, OpCode.Code.Move, "04 (move/16)", 3, 2, OpCode.NO_PARAM, dc,
                        bytecode[index + 1], bytecode[index + 2]);
                break;
            case (0x04):
                retVal = new OpCode(index, OpCode.Code.Move, "04 (move-wide)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x05):
                retVal = new OpCode(index, OpCode.Code.Move, "05 (move-wide/from16)", 2, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x06):
                retVal = new OpCode(index, OpCode.Code.Move, "06 (move-wide/16)", 3, 2, OpCode.NO_PARAM, dc,
                        bytecode[index + 1], bytecode[index + 2]);
                break;
            case (0x07):
                retVal = new OpCode(index, OpCode.Code.Move, "07 (move-object)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x08):
                retVal = new OpCode(index, OpCode.Code.Move, "08 (move-object/from16)", 2, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x09):
                retVal = new OpCode(index, OpCode.Code.Move, "09 (move-object/16)", 3, 2, OpCode.NO_PARAM, dc,
                        bytecode[index + 1], bytecode[index + 2]);
                break;
            case (0x0A):
                retVal = new OpCode(index, OpCode.Code.MoveResult, "0A (move-result)", 1, 1, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8);
                break;
            case (0x0B):
                retVal = new OpCode(index, OpCode.Code.MoveResult, "0B (move-result-wide)", 1, 1, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8);
                break;
            case (0x0C):
                retVal = new OpCode(index, OpCode.Code.MoveResult, "0C (move-result-object)", 1, 1, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8);
                break;
            case (0x0D):
                retVal = new OpCode(index, OpCode.Code.MoveException, "0D (move-exception)", 1, 1, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8);
                break;
            case (0x0E):
                retVal = new OpCode(index, OpCode.Code.Return, "0E (return-void)", 1, 0, OpCode.NO_PARAM, dc);
                break;
            case (0x0F):
                retVal = new OpCode(index, OpCode.Code.Return, "0F (return)", 1, 1, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8);
                break;

            case (0x10):
                retVal = new OpCode(index, OpCode.Code.Return, "10 (return-wide)", 1, 1, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8);
                break;
            case (0x11):
                retVal = new OpCode(index, OpCode.Code.Return, "11 (return-object)", 1, 1, OpCode.NO_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8);
                break;
            case (0x12):
                retVal = new OpCode(index, OpCode.Code.Const, "12 (const/4)", 1, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x13):
                retVal = new OpCode(index, OpCode.Code.Const, "13 (const/16)", 2, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x14):
                retVal = new OpCode(index, OpCode.Code.Const, "14 (const)", 3, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, ((bytecode[index + 2] << 16) | bytecode[index + 1]));
                break;
            case (0x15):
                retVal = new OpCode(index, OpCode.Code.Const, "15 (const/high16)", 2, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, ((bytecode[index + 1]) << 16));
                break;
            case (0x16):
                retVal = new OpCode(index, OpCode.Code.Const, "16 (const-wide/16)", 2, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x17):
                retVal = new OpCode(index, OpCode.Code.Const, "17 (const-wide/32)", 3, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, ((bytecode[index + 2] << 8) | bytecode[index + 1]));
                break;
            case (0x18):
                retVal = new OpCode(index, OpCode.Code.Const, "18 (const-wide)", 5, 1, OpCode.LITERAL_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, ((bytecode[4] << 48) | (bytecode[3] << 32) | (bytecode[index + 2] << 16) | (bytecode[index + 1] << 0)));
                break;
            case (0x19):
                retVal = new OpCode(index, OpCode.Code.Const, "19 (const-wide/high16)", 2, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1] << 48);
                break;
            case (0x1a):
                retVal = new OpCode(index, OpCode.Code.Const, "1A (const-string)", 2, 1, OpCode.STRING_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x1b):
                retVal = new OpCode(index, OpCode.Code.Const, "1B (const-string/jumbo)", 3, 1, OpCode.STRING_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 2] << 16) | bytecode[index + 1]);
                break;
            case (0x1c):
                retVal = new OpCode(index, OpCode.Code.Const, "1C (const-class)", 2, 1, OpCode.TYPE_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x1d):
                retVal = new OpCode(index, OpCode.Code.MonitorEnter, "1D (monitor-enter)", 1, 1, OpCode.NO_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8);
                break;
            case (0x1e):
                retVal = new OpCode(index, OpCode.Code.MonitorExit, "1E (monitor-exit)", 1, 1, OpCode.NO_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8);
                break;
            case (0x1f):
                retVal = new OpCode(index, OpCode.Code.CheckCast, "1F (check-cast)", 2, 1, OpCode.TYPE_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x20):
                retVal = new OpCode(index, OpCode.Code.InstanceOf, "20 (instance-of)", 2, 2, OpCode.TYPE_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x21):
                retVal = new OpCode(index, OpCode.Code.ArrayLength, "21 (array-length)", 1, 2, OpCode.NO_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x22):
                retVal = new OpCode(index, OpCode.Code.NewInstance, "22 (new-instance)", 2, 1, OpCode.TYPE_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x23):
                retVal = new OpCode(index, OpCode.Code.NewArray, "23 (new-array)", 2, 2, OpCode.TYPE_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x24):
                retVal = get35c(index, OpCode.Code.NewArray, "24 (filled-new-array)", bytecode, OpCode.TYPE_PARAM, dc);
            case (0x25): {
                // rough version : just take note of the starting register and the range (only for cosmetics for now)
                int register_start = bytecode[index + 2];
                int register_size = (bytecode[index] & 0xFF00) >> 8;
                retVal = new OpCode(index, OpCode.Code.NewArray, "25 (filled-new-array/range)", 3, 1,
                        OpCode.LITERAL_PARAM | OpCode.TYPE_PARAM, dc, register_start, register_size, bytecode[index + 1]);
                break;
            }
            case (0x26):
                retVal = new OpCode(index, OpCode.Code.FillArrayData, "26 (fill-array-data)", 3, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 2] << 16) | bytecode[index + 1]);
                break;
            case (0x27):
                retVal = new OpCode(index, OpCode.Code.Throw, "27 (throw)", 1, 1, OpCode.NO_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8);
                break;
            case (0x28):
                retVal = new OpCode(index, OpCode.Code.Goto, "28 (goto)", 1, 0, OpCode.LITERAL_PARAM,
                        dc, index + (byte) ((bytecode[index] & 0xFF00) >> 8));
                break;
            case (0x29):
                retVal = new OpCode(index, OpCode.Code.Goto, "29 (goto/16)", 2, 0, OpCode.LITERAL_PARAM,
                        dc, index + (short) bytecode[index + 1]);
                break;
            case (0x2a):
                retVal = new OpCode(index, OpCode.Code.Goto, "2A (goto/32)", 3, 0, OpCode.LITERAL_PARAM,
                        dc, index + (int) ((bytecode[index + 2] << 16) | bytecode[index + 1]));
                break;

            case (0x2b):
                retVal = new OpCode(index, OpCode.Code.PackedSwitch, "2B (packed-switch)", 3, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, index + (bytecode[index + 2] << 16) | bytecode[index + 1]);
                break;
            case (0x2c):
                retVal = new OpCode(index, OpCode.Code.SparseSwitch, "2C (sparse-switch)", 3, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, index + (bytecode[index + 2] << 16) | bytecode[index + 1]);
                break;

            case (0x2d):
                retVal = new OpCode(index, OpCode.Code.Compare, "2D (cmpl-float)", 2, 3, OpCode.NO_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x2e):
                retVal = new OpCode(index, OpCode.Code.Compare, "2E (cmpg-float)", 2, 3, OpCode.NO_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x2f):
                retVal = new OpCode(index, OpCode.Code.Compare, "2F (cmpl-double)", 2, 3, OpCode.NO_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x30):
                retVal = new OpCode(index, OpCode.Code.Compare, "30 (cmpg-double)", 2, 3, OpCode.NO_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x31):
                retVal = new OpCode(index, OpCode.Code.Compare, "31 (cmp-long)", 2, 3, OpCode.NO_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;

            case (0x32):
                retVal = new OpCode(index, OpCode.Code.If, "32 (if-eq)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, index + bytecode[index + 1]);
                break;
            case (0x33):
                retVal = new OpCode(index, OpCode.Code.If, "33 (if-ne)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, index + bytecode[index + 1]);
                break;
            case (0x34):
                retVal = new OpCode(index, OpCode.Code.If, "34 (if-lt)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, index + bytecode[index + 1]);
                break;
            case (0x35):
                retVal = new OpCode(index, OpCode.Code.If, "35 (if-ge)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, index + bytecode[index + 1]);
                break;
            case (0x36):
                retVal = new OpCode(index, OpCode.Code.If, "36 (if-gt)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, index + bytecode[index + 1]);
                break;
            case (0x37):
                retVal = new OpCode(index, OpCode.Code.If, "37 (if-le)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, index + bytecode[index + 1]);
                break;

            case (0x38):
                retVal = new OpCode(index, OpCode.Code.If, "38 (if-eqz)", 2, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, index + bytecode[index + 1]);
                break;
            case (0x39):
                retVal = new OpCode(index, OpCode.Code.If, "39 (if-nez)", 2, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, index + bytecode[index + 1]);
                break;
            case (0x3a):
                retVal = new OpCode(index, OpCode.Code.If, "3a (if-ltz)", 2, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, index + bytecode[index + 1]);
                break;
            case (0x3b):
                retVal = new OpCode(index, OpCode.Code.If, "3b (if-gez)", 2, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, index + bytecode[index + 1]);
                break;
            case (0x3c):
                retVal = new OpCode(index, OpCode.Code.If, "3c (if-gtz)", 2, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, index + bytecode[index + 1]);
                break;
            case (0x3d):
                retVal = new OpCode(index, OpCode.Code.If, "3d (if-lez)", 2, 1, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, index + bytecode[index + 1]);
                break;
            // 3e - 43 is unused
            case (0x44):
                retVal = new OpCode(index, OpCode.Code.Aget, "44 (aget)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x45):
                retVal = new OpCode(index, OpCode.Code.Aget, "45 (aget-wide)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x46):
                retVal = new OpCode(index, OpCode.Code.Aget, "46 (aget-object)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x47):
                retVal = new OpCode(index, OpCode.Code.Aget, "47 (aget-boolean)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x48):
                retVal = new OpCode(index, OpCode.Code.Aget, "48 (aget-byte)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x49):
                retVal = new OpCode(index, OpCode.Code.Aget, "49 (aget-char)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x4a):
                retVal = new OpCode(index, OpCode.Code.Aget, "4A (aget-short)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x4b):
                retVal = new OpCode(index, OpCode.Code.Aput, "4B (aput)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x4c):
                retVal = new OpCode(index, OpCode.Code.Aput, "4C (aput-wide)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x4d):
                retVal = new OpCode(index, OpCode.Code.Aput, "4D (aput-object)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x4e):
                retVal = new OpCode(index, OpCode.Code.Aput, "4E (aput-boolean)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x4f):
                retVal = new OpCode(index, OpCode.Code.Aput, "4F (aput-byte)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x50):
                retVal = new OpCode(index, OpCode.Code.Aput, "50 (aput-char)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x51):
                retVal = new OpCode(index, OpCode.Code.Aput, "51 (aput-short)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;

            case (0x52):
                retVal = new OpCode(index, OpCode.Code.Iget, "52 (iget)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x53):
                retVal = new OpCode(index, OpCode.Code.Iget, "53 (iget-wide)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x54):
                retVal = new OpCode(index, OpCode.Code.Iget, "54 (iget-object)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x55):
                retVal = new OpCode(index, OpCode.Code.Iget, "55 (iget-boolean)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x56):
                retVal = new OpCode(index, OpCode.Code.Iget, "56 (iget-byte)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x57):
                retVal = new OpCode(index, OpCode.Code.Iget, "57 (iget-char)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x58):
                retVal = new OpCode(index, OpCode.Code.Iget, "58 (iget-short)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x59):
                retVal = new OpCode(index, OpCode.Code.Iput, "59 (iput)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x5a):
                retVal = new OpCode(index, OpCode.Code.Iput, "5A (iput-wide)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x5b):
                retVal = new OpCode(index, OpCode.Code.Iput, "5B (iput-object)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x5c):
                retVal = new OpCode(index, OpCode.Code.Iput, "5C (iput-boolean)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x5d):
                retVal = new OpCode(index, OpCode.Code.Iput, "5D (iput-byte)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x5e):
                retVal = new OpCode(index, OpCode.Code.Iput, "5E (iput-char)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0x5f):
                retVal = new OpCode(index, OpCode.Code.Iput, "5F (iput-short)", 2, 2, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;

            case (0x60):
                retVal = new OpCode(index, OpCode.Code.Sget, "60 (sget)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x61):
                retVal = new OpCode(index, OpCode.Code.Sget, "61 (sget-wide)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x62):
                retVal = new OpCode(index, OpCode.Code.Sget, "62 (sget-object)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x63):
                retVal = new OpCode(index, OpCode.Code.Sget, "63 (sget-boolean)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x64):
                retVal = new OpCode(index, OpCode.Code.Sget, "64 (sget-byte)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x65):
                retVal = new OpCode(index, OpCode.Code.Sget, "65 (sget-char)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x66):
                retVal = new OpCode(index, OpCode.Code.Sget, "66 (sget-short)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x67):
                retVal = new OpCode(index, OpCode.Code.Sput, "67 (sput)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x68):
                retVal = new OpCode(index, OpCode.Code.Sput, "68 (sput-wide)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x69):
                retVal = new OpCode(index, OpCode.Code.Sput, "69 (sput-object)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x6a):
                retVal = new OpCode(index, OpCode.Code.Sput, "6A (sput-boolean)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x6b):
                retVal = new OpCode(index, OpCode.Code.Sput, "6B (sput-byte)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x6c):
                retVal = new OpCode(index, OpCode.Code.Sput, "6C (sput-char)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;
            case (0x6d):
                retVal = new OpCode(index, OpCode.Code.Sput, "6D (sput-short)", 2, 1, OpCode.FIELD_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, bytecode[index + 1]);
                break;

            case (0x6e):
                retVal = get35c(index, OpCode.Code.Invoke, "6E (invoke-virtual)", bytecode, OpCode.METHOD_PARAM, dc);
                break;
            case (0x6f):
                retVal = get35c(index, OpCode.Code.Invoke, "6F (invoke-super)", bytecode, OpCode.METHOD_PARAM, dc);
                break;
            case (0x70):
                retVal = get35c(index, OpCode.Code.Invoke, "70 (invoke-direct)", bytecode, OpCode.METHOD_PARAM, dc);
                break;
            case (0x71):
                retVal = get35c(index, OpCode.Code.Invoke, "71 (invoke-static)", bytecode, OpCode.METHOD_PARAM, dc);
                break;
            case (0x72):
                retVal = get35c(index, OpCode.Code.Invoke, "72 (invoke-interface)", bytecode, OpCode.METHOD_PARAM, dc);
                break;
            // 0x73 is unused
            case (0x74): {
                // rough version : just take note of the starting register and the range (only for cosmetics for now)
                int register_start = bytecode[index + 2];
                int register_size = (bytecode[index] & 0xFF00) >> 8;
                retVal = new OpCode(index, OpCode.Code.Invoke, "74 (invoke-virtual/range)", 3, 1, OpCode.METHOD_PARAM | OpCode.METHOD_PARAM, dc,
                        register_start, register_size, bytecode[index + 1]);
                break;
            }
            case (0x75): {
                // rough version : just take note of the starting register and the range (only for cosmetics for now)
                int register_start = bytecode[index + 2];
                int register_size = (bytecode[index] & 0xFF00) >> 8;
                retVal = new OpCode(index, OpCode.Code.Invoke, "75 (invoke-super/range)", 3, 1, OpCode.METHOD_PARAM | OpCode.METHOD_PARAM, dc,
                        register_start, register_size, bytecode[index + 1]);
                break;
            }
            case (0x76): {
                // rough version : just take note of the starting register and the range (only for cosmetics for now)
                int register_start = bytecode[index + 2];
                int register_size = (bytecode[index] & 0xFF00) >> 8;
                retVal = new OpCode(index, OpCode.Code.Invoke, "76 (invoke-direct/range)", 3, 1, OpCode.METHOD_PARAM | OpCode.METHOD_PARAM,
                        dc, register_start, register_size, bytecode[index + 1]);
                break;
            }
            case (0x77): {
                // rough version : just take note of the starting register and the range (only for cosmetics for now)
                int register_start = bytecode[index + 2];
                int register_size = (bytecode[index] & 0xFF00) >> 8;
                retVal = new OpCode(index, OpCode.Code.Invoke, "77 (invoke-static/range)", 3, 1, OpCode.METHOD_PARAM | OpCode.METHOD_PARAM,
                        dc, register_start, register_size, bytecode[index + 1]);
                break;
            }
            case (0x78): {
                // rough version : just take note of the starting register and the range (only for cosmetics for now)
                int register_start = bytecode[index + 2];
                int register_size = (bytecode[index] & 0xFF00) >> 8;
                retVal = new OpCode(index, OpCode.Code.Invoke, "78 (invoke-interface/range)", 3, 1, OpCode.METHOD_PARAM | OpCode.METHOD_PARAM,
                        dc, register_start, register_size, bytecode[index + 1]);
                break;
            }
            // 0x79 - 0x7a are unused
            case (0x7b):
                retVal = new OpCode(index, OpCode.Code.Unop, "7B (neg-int)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x7c):
                retVal = new OpCode(index, OpCode.Code.Unop, "7C (not-int)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x7d):
                retVal = new OpCode(index, OpCode.Code.Unop, "7D (neg-long)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x7e):
                retVal = new OpCode(index, OpCode.Code.Unop, "7E (not-long)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x7f):
                retVal = new OpCode(index, OpCode.Code.Unop, "7F (neg-float)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x80):
                retVal = new OpCode(index, OpCode.Code.Unop, "80 (neg-double)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x81):
                retVal = new OpCode(index, OpCode.Code.Unop, "81 (int-to-long)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x82):
                retVal = new OpCode(index, OpCode.Code.Unop, "82 (int-to-float)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x83):
                retVal = new OpCode(index, OpCode.Code.Unop, "83 (int-to-double)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x84):
                retVal = new OpCode(index, OpCode.Code.Unop, "84 (long-to-int)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x85):
                retVal = new OpCode(index, OpCode.Code.Unop, "85 (long-to-float)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x86):
                retVal = new OpCode(index, OpCode.Code.Unop, "86 (long-to-double)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x87):
                retVal = new OpCode(index, OpCode.Code.Unop, "87 (float-to-int)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x88):
                retVal = new OpCode(index, OpCode.Code.Unop, "88 (float-to-long)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x89):
                retVal = new OpCode(index, OpCode.Code.Unop, "89 (float-to-double)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x8a):
                retVal = new OpCode(index, OpCode.Code.Unop, "8A (double-to-int)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x8b):
                retVal = new OpCode(index, OpCode.Code.Unop, "8B (double-to-long)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x8c):
                retVal = new OpCode(index, OpCode.Code.Unop, "8C (double-to-float)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x8d):
                retVal = new OpCode(index, OpCode.Code.Unop, "8D (int-to-byte)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x8e):
                retVal = new OpCode(index, OpCode.Code.Unop, "8E (int-to-char)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0x8f):
                retVal = new OpCode(index, OpCode.Code.Unop, "8F (int-to-short)", 1, 2, OpCode.NO_PARAM, dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;

            case (0x90):
                retVal = new OpCode(index, OpCode.Code.Binop, "90 (add-int)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x91):
                retVal = new OpCode(index, OpCode.Code.Binop, "91 (sub-int)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x92):
                retVal = new OpCode(index, OpCode.Code.Binop, "92 (mul-int)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x93):
                retVal = new OpCode(index, OpCode.Code.Binop, "93 (div-int)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x94):
                retVal = new OpCode(index, OpCode.Code.Binop, "94 (rem-int)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x95):
                retVal = new OpCode(index, OpCode.Code.Binop, "95 (and-int)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x96):
                retVal = new OpCode(index, OpCode.Code.Binop, "96 (or-int)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x97):
                retVal = new OpCode(index, OpCode.Code.Binop, "97 (xor-int)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x98):
                retVal = new OpCode(index, OpCode.Code.Binop, "98 (shl-int)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x99):
                retVal = new OpCode(index, OpCode.Code.Binop, "99 (shr-int)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x9a):
                retVal = new OpCode(index, OpCode.Code.Binop, "9A (ushr-int)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x9b):
                retVal = new OpCode(index, OpCode.Code.Binop, "9B (add-long)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x9c):
                retVal = new OpCode(index, OpCode.Code.Binop, "9C (sub-long)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x9d):
                retVal = new OpCode(index, OpCode.Code.Binop, "9D (mul-long)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x9e):
                retVal = new OpCode(index, OpCode.Code.Binop, "9E (div-long)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0x9f):
                retVal = new OpCode(index, OpCode.Code.Binop, "9F (rem-long)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xa0):
                retVal = new OpCode(index, OpCode.Code.Binop, "A0 (and-long)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xa1):
                retVal = new OpCode(index, OpCode.Code.Binop, "A1 (or-long)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xa2):
                retVal = new OpCode(index, OpCode.Code.Binop, "A2 (xor-long)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xa3):
                retVal = new OpCode(index, OpCode.Code.Binop, "A3 (shl-long)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xa4):
                retVal = new OpCode(index, OpCode.Code.Binop, "A4 (shr-long)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xa5):
                retVal = new OpCode(index, OpCode.Code.Binop, "A5 (ushr-long)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xa6):
                retVal = new OpCode(index, OpCode.Code.Binop, "A6 (add-float)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xa7):
                retVal = new OpCode(index, OpCode.Code.Binop, "A7 (sub-float)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xa8):
                retVal = new OpCode(index, OpCode.Code.Binop, "A8 (mul-float)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xa9):
                retVal = new OpCode(index, OpCode.Code.Binop, "A9 (div-float)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xaa):
                retVal = new OpCode(index, OpCode.Code.Binop, "AA (rem-float)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xab):
                retVal = new OpCode(index, OpCode.Code.Binop, "AB (add-double)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xac):
                retVal = new OpCode(index, OpCode.Code.Binop, "AC (sub-double)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xad):
                retVal = new OpCode(index, OpCode.Code.Binop, "AD (mul-double)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xae):
                retVal = new OpCode(index, OpCode.Code.Binop, "AE (div-double)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xaf):
                retVal = new OpCode(index, OpCode.Code.Binop, "AF (rem-double)", 2, 3, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;

            case (0xb0):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "B0 (add-int/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xb1):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "B1 (sub-int/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xb2):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "B2 (mul-int/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xb3):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "B3 (div-int/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xb4):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "B4 (rem-int/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xb5):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "B5 (and-int/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xb6):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "B6 (or-int/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xb7):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "B7 (xor-int/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xb8):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "B8 (shl-int/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xb9):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "B9 (shr-int/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xba):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "BA (ushr-int/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xbb):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "BB (add-long/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xbc):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "BC (sub-long/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xbd):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "BD (mul-long/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xbe):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "BE (div-long/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xbf):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "BF (rem-long/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xc0):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "C0 (and-long/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xc1):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "C1 (or-long/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xc2):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "C2 (xor-long/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xc3):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "C3 (shl-long/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xc4):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "C4 (shr-long/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xc5):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "C5 (ushr-long/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xc6):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "C6 (add-float/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xc7):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "C7 (sub-float/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xc8):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "C8 (mul-float/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xc9):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "C9 (div-float/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xca):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "CA (rem-float/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xcb):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "CB (add-double/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xcc):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "CC (sub-double/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xcd):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "CD (mul-double/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xce):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "CE (div-double/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;
            case (0xcf):
                retVal = new OpCode(index, OpCode.Code.Binop2addr, "CF (rem-double/2addr)", 1, 2, OpCode.NO_PARAM, dc,
                        (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12);
                break;

            case (0xd0):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "D0 (add-int/lit16)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0xd1):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "D1 (rsub-int(reverse subtract)/lit16)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0xd2):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "D2 (mul-int/lit16)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0xd3):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "D3 (div-int/lit16)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0xd4):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "D4 (rem-int/lit16)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0xd5):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "D5 (and-int/lit16)", 2, 2, OpCode.LITERAL_PARAM, dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0xd6):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "D6 (or-int/lit16)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (0xd7):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "D7 (xor-int/lit16)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0x0F00) >> 8, (bytecode[index] & 0xF000) >> 12, bytecode[index + 1]);
                break;

            case (0xd8):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "D8 (add-int/lit8)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xd9):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "D9 (rsub-int/lit8)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xda):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "DA (mul-int/lit8)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xdb):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "DB (div-int/lit8)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xdc):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "DC (rem-int/lit8)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xdd):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "DD (and-int/lit8)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xde):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "DE (or-int/lit8)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xdf):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "DF (xor-int/lit8)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xe0):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "E0 (shl-int/lit8)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xe1):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "E1 (shr-int/lit8)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;
            case (0xe2):
                retVal = new OpCode(index, OpCode.Code.BinopConst, "E2 (ushr-int/lit8)", 2, 2, OpCode.LITERAL_PARAM,
                        dc, (bytecode[index] & 0xFF00) >> 8, (bytecode[index + 1] & 0x00FF) >> 0, (bytecode[index + 1] & 0xFF00) >> 8);
                break;

            // 0xe3 - 0xff are not used

            default:
                retVal = new OpCode(index, OpCode.Code.Unused, "XX (unused)", 1, 0, OpCode.NO_PARAM, dc);
                break;
        }

        return retVal;
    }

    private static OpCode get35c(int index, OpCode.Code code, String str_code, short[] bytecode,
                                 int param_flag, DexContainer dc) {
        OpCode retVal = null;
        switch ((bytecode[index] & 0xF000) >> 12) {
            case (5):
                retVal = new OpCode(index, code, str_code, 3, 5, param_flag, dc, (bytecode[index + 2] & 0x000F), (bytecode[index + 2] & 0x00F0) >> 4,
                        (bytecode[index + 2] & 0x0F00) >> 8, (bytecode[index + 2] & 0xF000) >> 12, (bytecode[index] & 0x0F00) >> 8, bytecode[index + 1]);
                break;
            case (4):
                retVal = new OpCode(index, code, str_code, 3, 4, param_flag, dc, (bytecode[index + 2] & 0x000F), (bytecode[index + 2] & 0x00F0) >> 4,
                        (bytecode[index + 2] & 0x0F00) >> 8, (bytecode[index + 2] & 0xF000) >> 12, bytecode[index + 1]);
                break;
            case (3):
                retVal = new OpCode(index, code, str_code, 3, 3, param_flag, dc, (bytecode[index + 2] & 0x000F), (bytecode[index + 2] & 0x00F0) >> 4,
                        (bytecode[index + 2] & 0x0F00) >> 8, bytecode[index + 1]);
                break;
            case (2):
                retVal = new OpCode(index, code, str_code, 3, 2, param_flag, dc, (bytecode[index + 2] & 0x000F), (bytecode[index + 2] & 0x00F0) >> 4, bytecode[index + 1]);
                break;
            case (1):
                retVal = new OpCode(index, code, str_code, 3, 1, param_flag, dc, (bytecode[index + 2] & 0x000F), bytecode[index + 1]);
                break;
            case (0):
                retVal = new OpCode(index, code, str_code, 3, 0, param_flag, dc, bytecode[index + 1]);
                break;
        }
        return retVal;
    }

    public static class OpCode {
        public static final int LITERAL_PARAM = 0x10;
        public static final int STRING_PARAM = 0x08;
        public static final int TYPE_PARAM = 0x04;
        public static final int FIELD_PARAM = 0x02;
        public static final int METHOD_PARAM = 0x01;
        public static final int NO_PARAM = 0x00;
        public int address;
        public Code code;
        public String str_code;
        public int register_size;
        public boolean literal_exists;
        public boolean string_exists;
        public boolean type_exists;
        public boolean field_exists;
        public boolean method_exists;
        public int[] registers;
        public long literal;
        public StringContainer.StringItem string_item;
        public TypeContainer.TypeItem type_item;
        public FieldContainer.FieldItem field_item;
        public MethodContainer.MethodItem method_item;
        public int read_count;
        public OpCode(int address, Code code, String str_code, int read_count, int register_size, int param, DexContainer dc, long... values) {
            this.address = address;
            this.code = code;
            this.str_code = str_code;
            this.register_size = register_size;
            this.literal_exists = (param & 0x10) != 0;
            this.string_exists = (param & 0x08) != 0;
            this.type_exists = (param & 0x04) != 0;
            this.field_exists = (param & 0x02) != 0;
            this.method_exists = (param & 0x01) != 0;
            this.read_count = read_count;

            registers = new int[register_size];
            for (int i = 0; i < register_size; i++) this.registers[i] = (int) values[i];

            int idx = register_size;

            if (literal_exists) {
                this.literal = values[idx++];
            }
            if (string_exists) {
                string_item = dc.sc.items[(int) values[idx++]];
            }
            if (type_exists) {
                type_item = dc.tc.items[(int) values[idx++]];
            }
            if (field_exists) {
                field_item = dc.fc.items[(int) values[idx++]];
            }
            if (method_exists) {
                method_item = dc.mc.items[(int) values[idx++]];
            }

        }

        public boolean isStringExists() {
            return string_exists;
        }

        public boolean isTypeExists() {
            return type_exists;
        }

        public boolean isFieldExists() {
            return field_exists;
        }

        public boolean isMethodExists() {
            return method_exists;
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();

            sb.append(address + " : " + str_code + " - Registers : {");
            if (register_size > 0) {
                sb.append("v" + registers[0]);
                for (int i = 1; i < register_size; i++) {
                    sb.append(", v" + registers[i]);
                }
            }
            sb.append("}");
            if (literal_exists) sb.append(" -> " + literal + "@value");
            if (string_exists) sb.append(" -> " + string_item.str_data + " (string)");
            if (type_exists) sb.append(" -> " + type_item.str_type + " (type)");
            if (field_exists) sb.append(" -> " + field_item.toString() + " (field)");
            if (method_exists) sb.append(" -> " + method_item.toString() + " (method)");

            return sb.toString();
        }

        public static enum Code {
            Nop, Move, MoveResult, MoveException, Return, Const, MonitorEnter, MonitorExit, CheckCast, InstanceOf,
            ArrayLength, NewInstance, NewArray, FillArrayData, Throw, Goto, PackedSwitch, SparseSwitch,
            Compare, If, Aget, Aput, Iget, Iput, Sget, Sput, Invoke, Binop, Binop2addr, BinopConst, Unop,
            Payload, Unused
        }
    }
}
