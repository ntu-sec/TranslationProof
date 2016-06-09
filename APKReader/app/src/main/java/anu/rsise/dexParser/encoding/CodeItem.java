package anu.rsise.dexParser.encoding;

import java.util.ArrayList;
import java.util.TreeMap;

import anu.rsise.dexParser.container.DebugSummary;
import anu.rsise.dexParser.container.DexContainer;

public class CodeItem {

    public short register_size;
    public short ins_size;
    public short outs_size;
    public short tries_size;
    public int debug_info_off;
    public DebugInfoItem debug_info;
    public DebugSummary debug_summary;
    public int insns_size; // this refers to the number of shorts of raw instructions
    public short[] insns_raw;
    public int insns_count; // this refers to the actual line of opcode after parsing
    public CodeInterpreter.OpCode[] insns;
    public TreeMap<Integer, CodeInterpreter.OpCode> insnMap; // the same as insns just in form of a map
    public short padding;
    public TryItem tries[];
    public EncodedCatchHandlerList handlers;

    public void interpretCode(DexContainer dc) {
        ArrayList<CodeInterpreter.OpCode> arr = new ArrayList<CodeInterpreter.OpCode>();
        insnMap = new TreeMap<Integer, CodeInterpreter.OpCode>();
        int pointer = 0;
        while (pointer < insns_size) {
            CodeInterpreter.OpCode temp = CodeInterpreter.interpretCode(dc, insns_raw, pointer);
            arr.add(temp);
            insnMap.put(temp.address, temp);
            pointer += temp.read_count;
        }
        insns_count = arr.size();
        insns = arr.toArray(new CodeInterpreter.OpCode[insns_count]);
    }

    public static class TryItem {
        public int start_addr;
        public short insn_count;
        public short handler_off;
        public EncodedCatchHandler handler;
    }
}
