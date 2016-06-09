package anu.rsise.dexParser.encoding;

import anu.rsise.dexParser.container.MethodContainer;

public class EncodedMethod {

    public int method_idx_diff;
    public MethodContainer.MethodItem method;
    public int access_flags;
    public String access_flags_str;
    public int code_off;
    public CodeItem code;
}
