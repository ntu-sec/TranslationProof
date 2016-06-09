package anu.rsise.dexParser.container;

import anu.rsise.GeneralUtil;

public class DexContainer {

    public ClassContainer cc;
    public FieldContainer fc;
    public HeaderStructure hs;
    public MethodContainer mc;
    public ProtoContainer pc;
    public StringContainer sc;
    public TypeContainer tc;

    public void ResolveType() {
        for (int i = 0; i < tc.size; i++) {
            tc.items[i].str_type = sc.items[tc.items[i].descriptor_idx].str_data;
        }
    }

    public void ResolveProto() {
        for (int i = 0; i < pc.size; i++) {
            pc.items[i].shorty_desc = sc.items[pc.items[i].shorty_idx].str_data;
            pc.items[i].return_type_str = tc.items[pc.items[i].return_type_idx].str_type;
        }
    }


    public void ResolveField() {
        for (int i = 0; i < fc.size; i++) {
            fc.items[i].class_str = tc.items[fc.items[i].class_idx].str_type;
            fc.items[i].type_str = tc.items[fc.items[i].type_idx].str_type;
            fc.items[i].name_str = sc.items[fc.items[i].name_idx].str_data;
        }
    }

    public void ResolveMethod() {
        for (int i = 0; i < mc.size; i++) {
            mc.items[i].class_str = tc.items[mc.items[i].class_idx].str_type;
            mc.items[i].proto_item = pc.items[mc.items[i].proto_idx];
            mc.items[i].name_str = sc.items[mc.items[i].name_idx].str_data;
        }
    }

    public void ResolveClass() {
        for (int i = 0; i < cc.size; i++) {
            cc.items[i].class_str = tc.items[cc.items[i].class_idx].str_type;
            cc.items[i].superclass_str = tc.items[cc.items[i].superclass_idx].str_type;
            if (cc.items[i].source_file_idx != GeneralUtil.NO_INDEX)
                cc.items[i].source_file_str = sc.items[cc.items[i].source_file_idx].str_data;
            cc.items[i].access_flag_str = ClassContainer.ACCESS_FLAG.resolveClasses(cc.items[i].access_flag);
        }
    }

}
