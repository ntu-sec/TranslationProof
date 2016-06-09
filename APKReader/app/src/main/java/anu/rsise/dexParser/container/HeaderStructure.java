package anu.rsise.dexParser.container;

import anu.rsise.GeneralUtil;

public class HeaderStructure {
    public byte magic[];
    public int checksum;
    public byte signature[];
    public int file_size;
    public int header_size;
    public int endian_tag;
    public int link_size;
    public int link_off;
    public int map_off;
    public int string_ids_size;
    public int string_ids_off;
    public int type_ids_size;
    public int type_ids_off;
    public int proto_ids_size;
    public int proto_ids_off;
    public int field_ids_size;
    public int field_ids_off;
    public int method_ids_size;
    public int method_ids_off;
    public int class_defs_size;
    public int class_defs_off;
    public int data_size;
    public int data_off;

    public HeaderStructure() {
        magic = new byte[8];
        checksum = 0;
        signature = new byte[20];
        file_size = 0;
        header_size = 0;
        endian_tag = 0;
        link_size = 0;
        link_off = 0;
        map_off = 0;
        string_ids_size = 0;
        string_ids_off = 0;
        type_ids_size = 0;
        type_ids_off = 0;
        proto_ids_size = 0;
        proto_ids_off = 0;
        field_ids_size = 0;
        field_ids_off = 0;
        method_ids_size = 0;
        method_ids_off = 0;
        class_defs_size = 0;
        class_defs_off = 0;
        data_size = 0;
        data_off = 0;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("############################\n");
        sb.append("#     header structure     #\n");
        sb.append("############################\n");

        sb.append("magic\t\t: ");
        for (int i = 0; i < 3; i++) sb.append(Character.toString((char) magic[i]));
        for (int i = 4; i < 7; i++) sb.append(Character.toString((char) magic[i]));
        sb.append("\n");

        sb.append("checksum\t: ");
        sb.append(GeneralUtil.valueToHex((checksum >> 28) & 0x0F));
        sb.append(GeneralUtil.valueToHex((checksum >> 24) & 0x0F));
        sb.append(GeneralUtil.valueToHex((checksum >> 20) & 0x0F));
        sb.append(GeneralUtil.valueToHex((checksum >> 16) & 0x0F));
        sb.append(GeneralUtil.valueToHex((checksum >> 12) & 0x0F));
        sb.append(GeneralUtil.valueToHex((checksum >> 8) & 0x0F));
        sb.append(GeneralUtil.valueToHex((checksum >> 4) & 0x0F));
        sb.append(GeneralUtil.valueToHex((checksum >> 0) & 0x0F));

        sb.append(" - " + checksum);
        sb.append("\n");

        sb.append("signature\t: ");
        for (int i = 0; i < 20; i++) {
            sb.append(Character.toString(GeneralUtil.valueToHex((signature[i] >> 4) & 0x0F)));
            sb.append(Character.toString(GeneralUtil.valueToHex(signature[i] & 0x0F)));
        }
        sb.append("\n");

        sb.append("file size\t: ");
        sb.append(Long.toString(file_size) + "\n");

        sb.append("header_size\t: ");
        sb.append(Long.toString(header_size) + "\n");

        sb.append("endian tag\t: ");
        sb.append(Long.toString(endian_tag) + "\n");

        sb.append("link size\t: ");
        sb.append(Long.toString(link_size) + "\n");

        sb.append("link offset\t: ");
        sb.append(Long.toString(link_off) + "\n");

        sb.append("map offset\t: ");
        sb.append(Long.toString(map_off) + "\n");

        sb.append("string ids size\t: ");
        sb.append(Long.toString(string_ids_size) + "\n");

        sb.append("string ids off\t: ");
        sb.append(Long.toString(string_ids_off) + "\n");

        sb.append("type ids size\t: ");
        sb.append(Long.toString(type_ids_size) + "\n");

        sb.append("type ids off\t: ");
        sb.append(Long.toString(type_ids_off) + "\n");

        sb.append("proto ids size\t: ");
        sb.append(Long.toString(proto_ids_size) + "\n");

        sb.append("proto ids off\t: ");
        sb.append(Long.toString(proto_ids_off) + "\n");

        sb.append("field ids size\t: ");
        sb.append(Long.toString(field_ids_size) + "\n");

        sb.append("field ids off\t: ");
        sb.append(Long.toString(field_ids_off) + "\n");

        sb.append("method ids size\t: ");
        sb.append(Long.toString(method_ids_size) + "\n");

        sb.append("method ids off\t: ");
        sb.append(Long.toString(method_ids_off) + "\n");

        sb.append("class defs size\t: ");
        sb.append(Long.toString(class_defs_size) + "\n");

        sb.append("class defs off\t: ");
        sb.append(Long.toString(class_defs_off) + "\n");

        sb.append("data size\t: ");
        sb.append(Long.toString(data_size) + "\n");

        sb.append("data offset\t: ");
        sb.append(Long.toString(data_off) + "\n");

        sb.append("############################\n");

        return sb.toString();
    }
}
