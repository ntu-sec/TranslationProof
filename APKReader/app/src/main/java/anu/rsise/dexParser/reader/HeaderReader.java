package anu.rsise.dexParser.reader;

import java.io.IOException;
import java.io.InputStream;

import anu.rsise.GeneralUtil;
import anu.rsise.dexParser.container.HeaderStructure;

public class HeaderReader {

    public static HeaderStructure readHeader(InputStream isr) throws IOException {

        HeaderStructure hs = new HeaderStructure();

        int i = 0;

        for (i = 0; i < 8; i++) {
            hs.magic[i] = (byte) isr.read();
        }

        hs.checksum = GeneralUtil.readIntFromInputStream(isr);

        for (i = 0; i < 20; i++) hs.signature[i] = (byte) isr.read();


        hs.file_size = GeneralUtil.readIntFromInputStream(isr);

        hs.header_size = GeneralUtil.readIntFromInputStream(isr);

        hs.endian_tag = GeneralUtil.readIntFromInputStream(isr);

        hs.link_size = GeneralUtil.readIntFromInputStream(isr);

        hs.link_off = GeneralUtil.readIntFromInputStream(isr);

        hs.map_off = GeneralUtil.readIntFromInputStream(isr);

        hs.string_ids_size = GeneralUtil.readIntFromInputStream(isr);
        hs.string_ids_off = GeneralUtil.readIntFromInputStream(isr);

        hs.type_ids_size = GeneralUtil.readIntFromInputStream(isr);
        hs.type_ids_off = GeneralUtil.readIntFromInputStream(isr);

        hs.proto_ids_size = GeneralUtil.readIntFromInputStream(isr);
        hs.proto_ids_off = GeneralUtil.readIntFromInputStream(isr);

        hs.field_ids_size = GeneralUtil.readIntFromInputStream(isr);
        hs.field_ids_off = GeneralUtil.readIntFromInputStream(isr);

        hs.method_ids_size = GeneralUtil.readIntFromInputStream(isr);
        hs.method_ids_off = GeneralUtil.readIntFromInputStream(isr);

        hs.class_defs_size = GeneralUtil.readIntFromInputStream(isr);
        hs.class_defs_off = GeneralUtil.readIntFromInputStream(isr);

        hs.data_size = GeneralUtil.readIntFromInputStream(isr);
        hs.data_off = GeneralUtil.readIntFromInputStream(isr);

        return hs;
    }
}
