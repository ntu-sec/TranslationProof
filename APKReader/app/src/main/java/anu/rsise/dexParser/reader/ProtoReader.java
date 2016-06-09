package anu.rsise.dexParser.reader;

import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

import anu.rsise.GeneralUtil;
import anu.rsise.dexParser.container.ProtoContainer;
import anu.rsise.dexParser.container.TypeContainer;
import anu.rsise.dexParser.container.TypeList;

public class ProtoReader {

    public static ProtoContainer readHeader(InputStream isr, int size) throws IOException {
        ProtoContainer pc = new ProtoContainer(size);

        for (int i = 0; i < size; i++) {
            pc.items[i].shorty_idx = GeneralUtil.readIntFromInputStream(isr);
            pc.items[i].return_type_idx = GeneralUtil.readIntFromInputStream(isr);
            pc.items[i].parameters_off = GeneralUtil.readIntFromInputStream(isr);
        }

        return pc;
    }

    public static ProtoContainer readHeader(RandomAccessFile raf, int offset, int size) throws IOException {
        raf.seek(offset);
        ProtoContainer pc = new ProtoContainer(size);

        for (int i = 0; i < size; i++) {
            pc.items[i].shorty_idx = GeneralUtil.readIntFromRandomAccessFile(raf);
            pc.items[i].return_type_idx = GeneralUtil.readIntFromRandomAccessFile(raf);
            pc.items[i].parameters_off = GeneralUtil.readIntFromRandomAccessFile(raf);
        }

        return pc;
    }

    public static void readData(RandomAccessFile raf, ProtoContainer pc, TypeContainer tc) throws IOException {
        for (int i = 0; i < pc.size; i++) {
            if (pc.items[i].parameters_off == 0) {
                pc.items[i].parameters = new TypeList(0);
                continue;
            }

            raf.seek(pc.items[i].parameters_off);

            pc.items[i].parameters = new TypeList(GeneralUtil.readIntFromRandomAccessFile(raf));

            for (int j = 0; j < pc.items[i].parameters.size; j++) {
                pc.items[i].parameters.list[j].type_idx = GeneralUtil.readShortFromRandomAccessFile(raf);
                pc.items[i].parameters.list[j].type_str = tc.items[pc.items[i].parameters.list[j].type_idx].str_type;
            }

        }
    }
}
