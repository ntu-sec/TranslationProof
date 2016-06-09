package anu.rsise.dexParser.reader;

import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

import anu.rsise.GeneralUtil;
import anu.rsise.dexParser.container.TypeContainer;

public class TypeReader {

    public static TypeContainer readHeader(InputStream isr, int size) throws IOException {
        TypeContainer tc = new TypeContainer(size);

        for (int i = 0; i < size; i++) {
            tc.items[i].descriptor_idx = GeneralUtil.readIntFromInputStream(isr);
            System.out.println(tc.items[i].descriptor_idx);
        }

        return tc;
    }

    public static TypeContainer readHeader(RandomAccessFile raf, int offset, int size) throws IOException {
        raf.seek(offset);
        TypeContainer tc = new TypeContainer(size);

        for (int i = 0; i < size; i++) {
            tc.items[i].descriptor_idx = GeneralUtil.readIntFromRandomAccessFile(raf);
        }

        return tc;
    }
}
