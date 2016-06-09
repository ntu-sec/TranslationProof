package anu.rsise.dexParser.reader;

import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

import anu.rsise.GeneralUtil;
import anu.rsise.dexParser.container.FieldContainer;

public class FieldReader {

    public static FieldContainer readHeader(InputStream isr, int size) throws IOException {
        FieldContainer fc = new FieldContainer(size);

        for (int i = 0; i < size; i++) {
            fc.items[i].class_idx = GeneralUtil.readShortFromInputStream(isr, GeneralUtil.Direction.Right_to_left);
            fc.items[i].type_idx = GeneralUtil.readShortFromInputStream(isr, GeneralUtil.Direction.Right_to_left);
            fc.items[i].name_idx = GeneralUtil.readIntFromInputStream(isr);
        }

        return fc;
    }

    public static FieldContainer readHeader(RandomAccessFile raf, int offset, int size) throws IOException {
        raf.seek(offset);
        FieldContainer fc = new FieldContainer(size);

        for (int i = 0; i < size; i++) {
            fc.items[i].class_idx = GeneralUtil.readShortFromRandomAccessFile(raf);
            fc.items[i].type_idx = GeneralUtil.readShortFromRandomAccessFile(raf);
            fc.items[i].name_idx = GeneralUtil.readIntFromRandomAccessFile(raf);
        }

        return fc;
    }
}
