package anu.rsise.dexParser.reader;

import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

import anu.rsise.GeneralUtil;
import anu.rsise.dexParser.container.MethodContainer;

public class MethodReader {

    public static MethodContainer readHeader(InputStream isr, int size) throws IOException {
        MethodContainer mc = new MethodContainer(size);

        for (int i = 0; i < size; i++) {
            mc.items[i].class_idx = GeneralUtil.readShortFromInputStream(isr, GeneralUtil.Direction.Right_to_left);
            mc.items[i].proto_idx = GeneralUtil.readShortFromInputStream(isr, GeneralUtil.Direction.Right_to_left);
            mc.items[i].name_idx = GeneralUtil.readIntFromInputStream(isr);
        }

        return mc;
    }

    public static MethodContainer readHeader(RandomAccessFile raf, int offset, int size) throws IOException {
        raf.seek(offset);
        MethodContainer mc = new MethodContainer(size);

        for (int i = 0; i < size; i++) {
            mc.items[i].class_idx = GeneralUtil.readShortFromRandomAccessFile(raf);
            mc.items[i].proto_idx = GeneralUtil.readShortFromRandomAccessFile(raf);
            mc.items[i].name_idx = GeneralUtil.readIntFromRandomAccessFile(raf);
        }

        return mc;
    }
}
