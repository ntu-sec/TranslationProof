package anu.rsise.dexParser.reader;

import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

import anu.rsise.GeneralUtil;
import anu.rsise.dexParser.container.StringContainer;

public class StringReader {

    public static StringContainer readHeader(InputStream isr, int size) throws IOException {
        StringContainer sc = new StringContainer(size);

        for (int i = 0; i < size; i++) {
            sc.items[i].string_data_off = GeneralUtil.readIntFromInputStream(isr);
        }

        return sc;
    }

    public static StringContainer readHeader(RandomAccessFile raf, int offset, int size) throws IOException {
        raf.seek(offset);
        StringContainer sc = new StringContainer(size);

        for (int i = 0; i < size; i++) {
            sc.items[i].string_data_off = GeneralUtil.readIntFromRandomAccessFile(raf);
        }

        return sc;
    }

    public static void readData(RandomAccessFile raf, StringContainer sc) throws IOException {
        for (int i = 0; i < sc.size; i++) {
            raf.seek(sc.items[i].string_data_off);

            sc.items[i].utf16_size = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);

            StringBuilder sb = new StringBuilder();

            for (int j = 0; j < sc.items[i].utf16_size; j++) {
                sb.append(Character.toString((char) raf.read()));
            }
            sc.items[i].str_data = sb.toString();
        }
    }
}
