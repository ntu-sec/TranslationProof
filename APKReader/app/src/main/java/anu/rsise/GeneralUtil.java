package anu.rsise;

import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

import anu.rsise.certParser.ExtendedLevel;
import anu.rsise.certParser.LevelPool;

public class GeneralUtil {

    public static final int NO_INDEX = 0xFFFFFFFF; // -1

    ;

    public static String intToBinary(int value) {
        int local = value;
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 32; i++) {
            sb.append(((local & (1 << (31 - i))) != 0) ? 1 : 0);
        }
        return sb.toString();
    }

    /* DEX cert style */
    public static String readStringFromInputStream(InputStream isr) throws IOException {
        int length = isr.read();
        char[] tempStr = new char[length];
        for (int i = 0; i < length; i++) {
            tempStr[i] = (char) isr.read();
        }
        return String.valueOf(tempStr);
    }

    public static int readIntFromInputStream(InputStream isr) throws IOException {
        int data;
        data = (isr.read() & 0x0FF) << 0;
        data |= (isr.read() & 0x0FF) << 8;
        data |= (isr.read() & 0x0FF) << 16;
        data |= (isr.read() & 0x0FF) << 24;
        return data;
    }

    public static ExtendedLevel readExtLevel(InputStream isr, LevelPool lvl_pool) throws IOException {
        int len = isr.read();
        if (len > 1) {
            byte[] b = new byte[len];
            isr.read(b);
            return ExtendedLevel.createArray(b, lvl_pool);
        }
      /* the case where the length is 1 */
        else if (len == 1) return ExtendedLevel.createSimple((byte) isr.read(), lvl_pool);
	  /* the case where the length is 0, this is only useful for the result pseudo-register */
        return null;
    }

    public static short readShortFromInputStream(InputStream isr, Direction dir) throws IOException {
        short data = 0;
        switch (dir) {
            case Right_to_left:
                data = (short) ((isr.read() & 0x0FF) << 0);
                data |= (short) ((isr.read() & 0x0FF) << 8);
            case Left_to_right:
                data = (short) ((isr.read() & 0x0FF) << 8);
                data |= (short) ((isr.read() & 0x0FF) << 0);
        }
        return data;
    }

    public static int readIntFromRandomAccessFile(RandomAccessFile raf) throws IOException {
        int data;
        data = (raf.read() & 0x0FF) << 0;
        data |= (raf.read() & 0x0FF) << 8;
        data |= (raf.read() & 0x0FF) << 16;
        data |= (raf.read() & 0x0FF) << 24;
        return data;
    }

    public static short readShortFromRandomAccessFile(RandomAccessFile raf) throws IOException {
        short data;
        data = (short) ((raf.read() & 0x0FF) << 0);
        data |= (short) ((raf.read() & 0x0FF) << 8);
        return data;
    }

    public static String longToHex(long value) {
        StringBuilder sb = new StringBuilder("0x");
        long x = (long) (0x0F000000 << 32);
        sb.append(valueToHex((int) ((value & (long) (0xF0000000 << 32)) >> 60)));
        for (int i = 14; i >= 0; i--, x >>= 4) sb.append(valueToHex((int) (value & x) >> i * 4));
        return sb.toString();
    }

    public static String intToHex(int value) {
        StringBuilder sb = new StringBuilder("0x");
        int x = 0x0F000000;
        sb.append(valueToHex((value & 0xF0000000) >> 28));
        for (int i = 6; i >= 0; i--, x >>= 4) sb.append(valueToHex((value & x) >> i * 4));
        return sb.toString();
    }

    public static String shortToHex(short value) {
        StringBuilder sb = new StringBuilder("0x");
        short x = (short) 0x0F00;
        sb.append(valueToHex((value & 0xF000) >> 12));
        for (int i = 2; i >= 0; i--, x >>= 4) sb.append(valueToHex((value & x) >> i * 4));
        return sb.toString();
    }

    public static String byteToHex(byte value) {
        StringBuilder sb = new StringBuilder("0x");
        sb.append(valueToHex((value & 0xF0) >> 4));
        sb.append(valueToHex((value & 0x0F) >> 0));
        return sb.toString();
    }

    public static char valueToHex(int value) {
        if (value >= 0 && value <= 9) return (char) (value + '0');
        if (value >= 10 && value <= 15) return (char) ((value - 10) + 'A');
        return '$';
    }

    public static int readLeb128(RandomAccessFile raf, int type) throws IOException {
        byte values[] = new byte[5];
        int it = 1;
        while (((values[it - 1] = (byte) (raf.read())) & 0x80) != 0) it++;

        int retVal = 0;
        if (type == LEB_TYPE.sleb128) retVal = sleb128Conversion(values, it);
        else if (type == LEB_TYPE.uleb128) retVal = uleb128Conversion(values, it);
        else retVal = uleb128Conversion(values, it) - 1;

        return retVal;
    }

    public static int uleb128Conversion(byte[] value, int size) {
        int ret = 0;

        for (int i = size - 1; i >= 0; i--) {
            ret = (ret << 7) | (value[i] & 0x7F);
        }

        return ret;
    }

    public static int sleb128Conversion(byte[] value, int size) {
        int ret = 0;

        for (int i = size - 1; i >= 0; i--) {
            ret = (ret << 7) | (value[i] & 0x7F);
        }

        if ((value[0] & 0x40) > 0) // negative number
        {
            ret |= 0xFFFFFFFF << (size * 7);
        }

        return ret;
    }

    public static int lebSize(int lebNum) {
        if ((lebNum & 0xF0000000) != 0) return 5; // bit 29 - 32
        else if ((lebNum & 0x0FE00000) != 0) return 4; // bit 22 - 28
        else if ((lebNum & 0x001FC000) != 0) return 3; // bit 15 - 21
        else if ((lebNum & 0x00003F80) != 0) return 2; // bit 8 - 14
        return 1; // bit 1 - 7
    }

    public static long readBytes(RandomAccessFile raf, int size) throws IOException {
        long retVal = 0;
        for (int i = 0, shifter = 0; i < size; i++, shifter += 8) {
            retVal |= (raf.read() & 0x0FF) << shifter;
        }
        return retVal;
    }

    public enum Direction {
        Left_to_right,
        Right_to_left
    }

    /*
     * type : 0 is sleb128, 1 is uleb128, and 2 is uleb128p1
     */
    public static class LEB_TYPE {
        public static final int sleb128 = 0;
        public static final int uleb128 = 1;
        public static final int uleb128p1 = 2;
    }
}
