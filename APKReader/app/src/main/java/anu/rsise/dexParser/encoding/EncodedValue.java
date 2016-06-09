package anu.rsise.dexParser.encoding;

import anu.rsise.dexParser.container.FieldContainer;
import anu.rsise.dexParser.container.MethodContainer;
import anu.rsise.dexParser.container.TypeContainer;

public class EncodedValue {

    public static final int VALUE_BYTE = 0x00;
    public static final int VALUE_SHORT = 0x02;
    public static final int VALUE_CHAR = 0x03;
    public static final int VALUE_INT = 0x04;
    public static final int VALUE_LONG = 0x06;
    public static final int VALUE_FLOAT = 0x10;
    public static final int VALUE_DOUBLE = 0x11;
    public static final int VALUE_STRING = 0x17;
    public static final int VALUE_TYPE = 0x18;
    public static final int VALUE_FIELD = 0x19;
    public static final int VALUE_METHOD = 0x1a;
    public static final int VALUE_ENUM = 0x1b;
    public static final int VALUE_ARRAY = 0x1c;
    public static final int VALUE_ANNOTATION = 0x1d;
    public static final int VALUE_NULL = 0x1e;
    public static final int VALUE_BOOLEAN = 0x1f;
    public int type;
    public byte size;
    public Object valueContainer;

    public String toString() {
        return valueContainer.toString();
    }

    public void setArgsType(byte value_type) {
        type = value_type & 0x1f;
        switch (type) {
            case (VALUE_BYTE): {
                size = 0;
                valueContainer = new ByteValue();
                return;
            }
            case (VALUE_SHORT): {
                size = (byte) (((value_type & 0xE0) >> 5) + 1);
                valueContainer = new ShortValue();
                return;
            }
            case (VALUE_CHAR): {
                size = (byte) (((value_type & 0xE0) >> 5) + 1);
                valueContainer = new CharValue();
                return;
            }
            case (VALUE_INT): {
                size = (byte) (((value_type & 0xE0) >> 5) + 1);
                valueContainer = new IntValue();
                return;
            }
            case (VALUE_LONG): {
                size = (byte) (((value_type & 0xE0) >> 5) + 1);
                valueContainer = new LongValue();
                return;
            }
            case (VALUE_FLOAT): {
                size = (byte) (((value_type & 0xE0) >> 5) + 1);
                valueContainer = new FloatValue();
                return;
            }
            case (VALUE_DOUBLE): {
                size = (byte) (((value_type & 0xE0) >> 5) + 1);
                valueContainer = new DoubleValue();
                return;
            }
            case (VALUE_STRING): {
                size = (byte) (((value_type & 0xE0) >> 5) + 1);
                valueContainer = new StringValue();
                return;
            }
            case (VALUE_TYPE): {
                size = (byte) (((value_type & 0xE0) >> 5) + 1);
                valueContainer = new TypeValue();
                return;
            }
            case (VALUE_FIELD): {
                size = (byte) (((value_type & 0xE0) >> 5) + 1);
                valueContainer = new FieldValue();
                return;
            }
            case (VALUE_METHOD): {
                size = (byte) (((value_type & 0xE0) >> 5) + 1);
                valueContainer = new MethodValue();
                return;
            }
            case (VALUE_ENUM): {
                size = (byte) (((value_type & 0xE0) >> 5) + 1);
                valueContainer = new EnumValue();
                return;
            }
            case (VALUE_ARRAY): {
                size = 0;
                valueContainer = new EncodedArray();
                return;
            }
            case (VALUE_ANNOTATION): {
                size = 0;
                valueContainer = new EncodedAnnotation();
                return;
            }
            case (VALUE_NULL): {
                size = 0;
                valueContainer = new NullValue();
                return;
            }
            case (VALUE_BOOLEAN): {
                valueContainer = new BooleanValue((value_type & 0xE0) >> 5);
                return;
            }
        }
    }

    public static class ByteValue {
        public byte value;

        public String toString() {
            return Byte.toString(value);
        }
    }

    public static class ShortValue {
        public short value;

        public String toString() {
            return Short.toString(value);
        }
    }

    public static class CharValue {
        public char value;

        public String toString() {
            return Character.toString(value);
        }
    }

    public static class IntValue {
        public int value;

        public String toString() {
            return Integer.toString(value);
        }
    }

    public static class LongValue {
        public long value;

        public String toString() {
            return Long.toString(value);
        }
    }

    public static class FloatValue {
        public float value;

        public String toString() {
            return Float.toString(value);
        }
    }

    public static class DoubleValue {
        public double value;

        public String toString() {
            return Double.toString(value);
        }
    }

    public static class StringValue {
        public int string_idx;
        public String value;

        public String toString() {
            return value;
        }
    }

    public static class TypeValue {
        public int type_idx;
        public TypeContainer.TypeItem value;

        public String toString() {
            return value.toString();
        }
    }

    public static class FieldValue {
        public int field_idx;
        public FieldContainer.FieldItem value;

        public String toString() {
            return value.toString();
        }
    }

    public static class MethodValue {
        public int method_idx;
        public MethodContainer.MethodItem value;

        public String toString() {
            return value.toString();
        }
    }

    public static class EnumValue {
        public int field_idx;
        public FieldContainer.FieldItem value;

        public String toString() {
            return value.toString();
        }
    }

    public static class NullValue {
        public String toString() {
            return "NULL";
        }
    }

    public static class BooleanValue {
        public boolean value;

        public BooleanValue(int v) {
            value = ((v & 0x01) == 1) ? true : false;
        }

        public String toString() {
            return Boolean.toString(value);
        }
    }
}
