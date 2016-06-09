package anu.rsise.dexParser.reader;

import java.io.IOException;
import java.io.RandomAccessFile;

import anu.rsise.GeneralUtil;
import anu.rsise.GeneralUtil.LEB_TYPE;
import anu.rsise.dexParser.container.DexContainer;
import anu.rsise.dexParser.encoding.EncodedArray;
import anu.rsise.dexParser.encoding.EncodedValue;

public class ValueReader {

    public static EncodedValue readEncodedValue(RandomAccessFile raf, DexContainer dc) throws IOException {
        EncodedValue ev = new EncodedValue();

        ev.setArgsType(raf.readByte());
        switch (ev.type) {
            case (EncodedValue.VALUE_BYTE): {
                ((EncodedValue.ByteValue) (ev.valueContainer)).value = raf.readByte();
                break;
            }
            case (EncodedValue.VALUE_SHORT): {
                ((EncodedValue.ShortValue) (ev.valueContainer)).value = (short) GeneralUtil.readBytes(raf, ev.size);
                break;
            }
            case (EncodedValue.VALUE_CHAR): {
                ((EncodedValue.CharValue) (ev.valueContainer)).value = (char) GeneralUtil.readBytes(raf, ev.size);
                break;
            }
            case (EncodedValue.VALUE_INT): {
                ((EncodedValue.IntValue) (ev.valueContainer)).value = (int) GeneralUtil.readBytes(raf, ev.size);
                break;
            }
            case (EncodedValue.VALUE_LONG): {
                ((EncodedValue.LongValue) (ev.valueContainer)).value = GeneralUtil.readBytes(raf, ev.size);
                break;
            }
            case (EncodedValue.VALUE_FLOAT): {
                ((EncodedValue.FloatValue) (ev.valueContainer)).value = Float.intBitsToFloat((int) (GeneralUtil.readBytes(raf, ev.size)));
                break;
            }
            case (EncodedValue.VALUE_DOUBLE): {
                ((EncodedValue.DoubleValue) (ev.valueContainer)).value = Double.longBitsToDouble(GeneralUtil.readBytes(raf, ev.size));
                break;
            }
            case (EncodedValue.VALUE_STRING): {
                EncodedValue.StringValue container = (EncodedValue.StringValue) (ev.valueContainer);
                container.string_idx = (int) GeneralUtil.readBytes(raf, ev.size);
                container.value = dc.sc.items[container.string_idx].str_data;
                break;
            }
            case (EncodedValue.VALUE_TYPE): {
                EncodedValue.TypeValue container = (EncodedValue.TypeValue) (ev.valueContainer);
                container.type_idx = (int) GeneralUtil.readBytes(raf, ev.size);
                container.value = dc.tc.items[container.type_idx];
                break;
            }
            case (EncodedValue.VALUE_FIELD): {
                EncodedValue.FieldValue container = (EncodedValue.FieldValue) (ev.valueContainer);
                container.field_idx = (int) GeneralUtil.readBytes(raf, ev.size);
                container.value = dc.fc.items[container.field_idx];
                break;
            }
            case (EncodedValue.VALUE_METHOD): {
                EncodedValue.MethodValue container = (EncodedValue.MethodValue) (ev.valueContainer);
                container.method_idx = (int) GeneralUtil.readBytes(raf, ev.size);
                container.value = dc.mc.items[container.method_idx];
                break;
            }
            case (EncodedValue.VALUE_ENUM): {
                EncodedValue.EnumValue container = (EncodedValue.EnumValue) (ev.valueContainer);
                container.field_idx = (int) GeneralUtil.readBytes(raf, ev.size);
                container.value = dc.fc.items[container.field_idx];
                break;
            }
            case (EncodedValue.VALUE_ARRAY): {
                ev.valueContainer = readEncodedArray(raf, dc);
                break;
            }
            case (EncodedValue.VALUE_ANNOTATION): {
                ev.valueContainer = AnnotationReader.readEncodedAnnotation(raf, dc);
                break;
            }
        }

        return ev;
    }

    public static EncodedArray readEncodedArray(RandomAccessFile raf, DexContainer dc) throws IOException {
        EncodedArray ea = new EncodedArray();
        ea.setSize(GeneralUtil.readLeb128(raf, LEB_TYPE.uleb128));

        for (int j = 0; j < ea.size; j++) {
            ea.values[j] = readEncodedValue(raf, dc);
        }
        return ea;
    }
}
