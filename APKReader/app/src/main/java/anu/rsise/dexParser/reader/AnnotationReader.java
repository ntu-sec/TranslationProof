package anu.rsise.dexParser.reader;

import java.io.IOException;
import java.io.RandomAccessFile;

import anu.rsise.GeneralUtil;
import anu.rsise.GeneralUtil.LEB_TYPE;
import anu.rsise.dexParser.container.DexContainer;
import anu.rsise.dexParser.encoding.EncodedAnnotation;

public class AnnotationReader {

    public static EncodedAnnotation readEncodedAnnotation(RandomAccessFile raf, DexContainer dc) throws IOException {
        EncodedAnnotation ea = new EncodedAnnotation();

        ea.type_idx = GeneralUtil.readLeb128(raf, LEB_TYPE.uleb128);

        ea.type = dc.tc.items[ea.type_idx];
        ea.setSize(GeneralUtil.readLeb128(raf, LEB_TYPE.uleb128));

        for (int j = 0; j < ea.size; j++) {
            ea.elements[j].name_idx = GeneralUtil.readLeb128(raf, LEB_TYPE.uleb128);
            ea.elements[j].name_str = dc.sc.items[ea.elements[j].name_idx].str_data;
            ea.elements[j].value = ValueReader.readEncodedValue(raf, dc);
        }
        return ea;
    }
}
