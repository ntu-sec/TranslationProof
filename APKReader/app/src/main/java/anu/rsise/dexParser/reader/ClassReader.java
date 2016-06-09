package anu.rsise.dexParser.reader;

import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

import anu.rsise.GeneralUtil;
import anu.rsise.GeneralUtil.LEB_TYPE;
import anu.rsise.dexParser.container.AnnotationsDirectory;
import anu.rsise.dexParser.container.ClassContainer;
import anu.rsise.dexParser.container.DexContainer;
import anu.rsise.dexParser.container.FieldContainer;
import anu.rsise.dexParser.container.MethodContainer;
import anu.rsise.dexParser.container.TypeContainer;
import anu.rsise.dexParser.container.TypeList;
import anu.rsise.dexParser.encoding.CodeItem;
import anu.rsise.dexParser.encoding.EncodedCatchHandlerList;
import anu.rsise.dexParser.encoding.EncodedField;
import anu.rsise.dexParser.encoding.EncodedMethod;

public class ClassReader {

    public static ClassContainer readHeader(InputStream isr, int size) throws IOException {
        ClassContainer cc = new ClassContainer(size);

        for (int i = 0; i < size; i++) {
            cc.items[i].class_idx = GeneralUtil.readIntFromInputStream(isr);
            cc.items[i].access_flag = GeneralUtil.readIntFromInputStream(isr);
            cc.items[i].superclass_idx = GeneralUtil.readIntFromInputStream(isr);
            cc.items[i].interface_off = GeneralUtil.readIntFromInputStream(isr);
            cc.items[i].source_file_idx = GeneralUtil.readIntFromInputStream(isr);
            cc.items[i].annotations_off = GeneralUtil.readIntFromInputStream(isr);
            cc.items[i].class_data_off = GeneralUtil.readIntFromInputStream(isr);
            cc.items[i].static_values_off = GeneralUtil.readIntFromInputStream(isr);
        }

        return cc;
    }

    public static ClassContainer readHeader(RandomAccessFile raf, int offset, int size) throws IOException {
        raf.seek(offset);
        ClassContainer cc = new ClassContainer(size);

        for (int i = 0; i < size; i++) {
            cc.items[i].class_idx = GeneralUtil.readIntFromRandomAccessFile(raf);
            cc.items[i].access_flag = GeneralUtil.readIntFromRandomAccessFile(raf);
            cc.items[i].superclass_idx = GeneralUtil.readIntFromRandomAccessFile(raf);
            cc.items[i].interface_off = GeneralUtil.readIntFromRandomAccessFile(raf);
            cc.items[i].source_file_idx = GeneralUtil.readIntFromRandomAccessFile(raf);
            cc.items[i].annotations_off = GeneralUtil.readIntFromRandomAccessFile(raf);
            cc.items[i].class_data_off = GeneralUtil.readIntFromRandomAccessFile(raf);
            cc.items[i].static_values_off = GeneralUtil.readIntFromRandomAccessFile(raf);
        }

        return cc;
    }

    public static void readInterfaces(RandomAccessFile raf, ClassContainer cc, TypeContainer tc) throws IOException {
        for (int i = 0; i < cc.size; i++) {
            if (cc.items[i].interface_off == 0) {
                cc.items[i].interfaces = new TypeList(0);
                continue;
            }

            raf.seek(cc.items[i].interface_off);

            cc.items[i].interfaces = new TypeList(GeneralUtil.readIntFromRandomAccessFile(raf));

            for (int j = 0; j < cc.items[i].interfaces.size; j++) {
                cc.items[i].interfaces.list[j].type_idx = GeneralUtil.readShortFromRandomAccessFile(raf);
                cc.items[i].interfaces.list[j].type_str = tc.items[cc.items[i].interfaces.list[j].type_idx].str_type;
            }

        }
    }

    private static void readEncodedField(RandomAccessFile raf, FieldContainer fc, EncodedField[] ef, int size) throws IOException {
        if (size <= 0) return;

        int last;

        ef[0].field_idx_diff = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
        last = ef[0].field_idx_diff;
        ef[0].field = fc.items[last];
        ef[0].access_flags = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
        ef[0].access_flags_str = ClassContainer.ACCESS_FLAG.resolveFields(ef[0].access_flags);

        for (int j = 1; j < size; j++) {
            ef[j].field_idx_diff = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
            last += ef[j].field_idx_diff;
            ef[j].field = fc.items[last];
            ef[j].access_flags = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
            ef[j].access_flags_str = ClassContainer.ACCESS_FLAG.resolveFields(ef[j].access_flags);
        }
    }

    public static CodeItem readCodeItem(RandomAccessFile raf, int offset, DexContainer dc) throws IOException {
        CodeItem ci = new CodeItem();

        if (offset == 0) {
            ci.register_size = 0;
            ci.ins_size = 0;
            ci.outs_size = 0;
            ci.tries_size = 0;
            ci.debug_info_off = 0;
            ci.insns_size = 0;
        }
        raf.seek(offset);

        ci.register_size = GeneralUtil.readShortFromRandomAccessFile(raf);
        ci.ins_size = GeneralUtil.readShortFromRandomAccessFile(raf);
        ci.outs_size = GeneralUtil.readShortFromRandomAccessFile(raf);
        ci.tries_size = GeneralUtil.readShortFromRandomAccessFile(raf);
        ci.debug_info_off = GeneralUtil.readIntFromRandomAccessFile(raf);
        ci.insns_size = GeneralUtil.readIntFromRandomAccessFile(raf);
        ci.insns_raw = new short[ci.insns_size];
        for (int i = 0; i < ci.insns_size; i++) {
            ci.insns_raw[i] = GeneralUtil.readShortFromRandomAccessFile(raf);
        }

        if (ci.tries_size > 0) {
            if ((ci.insns_size % 2) != 0) {
                ci.padding = GeneralUtil.readShortFromRandomAccessFile(raf);
            }

            ci.tries = new CodeItem.TryItem[ci.tries_size];
            for (int i = 0; i < ci.tries_size; i++) {
                ci.tries[i] = new CodeItem.TryItem();
                ci.tries[i].start_addr = GeneralUtil.readIntFromRandomAccessFile(raf);
                ci.tries[i].insn_count = GeneralUtil.readShortFromRandomAccessFile(raf);
                ci.tries[i].handler_off = GeneralUtil.readShortFromRandomAccessFile(raf);
            }

            ci.handlers = new EncodedCatchHandlerList();
            int temp = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
            ci.handlers.setSize(temp);
            int lastAddress = GeneralUtil.lebSize(ci.handlers.size);
            for (int i = 0; i < ci.handlers.size; i++) {
                ci.handlers.list[i].setSize(GeneralUtil.readLeb128(raf, LEB_TYPE.sleb128));
                ci.handlers.list[i].offset_from_start_list = lastAddress;
                lastAddress += GeneralUtil.lebSize(ci.handlers.list[i].size);
                for (int j = 0; j < ci.handlers.list[i].size; j++) {
                    ci.handlers.list[i].handlers[j].type_idx = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
                    ci.handlers.list[i].handlers[j].addr = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
                    ci.handlers.list[i].handlers[j].type = dc.tc.items[ci.handlers.list[i].handlers[j].type_idx];
                    lastAddress += GeneralUtil.lebSize(ci.handlers.list[i].handlers[j].type_idx);
                    lastAddress += GeneralUtil.lebSize(ci.handlers.list[i].handlers[j].addr);
                }
                if (ci.handlers.list[i].catch_all_exists) {
                    ci.handlers.list[i].catch_all_addr = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
                    lastAddress += GeneralUtil.lebSize(ci.handlers.list[i].catch_all_addr);
                }
            }

            for (int i = 0; i < ci.tries_size; i++) {
                for (int j = 0; j < ci.handlers.size; j++) {
                    if (ci.tries[i].handler_off == ci.handlers.list[j].offset_from_start_list) {
                        ci.tries[i].handler = ci.handlers.list[j];
                        break;
                    }
                }
            }
        }

        // reading debug info
        raf.seek(ci.debug_info_off);
        ci.debug_info = DebugInfoReader.readDebugInfo(raf, dc);

        return ci;
    }

    private static void readEncodedMethod(RandomAccessFile raf, MethodContainer mc, EncodedMethod[] em, int size) throws IOException {
        if (size <= 0) return;

        int last;

        em[0].method_idx_diff = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
        last = em[0].method_idx_diff;
        em[0].method = mc.items[last];
        em[0].access_flags = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
        em[0].access_flags_str = ClassContainer.ACCESS_FLAG.resolveMethods(em[0].access_flags);
        em[0].code_off = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);

        for (int j = 1; j < size; j++) {
            em[j].method_idx_diff = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
            last += em[j].method_idx_diff;
            em[j].method = mc.items[last];
            em[j].access_flags = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
            em[j].access_flags_str = ClassContainer.ACCESS_FLAG.resolveMethods(em[j].access_flags);
            em[j].code_off = GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128);
        }
    }

    public static void readClassData(RandomAccessFile raf, DexContainer dc) throws IOException {
        for (int i = 0; i < dc.cc.size; i++) {
            dc.cc.items[i].class_data = new ClassContainer.ClassItem.ClassDataItem();
            ClassContainer.ClassItem.ClassDataItem cdi = dc.cc.items[i].class_data;
            if (dc.cc.items[i].class_data_off == 0) {
                cdi.static_fields_size = 0;
                cdi.instance_fields_size = 0;
                cdi.direct_methods_size = 0;
                cdi.virtual_methods_size = 0;
                continue;
            }

            raf.seek(dc.cc.items[i].class_data_off);

            cdi.setStaticField(GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128));
            cdi.setInstanceField(GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128));
            cdi.setDirectMethod(GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128));
            cdi.setVirtualMethod(GeneralUtil.readLeb128(raf, GeneralUtil.LEB_TYPE.uleb128));

            readEncodedField(raf, dc.fc, cdi.static_fields, cdi.static_fields_size);
            readEncodedField(raf, dc.fc, cdi.instance_fields, cdi.instance_fields_size);

            readEncodedMethod(raf, dc.mc, cdi.direct_methods, cdi.direct_methods_size);
            readEncodedMethod(raf, dc.mc, cdi.virtual_methods, cdi.virtual_methods_size);

            for (int j = 0; j < cdi.direct_methods_size; j++) {
                if (cdi.direct_methods[j].code_off == 0) continue;
                cdi.direct_methods[j].code = readCodeItem(raf, cdi.direct_methods[j].code_off, dc);
                /* at this moment we have no use of debug information */
                cdi.direct_methods[j].code.interpretCode(dc);
            }

            for (int j = 0; j < cdi.virtual_methods_size; j++) {
                if (cdi.virtual_methods[j].code_off == 0) continue;
                cdi.virtual_methods[j].code = readCodeItem(raf, cdi.virtual_methods[j].code_off, dc);
				/* at this moment we have no use of debug information */
                cdi.virtual_methods[j].code.interpretCode(dc);
            }
        }
    }

    public static void readStaticValues(RandomAccessFile raf, ClassContainer cc, DexContainer dict) throws IOException {
        for (int i = 0; i < cc.size; i++) {
            if (cc.items[i].static_values_off == 0) continue;
            raf.seek(cc.items[i].static_values_off);
            cc.items[i].static_values = ValueReader.readEncodedArray(raf, dict);
        }
    }

    public static void readAnnotations(RandomAccessFile raf, ClassContainer cc, DexContainer dict) throws IOException {
        for (int i = 0; i < cc.size; i++) {
            // denote no annotations
            if (cc.items[i].annotations_off == 0) continue;

            raf.seek(cc.items[i].annotations_off);

            cc.items[i].annotationDirectory = new AnnotationsDirectory();
            AnnotationsDirectory ad = cc.items[i].annotationDirectory;

            ad.class_annotations_off = GeneralUtil.readIntFromRandomAccessFile(raf);
            ad.setFieldSize(GeneralUtil.readIntFromRandomAccessFile(raf));
            ad.setMethodSize(GeneralUtil.readIntFromRandomAccessFile(raf));
            ad.setParameterSize(GeneralUtil.readIntFromRandomAccessFile(raf));

            // read fields
            for (int j = 0; j < ad.field_size; j++) {
                ad.field_annotations[j].field_idx = GeneralUtil.readIntFromRandomAccessFile(raf);
                ad.field_annotations[j].field = dict.fc.items[ad.field_annotations[j].field_idx];
                ad.field_annotations[j].annotation_off = GeneralUtil.readIntFromRandomAccessFile(raf);
            }

            // read methods
            for (int j = 0; j < ad.method_size; j++) {
                ad.method_annotations[j].method_idx = GeneralUtil.readIntFromRandomAccessFile(raf);
                ad.method_annotations[j].method = dict.mc.items[ad.method_annotations[j].method_idx];
                ad.method_annotations[j].annotation_off = GeneralUtil.readIntFromRandomAccessFile(raf);
            }

            // read parameters
            for (int j = 0; j < ad.parameter_size; j++) {
                ad.parameter_annotations[j].method_idx = GeneralUtil.readIntFromRandomAccessFile(raf);
                ad.parameter_annotations[j].method = dict.mc.items[ad.parameter_annotations[j].method_idx];
                ad.parameter_annotations[j].annotation_off = GeneralUtil.readIntFromRandomAccessFile(raf);
            }

            // read class annotation
            if (ad.class_annotations_off != 0) {
                raf.seek(ad.class_annotations_off);
                cc.items[i].setClassAnnotation(readAnnotationSetItem(raf, dict));
            }
            // read field's actual annotations
            for (int j = 0; j < ad.field_size; j++) {
                raf.seek(ad.field_annotations[j].annotation_off);
                ad.field_annotations[j].annotationSetItem = readAnnotationSetItem(raf, dict);
                ad.field_annotations[j].field.setAnnotation(ad.field_annotations[j].annotationSetItem);
            }
            // read method's actual annotations
            for (int j = 0; j < ad.method_size; j++) {
                raf.seek(ad.method_annotations[j].annotation_off);
                ad.method_annotations[j].annotationSetItem = readAnnotationSetItem(raf, dict);
                ad.method_annotations[j].method.setMethodAnnotation(ad.method_annotations[j].annotationSetItem);
            }
            // read parameter's actual annotations
            for (int j = 0; j < ad.parameter_size; j++) {
                AnnotationsDirectory.ParameterAnnotation pa = ad.parameter_annotations[j];

                raf.seek(pa.annotation_off);

                pa.annotationSetRefList = new AnnotationsDirectory.AnnotationSetRefList();
                pa.annotationSetRefList.setSize(GeneralUtil.readIntFromRandomAccessFile(raf));

                for (int k = 0; k < pa.annotationSetRefList.size; k++) {
                    pa.annotationSetRefList.list[k].annotation_off = GeneralUtil.readIntFromRandomAccessFile(raf);
                }

                for (int k = 0; k < pa.annotationSetRefList.size; k++) {
                    raf.seek(pa.annotationSetRefList.list[k].annotation_off);
                    pa.annotationSetRefList.list[k].annotationSetItem = readAnnotationSetItem(raf, dict);
                }

                pa.method.setParameterAnnotation(pa.annotationSetRefList);
            }
        }
    }

    public static AnnotationsDirectory.AnnotationSetItem readAnnotationSetItem(RandomAccessFile raf, DexContainer dict) throws IOException {
        AnnotationsDirectory.AnnotationSetItem asi = new AnnotationsDirectory.AnnotationSetItem();

        asi.setSize(GeneralUtil.readIntFromRandomAccessFile(raf));

        for (int i = 0; i < asi.size; i++) {
            asi.entries[i].annotation_off = GeneralUtil.readIntFromRandomAccessFile(raf);
        }

        for (int i = 0; i < asi.size; i++) {
            raf.seek(asi.entries[i].annotation_off);
            asi.entries[i].visibility = raf.readByte();
            asi.entries[i].annotation = AnnotationReader.readEncodedAnnotation(raf, dict);
        }

        return asi;
    }
}
