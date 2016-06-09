package anu.rsise.dexParser.reader;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.RandomAccessFile;

import anu.rsise.dexParser.DecompilerArguments;
import anu.rsise.dexParser.container.DexContainer;
import anu.rsise.dexParser.container.HeaderStructure;

public class DexFileReader {
    public static DexContainer execute(File file, PrintStream ps, boolean closePrintStream) {
        DexContainer dc = new DexContainer();

        try {
            FileInputStream isr = new FileInputStream(file);

            // Header
            dc.hs = HeaderReader.readHeader(isr);

            isr.close();
        } catch (FileNotFoundException fnfe) {
            System.out.println(fnfe);
        } catch (IOException ioe) {
            System.out.println(ioe);
        }

        try {
            RandomAccessFile raf = new RandomAccessFile(file, "r");

            HeaderStructure hs = dc.hs;

            // String_Ids
            dc.sc = StringReader.readHeader(raf, hs.string_ids_off, hs.string_ids_size);

            // Type_Ids
            dc.tc = TypeReader.readHeader(raf, hs.type_ids_off, hs.type_ids_size);

            // Proto_Ids
            dc.pc = ProtoReader.readHeader(raf, hs.proto_ids_off, hs.proto_ids_size);

            // Field_Ids
            dc.fc = FieldReader.readHeader(raf, hs.field_ids_off, hs.field_ids_size);

            // Method_Ids
            dc.mc = MethodReader.readHeader(raf, hs.method_ids_off, hs.method_ids_size);

            // Class_Defs
            dc.cc = ClassReader.readHeader(raf, hs.class_defs_off, hs.class_defs_size);

            // Data

            // Link_Data

            // Read String Data
            StringReader.readData(raf, dc.sc);

            // Match Type with String
            dc.ResolveType();

            // Prototype
            dc.ResolveProto();
            ProtoReader.readData(raf, dc.pc, dc.tc);

            // Field
            dc.ResolveField();

            // Methods
            dc.ResolveMethod();

            // Class
            dc.ResolveClass();
            ClassReader.readClassData(raf, dc);
            ClassReader.readInterfaces(raf, dc.cc, dc.tc);
            ClassReader.readStaticValues(raf, dc.cc, dc);
            ClassReader.readAnnotations(raf, dc.cc, dc);

            raf.close();
        } catch (FileNotFoundException fnfe) {
            System.out.println(fnfe);
        } catch (IOException ioe) {
            System.out.println(ioe);
        }

        if (closePrintStream) {
            ps.close();
        }

        return dc;
    }

    public static DexContainer execute(DecompilerArguments args) {
        File file = new File(args.fileName);
        PrintStream ps = System.out;
        if (args.outputFileSupplied) {
            try {
                ps = new PrintStream(new FileOutputStream(args.outputFile));
            } catch (FileNotFoundException e) {
                e.printStackTrace();
                System.exit(1);
            }
        }

        return execute(file, ps, args.outputFileSupplied);
    }

}
