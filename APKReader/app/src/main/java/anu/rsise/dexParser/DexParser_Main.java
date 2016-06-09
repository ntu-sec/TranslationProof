package anu.rsise.dexParser;

import anu.rsise.dexParser.container.DexContainer;
import anu.rsise.dexParser.reader.DexFileReader;

public class DexParser_Main {

    /**
     * @param args
     */
    public static DexContainer main(String[] args) {
        DecompilerArguments arguments = new DecompilerArguments();

        for (int i = 0; i < args.length; i++) {
            if (args[i].equals("-d")) {
                arguments.dumpBytes = true;
            }
            if (args[i].endsWith(".dex")) {
                arguments.fileName = args[i];
                arguments.fileSupplied = true;
                if (i < args.length - 1) {
                    arguments.outputFile = args[i + 1];
                    arguments.outputFileSupplied = true;
                }
                break;
            }
        }

        if (!arguments.fileSupplied) {
            System.out.println("Require an input of dex file");
            return null;
        }

        return DexFileReader.execute(arguments);
    }

}
