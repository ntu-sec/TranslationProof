package anu.rsise.certParser;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import anu.rsise.GeneralUtil;

public class DexCertReader {
    public DexCertReader() {
    }

    public CertContainer execute(InputStream isr) {
        CertContainer cc = new CertContainer();
        try {
            LevelPool lvl_pool = LevelPool.read(isr);
            cc.set_lvl_pool(lvl_pool);

            int default_level_id = isr.read();
            cc.set_default_level_id(default_level_id);

            LevelRelationship lvl_rel = LevelRelationship.read(isr, lvl_pool.get_length());
            cc.set_lvl_rel(lvl_rel);

            FieldPolicy ft = FieldPolicy.read(isr, lvl_pool);
            cc.set_field_policy(ft);

            short method_length = GeneralUtil.readShortFromInputStream(isr, GeneralUtil.Direction.Left_to_right);
            for (int i = 0; i < method_length; i++) {
                MethodCert method = MethodCert.read(isr, lvl_pool);
                StringBuilder sb = new StringBuilder();
                cc.add_method(method.class_name(), method.name(), method.desc(), method);
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
        return cc;
    }

    public CertContainer execute(String file) {
        CertContainer cc = null;
        try {
            FileInputStream isr = new FileInputStream(file);
            cc = execute(isr);

        } catch (FileNotFoundException fnfe) {
            fnfe.printStackTrace();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
        return cc;
    }
}
