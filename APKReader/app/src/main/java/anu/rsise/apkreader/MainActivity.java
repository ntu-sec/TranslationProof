package anu.rsise.apkreader;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import anu.rsise.certParser.CertContainer;
import anu.rsise.certParser.DexCertReader;
import anu.rsise.dexParser.container.DexContainer;
import anu.rsise.dexParser.reader.DexFileReader;
import anu.rsise.typeChecker.TypeChecker;

public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        /*setContentView(R);*/

        Toolbar toolbar = (Toolbar) findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);

    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    public void button_click(View v) {
        TextView result_text = (TextView) findViewById(R.id.txtResult);
        TextView certTime_text = (TextView) findViewById(R.id.lblCertTime);
        TextView fileTime_text = (TextView) findViewById(R.id.lblDEXTime);
        TextView tcTime_text = (TextView) findViewById(R.id.lblTypeCheckTime);
        TextView comment_text = (TextView) findViewById(R.id.lblComment);

        EditText input_name = (EditText) findViewById(R.id.txtName);
        String app_name = input_name.getText().toString(); /* anu.rsise.simpleapp */

        InputStream contentStream = null;
        InputStream certStream = null;

        final PackageManager pm = getPackageManager();
        //get a list of installed apps.
        try {
            ApplicationInfo ai = pm.getApplicationInfo(app_name.trim(), PackageManager.GET_META_DATA);

            ZipFile zipFile = new ZipFile(ai.sourceDir);
            Enumeration<? extends ZipEntry> entries = zipFile.entries();

            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                if (entry.getName().equals("classes.dex")) // hard coded for now
                {
                    contentStream = zipFile.getInputStream(entry);
                    Toast.makeText(this, "Found the DEX file " + entry.getName(), Toast.LENGTH_SHORT).show();
                    if (certStream != null) break;
                }

                if (entry.getName().equals("assets/Certificate.cert")) {
                    certStream = zipFile.getInputStream(entry);
                    Toast.makeText(this, "Found the certificate " + entry.getName(), Toast.LENGTH_SHORT).show();
                    if (contentStream != null) break;
                }
            }

        } catch (PackageManager.NameNotFoundException nnfe) {
            nnfe.printStackTrace();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }

        if (contentStream == null && certStream == null) {
            Toast.makeText(this, "DEX file and certificate not found", Toast.LENGTH_SHORT).show();
            return;
        } else if (contentStream == null) {
            Toast.makeText(this, "DEX file not found", Toast.LENGTH_SHORT).show();
            return;
        } else if (certStream == null) {
            Toast.makeText(this, "Certificate not found", Toast.LENGTH_SHORT).show();
            return;
        }

        Toast.makeText(this, "Creating temporary file", Toast.LENGTH_LONG).show();
        File tempFile = null;
        FileOutputStream fos;
        try {
            tempFile = File.createTempFile("temp", null, getCacheDir());
            fos = new FileOutputStream(tempFile);
            int tempByte = 0;
            while (tempByte != -1) {
                tempByte = contentStream.read();
                if (tempByte != -1) {
                    // output to temporary file
                    fos.write(tempByte);
                }
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }

        if (tempFile == null) {
            Toast.makeText(this, "Aborting type checking: tempFile is null", Toast.LENGTH_SHORT).show();
            return;
        }

        long t0 = System.currentTimeMillis();

        DexCertReader dcr = new DexCertReader();
        CertContainer cert = dcr.execute(certStream);

        long t1 = System.currentTimeMillis();
        certTime_text.setText(Long.toString(t1 - t0));

        DexContainer dexFile = DexFileReader.execute(tempFile, System.out, false);

        long t2 = System.currentTimeMillis();
        fileTime_text.setText(Long.toString(t2 - t1));

        TypeChecker tc = new TypeChecker(cert, dexFile);
        TypeChecker.Result res = tc.typeCheck();
        //res = tc.typeCheck();
        String result = res.success() ? "True" : "False";

        long t3 = System.currentTimeMillis();
        tcTime_text.setText(Long.toString(t3 - t2));

        result_text.setText(result);
        comment_text.setText(res.comment());
    }
}
