package anu.rsise.simpleapp;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;

public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
    }

    public void btnCalculate_click(View v) {
        EditText opr1 = (EditText) findViewById(R.id.txtOpr1);
        EditText opr2 = (EditText) findViewById(R.id.txtOpr2);
        TextView result = (TextView) findViewById(R.id.lblResult);

        String resValue = opr1.getText().toString() + opr2.getText().toString();
        result.setText(resValue);
    }
}
