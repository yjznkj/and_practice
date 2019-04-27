package com.example.app_0001_leddemo;

import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.Toast;
import com.example.hardlibrary.*;
public class MainActivity extends AppCompatActivity {
    private boolean ledon=false;
    private Button button=null;
    private CheckBox checkBoxLed1=null;
    private CheckBox checkBoxLed2=null;
    private CheckBox checkBoxLed3=null;
    private CheckBox checkBoxLed4=null;


    class MyButtonListener implements View.OnClickListener{
        @Override
        public void onClick(View view) {

           // HardControl HardControl=new HardControl();

            ledon=!ledon;

            if (ledon){
                button.setText(getString(R.string.LED_OFF));
                checkBoxLed1.setChecked(true);
                checkBoxLed2.setChecked(true);
                checkBoxLed3.setChecked(true);
                checkBoxLed4.setChecked(true);

                for(int i=0;i<4;i++)
                {
                    HardControl.ledCtrl(i,1);
                }
            }
            else {
                button.setText(getString(R.string.LED_ON));
                checkBoxLed1.setChecked(false);
                checkBoxLed2.setChecked(false);
                checkBoxLed3.setChecked(false);
                checkBoxLed4.setChecked(false);

                for(int i=0;i<4;i++)
                {
                    HardControl.ledCtrl(i,0);
                }
            }
        }
    }
    public void onCheckboxClicked(View view){
        boolean checked=((CheckBox)view).isChecked();
        switch (view.getId()){
            case R.id.led1:
                if(checked){
                    Toast.makeText(getApplicationContext(),"led1 on",Toast.LENGTH_SHORT).show();
                    HardControl.ledCtrl(0,1);
                }

                else{
                    Toast.makeText(getApplicationContext(),"led1 off",Toast.LENGTH_SHORT).show();
                    HardControl.ledCtrl(0,0);
                }
                break;
            case R.id.led2:
                if(checked){
                    Toast.makeText(getApplicationContext(),"led2 on",Toast.LENGTH_SHORT).show();
                    HardControl.ledCtrl(1,1);
                }

                else{
                    Toast.makeText(getApplicationContext(),"led2 off",Toast.LENGTH_SHORT).show();
                    HardControl.ledCtrl(1,0);
                }
                break;
            case R.id.led3:
                if(checked){
                    Toast.makeText(getApplicationContext(),"led3 on",Toast.LENGTH_SHORT).show();
                    HardControl.ledCtrl(2,1);
                }

                else{
                    Toast.makeText(getApplicationContext(),"led3 off",Toast.LENGTH_SHORT).show();
                    HardControl.ledCtrl(2,0);
                }
                break;
            case R.id.led4:
                if(checked){
                    Toast.makeText(getApplicationContext(),"led4 on",Toast.LENGTH_SHORT).show();
                    HardControl.ledCtrl(3,1);
                }

                else{
                    Toast.makeText(getApplicationContext(),"led4 off",Toast.LENGTH_SHORT).show();
                    HardControl.ledCtrl(3,0);
                }
                break;

        }

    }
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        button=findViewById(R.id.BUTTON);

        int ledOpen = HardControl.ledOpen();

        checkBoxLed1=findViewById(R.id.led1);
        checkBoxLed2=findViewById(R.id.led2);
        checkBoxLed3=findViewById(R.id.led3);
        checkBoxLed4=findViewById(R.id.led4);

        button.setOnClickListener(new MyButtonListener());

 /*       button.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                ledon=!ledon;
                if (ledon){
                    button.setText("ALL OFF");
                }
                else
                    button.setText("ALL ON");

            }
        });
       */
    }
}
