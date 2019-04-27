package com.example.app_0001_leddemo;

import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.widget.Button;
import android.view.View;
import android.widget.CheckBox;
import android.widget.Toast;


public class MainActivity extends AppCompatActivity {
    private Button button = null;
    private boolean ledon=false;
    private CheckBox checkBoxled1=null;
    private CheckBox checkBoxled2=null;
    private CheckBox checkBoxled3=null;
    private CheckBox checkBoxled4=null;

    class MyButtonListener implements View.OnClickListener{
        @Override
        public void onClick(View v) {
            ledon =!ledon;
            if(ledon)
            {
                button.setText("LED_OFF");
                checkBoxled1.setChecked(true);
                checkBoxled2.setChecked(true);
                checkBoxled3.setChecked(true);
                checkBoxled4.setChecked(true);

            }
            else
            {
                button.setText("LED_ON");

                checkBoxled1.setChecked(false);
                checkBoxled2.setChecked(false);
                checkBoxled3.setChecked(false);
                checkBoxled4.setChecked(false);


            }
        }
    }

    public void onCheckboxClicked(View view) {
        // Is the view now checked?
        boolean checked = ((CheckBox) view).isChecked();

        // Check which checkbox was clicked
        switch(view.getId()) {
            case R.id.led1:
                if (checked)
                {
                    Toast.makeText(getApplicationContext(), "led1 on",Toast.LENGTH_SHORT).show();
                     // Put some meat on the sandwich
                }

            else
                {
                    Toast.makeText(getApplicationContext(), "led1 off",Toast.LENGTH_SHORT).show();
                    // Remove the meat
                }

                break;
            case R.id.led2:
                if (checked)
                {
                    Toast.makeText(getApplicationContext(), "led2 on",Toast.LENGTH_SHORT).show();
                    // Put some meat on the sandwich
                }

                else
                {
                    Toast.makeText(getApplicationContext(), "led2 off",Toast.LENGTH_SHORT).show();
                    // Remove the meat
                }

                break;
            case R.id.led3:
                if (checked)
                {
                    Toast.makeText(getApplicationContext(), "led3 on",Toast.LENGTH_SHORT).show();
                    // Put some meat on the sandwich
                }

                else
                {
                    Toast.makeText(getApplicationContext(), "led3 off",Toast.LENGTH_SHORT).show();
                    // Remove the meat
                }

                break;
            case R.id.led4:
                if (checked)
                {
                    Toast.makeText(getApplicationContext(), "led4 on",Toast.LENGTH_SHORT).show();
                    // Put some meat on the sandwich
                }

                else
                {
                    Toast.makeText(getApplicationContext(), "led4 off",Toast.LENGTH_SHORT).show();
                    // Remove the meat
                }

                break;

            // TODO: Veggie sandwich
        }
    }


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        button=(Button) findViewById(R.id.button);

        checkBoxled1=(CheckBox) findViewById(R.id.led1);
        checkBoxled2=(CheckBox) findViewById(R.id.led2);
        checkBoxled3=(CheckBox) findViewById(R.id.led3);
        checkBoxled4=(CheckBox) findViewById(R.id.led4);



        button.setOnClickListener(new MyButtonListener());

        /*button.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                // Perform action on click
                ledon =!ledon;
                if(ledon)
                {
                    button.setText("LED_OFF");
            }
                else
                    {
                        button.setText("LED_ON");
                    }
            }
        });*/
    }
}
