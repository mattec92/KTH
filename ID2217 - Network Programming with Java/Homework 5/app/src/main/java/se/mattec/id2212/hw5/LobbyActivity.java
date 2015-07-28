package se.mattec.id2212.hw5;

import android.content.Intent;
import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;
import android.widget.EditText;

import butterknife.ButterKnife;
import butterknife.InjectView;
import butterknife.OnClick;


public class LobbyActivity
        extends ActionBarActivity
{
    private static final String DEFAULT_IP = "192.168.1.101";
    private static final int DEFAULT_PORT = 4444;

    @InjectView(R.id.port_edittext)
    EditText portEditText;

    @InjectView(R.id.ip_edittext)
    EditText ipEditText;



    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_lobby);

        ButterKnife.inject(this);

        ipEditText.setHint("Ip address (" + DEFAULT_IP + ")");
        portEditText.setHint("Port (" + DEFAULT_PORT + ")");
    }



    @OnClick(R.id.start_button)
    public void clickStartButton()
    {
        String ip = ipEditText.getText().toString();
        String port = portEditText.getText().toString();
        int portAsInteger;

        if (ip == null || ip.isEmpty())
        {
            ip = DEFAULT_IP;
        }

        if (port == null || port.isEmpty())
        {
            portAsInteger = DEFAULT_PORT;
        }
        else
        {
            try
            {
                portAsInteger = Integer.parseInt(port);
            }
            catch (NumberFormatException e)
            {
                portAsInteger = DEFAULT_PORT;
            }
        }

        Intent intent = new Intent(this, GameActivity.class);

        intent.putExtra("IP", ip);
        intent.putExtra("PORT", portAsInteger);

        startActivity(intent);
    }

}
