package se.mattec.id2212.hw5;

import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;
import android.view.KeyEvent;
import android.view.View;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import butterknife.ButterKnife;
import butterknife.InjectView;
import butterknife.OnClick;

public class GameActivity
        extends ActionBarActivity
{
    private String ip;
    private int port;

    private Socket socket;
    private PrintWriter writer;

    private ThreadPoolExecutor executor;

    @InjectView(R.id.score_textview)
    TextView scoreTextView;

    @InjectView(R.id.guesses_left_textview)
    TextView guessesLeftTextView;

    @InjectView(R.id.message_textview)
    TextView messageTextView;

    @InjectView(R.id.current_word_textview)
    TextView currentWordTextView;

    @InjectView(R.id.guess_edittext)
    EditText guessEditText;

    @InjectView(R.id.failed_layout)
    LinearLayout failedView;



    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_game);

        ButterKnife.inject(this);

        ip = getIntent().getStringExtra("IP");
        port = getIntent().getIntExtra("PORT", 4444);

        executor = new ThreadPoolExecutor(10, 20, 10000, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<Runnable>());

        new MessageReceiverAsyncTask().executeOnExecutor(executor);

        guessEditText.setOnKeyListener(new View.OnKeyListener()
        {
            @Override
            public boolean onKey(View v,
                                 int keyCode,
                                 KeyEvent event)
            {
                if (keyCode == KeyEvent.KEYCODE_ENTER)
                {
                    sendGuess();
                }
                return true;
            }
        });
    }



    @Override
    protected void onDestroy()
    {
        super.onDestroy();

        if (socket != null)
        {
            try
            {
                socket.close();
            }
            catch (IOException e)
            {
                e.printStackTrace();

            }
        }
    }



    @OnClick(R.id.send_button)
    public void sendGuess()
    {
        String guess = guessEditText.getText().toString();

        if (guess != null && guess.isEmpty() == false)
        {
            new MessageSenderAsyncTask(guess).executeOnExecutor(executor);
            guessEditText.setText("");
        }
    }



    @OnClick(R.id.new_game_button)
    public void startNewGame()
    {
        new MessageSenderAsyncTask("start game").executeOnExecutor(executor);
    }



    @OnClick(R.id.failed_button)
    public void clickBackToLobby()
    {
        Intent intent = new Intent(this, LobbyActivity.class);
        startActivity(intent);

        finish();
    }



    class MessageSenderAsyncTask
            extends AsyncTask<Void, Void, Void>
    {
        private String message;



        public MessageSenderAsyncTask(String message)
        {
            this.message = message;
        }



        @Override
        protected Void doInBackground(Void... params)
        {
            try
            {
                if (socket == null)
                {
                    socket = new Socket(ip, port);
                }

                if (writer == null)
                {
                    writer = new PrintWriter(socket.getOutputStream());
                }

                writer.println(message);
                writer.flush();
            }
            catch (IOException e)
            {
                e.printStackTrace();

                runOnUiThread(new Runnable()
                {
                    @Override
                    public void run()
                    {
                        failedView.setVisibility(View.VISIBLE);
                    }
                });
            }
            return null;
        }
    }


    class MessageReceiverAsyncTask
            extends AsyncTask<Void, Void, Void>
    {

        @Override
        protected Void doInBackground(Void... params)
        {
            try
            {
                if (socket == null)
                {
                    socket = new Socket(ip, port);
                }

                BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));

                String str;

                while ((str = reader.readLine()) != null)
                {
                    final String[] message = str.split(",");

                    runOnUiThread(new Runnable()
                    {
                        @Override
                        public void run()
                        {
                            currentWordTextView.setText(message[0]);
                            messageTextView.setText(message[1]);
                            guessesLeftTextView.setText("Guesses left: " + message[2]);
                            scoreTextView.setText("Score: " + message[3]);
                        }
                    });
                }
            }
            catch (IOException e)
            {
                e.printStackTrace();

                runOnUiThread(new Runnable()
                {
                    @Override
                    public void run()
                    {
                        failedView.setVisibility(View.VISIBLE);
                    }
                });
            }
            return null;
        }
    }
}
