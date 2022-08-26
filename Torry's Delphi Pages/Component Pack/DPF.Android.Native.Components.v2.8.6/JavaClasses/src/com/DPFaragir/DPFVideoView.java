// ------------------------------------------------------------------------------
// com.DPFaragir.DPFVideoView Java Sub Class
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
package com.DPFaragir;

import android.app.Activity;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.widget.VideoView;
import android.widget.MediaController;
import android.widget.TableLayout;
import android.media.MediaPlayer;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.graphics.Color;
import android.text.method.ScrollingMovementMethod;
import android.net.Uri;
import android.widget.Toast;
import android.media.MediaPlayer.OnErrorListener;
import android.media.MediaPlayer.OnPreparedListener;
import android.app.Dialog;
import android.app.Fragment;
import android.app.ListFragment;
import android.media.AudioManager;
import android.view.SurfaceView;
import android.view.View;
import android.view.SurfaceHolder;
import android.os.Handler.Callback;
import android.media.MediaPlayer.OnCompletionListener;
import android.view.View.OnClickListener;
import android.media.MediaPlayer.OnSeekCompleteListener;
import java.io.IOException;

public class DPFVideoView extends SurfaceView
implements /*OnPreparedListener, OnCompletionListener, */SurfaceHolder.Callback {



  private Context mContext ;

  private boolean active = true;
  private int elapsed = 0;
  private static int SPLASH_TIME_OUT = 30000;
  private static int SPLASH_INTERVAL = 100;
  private VideoView mThis;


  private SurfaceView surfaceViewFrame;
  private MediaPlayer player;
  private SurfaceHolder holder;

    //--------------------------------------------------------------------------
    public DPFVideoView(Context context) {
        super(context);

        mContext = context ;


        /*
        Dialog dialog = new Dialog(context);
        dialog.setContentView(this);
        dialog.setCancelable(true);
        */

        holder = getHolder();
        if (holder == null)
          Toast.makeText(mContext, " holder is null", Toast.LENGTH_LONG).show();
        else
          Toast.makeText(mContext, " holder Ok.", Toast.LENGTH_LONG).show();

        holder.addCallback(this);
        //holder.setFixedSize(getWindow().getWindowManager().getDefaultDisplay().getWidth());
        holder.setFixedSize(800,600);
        //setZOrderMediaOverlay(true);
        //setVisibility(View.VISIBLE);
        //holder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);


            player = new MediaPlayer();
                    try
                    {
            player.setDisplay(holder);
            }
                    catch (IllegalArgumentException e)
                    {
                        e.printStackTrace();
                        Toast.makeText(mContext, " Error: " + e.toString(), Toast.LENGTH_LONG).show();
                    }


            //player.setOnPreparedListener(this);
            //player.setOnCompletionListener(this);

            //player.setOnSeekCompleteListener(this);
            player.setScreenOnWhilePlaying(true);


    }

    public void playVideo()
        {

        Dialog dialog = new Dialog(mContext);
        dialog.setContentView(this);
        dialog.setCancelable(true);
            new Thread(new Runnable()
            {
                public void run()
                {
                    try
                    {

                        player.setDataSource("http://download.wavetlan.com/SVV/Media/HTTP/MP4/ConvertedFiles/Media-Convert/Unsupported/test7.mp4");
                        //player.prepare();
                        //player.start();
                        player.prepareAsync();
                    }
                    catch (IllegalArgumentException e)
                    {
                        e.printStackTrace();
                    }
                    catch (IllegalStateException e)
                    {
                        e.printStackTrace();
                    }
                    catch (IOException e)
                    {
                        e.printStackTrace();
                    }
                }
            }).start();
        }

public void draw(Canvas canvas)
{
  super.draw(canvas);
}


/*
@Override
public void onPrepared(MediaPlayer mp)
{
   Toast.makeText(mContext, " onPrepared ", Toast.LENGTH_LONG).show();
            if (!player.isPlaying())
            {
                //b = true;
                player.start();
            }

}
*/

@Override
public void surfaceChanged(SurfaceHolder holder, int format, int width, int height)
{
   Toast.makeText(mContext, " surfaceChanged", Toast.LENGTH_LONG).show();
}


@Override
public void surfaceDestroyed(SurfaceHolder surfaceholder)
{
   Toast.makeText(mContext, " surfaceDestroyed", Toast.LENGTH_LONG).show();
}


@Override
public void surfaceCreated(SurfaceHolder holder)
{
   Toast.makeText(mContext, " surfaceCreated ", Toast.LENGTH_LONG).show();
  //playVideo();
  //player.setDisplay(holder);

}

public void onCompletion(MediaPlayer arg0)
{
   Toast.makeText(mContext, " onCompletion ", Toast.LENGTH_LONG).show();

}

}

class RefresherThread extends Thread {
    static SurfaceHolder threadSurfaceHolder;
    static DPFVideoView threadView;
    static boolean running;

    public void run (){
        while(running){
            //your amazing draw/logic cycle goes here
        }
    }
}

